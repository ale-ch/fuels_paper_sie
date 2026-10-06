import sys
import pandas as pd
from pathlib import Path


def run_batching_pipeline(
    master_file,
    prices_dir,
    output_dir,
    num_chunks=50,
    batch_size=30
):
    prices_dir = Path(prices_dir)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # 1. Load master data
    print("Loading master pumps list...")

    master_df = pd.read_csv(
        master_file,
        sep='|',
        dtype={'id_pump': str}
    )

    # Remove duplicate pump IDs if necessary
    master_df = master_df.drop_duplicates(subset='id_pump')

    # Create chunk map
    unique_ids = master_df['id_pump'].unique()

    chunk_map = {
        pid: i % num_chunks
        for i, pid in enumerate(unique_ids)
    }

    print(
        f"Loaded {len(unique_ids)} unique pumps "
        f"and mapped them to {num_chunks} chunks."
    )

    # 2. Get daily price files
    all_files = sorted(
        f for f in prices_dir.glob('*.csv')
        if not f.name.startswith('._')
    )

    total_files = len(all_files)

    print(f"Found {total_files} daily price files to process.")

    # 3. Process batches
    for i in range(0, total_files, batch_size):

        batch_files = all_files[i:i + batch_size]

        print(
            f"Processing batch {i // batch_size + 1} / "
            f"{-( -total_files // batch_size )} "
            f"(Files {i + 1} to "
            f"{min(i + batch_size, total_files)})..."
        )

        batch_df_list = []

        for file_path in batch_files:

            df = pd.read_csv(
                file_path,
                sep='|',
                dtype={'id_pump': str}
            )

            batch_df_list.append(df)

        if not batch_df_list:
            continue

        batch_df = pd.concat(
            batch_df_list,
            ignore_index=True
        )

        # 4. Join master data to price data
        #
        # Every price row gets the corresponding master information
        # based on id_pump.
        batch_df = batch_df.merge(
            master_df,
            on='id_pump',
            how='left',
            suffixes=('', '_master')
        )

        # 5. Assign rows to chunks
        batch_df['chunk_id'] = batch_df['id_pump'].map(chunk_map)

        # IDs not present in master go to overflow chunk
        batch_df['chunk_id'] = (
            batch_df['chunk_id']
            .fillna(num_chunks)
            .astype(int)
        )

        # 6. Write chunks
        for chunk_id, chunk_df in batch_df.groupby('chunk_id'):

            chunk_file = output_dir / f"chunk_{chunk_id}.csv"

            write_header = not chunk_file.exists()

            chunk_df = chunk_df.drop(columns=['chunk_id'])

            chunk_df.to_csv(
                chunk_file,
                mode='a',
                sep='|',
                index=False,
                header=write_header,
                na_rep='NA'
            )

    print("Batching pipeline completed.")


if __name__ == "__main__":

    if len(sys.argv) < 4:
        print(
            "Usage: python batching_pipeline.py "
            "<master_file.csv> "
            "<prices_input_dir> "
            "<chunks_output_dir> "
            "[num_chunks] "
            "[batch_size]"
        )
        sys.exit(1)

    master_file = sys.argv[1]
    prices_dir = sys.argv[2]
    output_dir = sys.argv[3]

    num_chunks = int(sys.argv[4]) if len(sys.argv) > 4 else 50
    batch_size = int(sys.argv[5]) if len(sys.argv) > 5 else 30

    run_batching_pipeline(
        master_file,
        prices_dir,
        output_dir,
        num_chunks,
        batch_size
    )

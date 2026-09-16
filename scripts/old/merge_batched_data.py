import sys
import pandas as pd
from pathlib import Path

def run_batching_pipeline(master_file, prices_dir, output_dir, num_chunks=50, batch_size=30):
    prices_dir = Path(prices_dir)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # 1. Create the ID to Chunk Map
    print("Loading master pumps list and creating chunk map...")
    master_df = pd.read_csv(master_file, sep='|', usecols=['id_pump'], dtype=str)
    unique_ids = master_df['id_pump'].unique()
    
    # Map each unique ID to a chunk number (0 to num_chunks - 1)
    chunk_map = {pid: i % num_chunks for i, pid in enumerate(unique_ids)}
    print(f"Mapped {len(unique_ids)} unique pumps to {num_chunks} chunks.")
    
    # 2. Get list of daily price files
    all_files = sorted([f for f in prices_dir.glob('*.csv') if not f.name.startswith('._')])
    total_files = len(all_files)
    print(f"Found {total_files} daily price files to process.")
    
    # 3. Process in batches
    for i in range(0, total_files, batch_size):
        batch_files = all_files[i:i + batch_size]
        print(f"Processing batch {i // batch_size + 1} / {-( -total_files // batch_size )} (Files {i+1} to {min(i+batch_size, total_files)})...")
        
        batch_df_list = []
        for file_path in batch_files:
            # Read standardized daily prices
            df = pd.read_csv(file_path, sep='|', dtype={'id_pump': str})
            batch_df_list.append(df)
            
        if not batch_df_list:
            continue
            
        # Concatenate the batch in memory
        batch_df = pd.concat(batch_df_list, ignore_index=True)
        
        # Tag and route rows to chunks
        batch_df['chunk_id'] = batch_df['id_pump'].map(chunk_map)
        
        # Handle new IDs that might not be in the master list (assign to a default overflow chunk)
        batch_df['chunk_id'] = batch_df['chunk_id'].fillna(num_chunks).astype(int)
        
        # 4. Scatter to persistent chunk files
        for chunk_id, chunk_df in batch_df.groupby('chunk_id'):
            chunk_file = output_dir / f"chunk_{chunk_id}.csv"
            
            write_header = not chunk_file.exists()
            
            chunk_df.drop(columns=['chunk_id']).to_csv(
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
        print("Usage: python batching_pipeline.py <master_file.csv> <prices_input_dir> <chunks_output_dir> [num_chunks] [batch_size]")
        sys.exit(1)
        
    master_file = sys.argv[1]
    prices_dir = sys.argv[2]
    output_dir = sys.argv[3]
    num_chunks = int(sys.argv[4]) if len(sys.argv) > 4 else 50
    batch_size = int(sys.argv[5]) if len(sys.argv) > 5 else 30
    
    run_batching_pipeline(master_file, prices_dir, output_dir, num_chunks, batch_size)

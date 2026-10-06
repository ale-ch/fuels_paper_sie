import sys
import pandas as pd
from pathlib import Path

def create_master_pumps_list(processed_dir, output_file):
    processed_dir = Path(processed_dir)
    print(f"Scanning directory: {processed_dir}")

    df_list = []
    # Sort files to ensure chronological order if filenames contain dates
    files = sorted([f for f in processed_dir.glob('*.csv') if not f.name.startswith('._')])
    
    total_files = len(files)
    if not files:
        print("No processed files found.")
        return
        
    print(f"Found {total_files} valid CSV files.")

    for i, file_path in enumerate(files, 1):
        print(f"Reading file {i}/{total_files}: {file_path.name}")
        # Read as string to prevent issues with coordinate precision or leading zeros
        df = pd.read_csv(file_path, sep='|', dtype=str)
        df_list.append(df)

    print(f"Concatenating {len(df_list)} files in memory...")
    master_df = pd.concat(df_list, ignore_index=True)
    print("Concatenation complete.")

    initial_count = len(master_df)
    print(f"Total rows before deduplication: {initial_count}")
    
    print("Deduplicating based on 'id_pump' (keeping last occurrence)...")
    # Deduplicate keeping the last occurrence (most recent if files are chronologically sorted)
    master_df.drop_duplicates(subset=['id_pump'], keep='last', inplace=True)

    final_count = len(master_df)
    print(f"Deduplication complete. Removed {initial_count - final_count} duplicate rows.")
    print(f"Unique pump IDs (final rows): {final_count}")

    print(f"Saving master list to: {output_file}...")
    master_df.to_csv(output_file, sep='|', index=False, na_rep='NA')
    
    print("Process completed successfully.")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python merge_stations.py <processed_dir> <output_master_file>")
        sys.exit(1)

    processed_dir = sys.argv[1]
    output_file = sys.argv[2]

    create_master_pumps_list(processed_dir, output_file)

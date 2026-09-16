import sys
import pandas as pd
from pathlib import Path

def filter_chunk_columns(input_dir, output_dir):
    input_dir = Path(input_dir)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    target_columns = [
        'id_pump', 'price_gasoline', 'price_gasoline_self', 
        'price_diesel', 'price_diesel_self', 'date', 'brand', 
        'station_type', 'city', 'province', 'latitude', 'longitude'
    ]
    
    chunk_files = sorted(input_dir.glob('chunk_*.csv'))
    
    if not chunk_files:
        print("No chunk files found.")
        return
        
    for chunk_file in chunk_files:
        print(f"Filtering {chunk_file.name}...")
        df = pd.read_csv(chunk_file, sep='|', dtype=str)
        
        # Keep only the allowed columns that exist in the current chunk
        cols_to_keep = [col for col in target_columns if col in df.columns]
        df = df[cols_to_keep]
        
        output_path = output_dir / chunk_file.name
        df.to_csv(output_path, sep='|', index=False, na_rep='NA')
        print(f"Exported {chunk_file.name} with {len(cols_to_keep)} columns.")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python filter_chunks.py <input_chunks_dir> <output_chunks_dir>")
        sys.exit(1)
        
    filter_chunk_columns(sys.argv[1], sys.argv[2])

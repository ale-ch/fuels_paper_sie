# Required installations: pip install pandas sqlalchemy psycopg2-binary (for PostgreSQL) or pymysql (for MySQL)
import sys
import pandas as pd
from pathlib import Path
import rdata

def upload_chunks_to_rds(chunks_dir, output_dir):
    chunks_dir = Path(chunks_dir)
    
    chunk_files = sorted(chunks_dir.glob('chunk_*.csv'))
    
    if not chunk_files:
        print("No chunk files found.")
        return
        
    for chunk_file in chunk_files:
        print(f"Uploading {chunk_file.name}...")
        df = pd.read_csv(chunk_file, sep='|', dtype=str)
        
        # Write data to the database
        rdata.write_rds(f"{chunk_file.name}.RDS", df)
        print(f"Finished uploading {chunk_file.name}.")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python upload_to_rds.py <chunks_dir> <output_dir>")
        sys.exit(1)
        
    upload_chunks_to_rds(sys.argv[1], sys.argv[2])

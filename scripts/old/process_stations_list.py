import sys
import pandas as pd
import io
from pathlib import Path

def process_station_file(file_path, output_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.read().strip().split('\n')
    except UnicodeDecodeError:
        with open(file_path, 'r', encoding='latin-1') as f:
            lines = f.read().strip().split('\n')
        
    if not lines or len(lines) < 2:
        return 0
        
    # Dynamically find separator from the header line
    sep = '|' if '|' in lines[1] else ','
    
    # Read CSV skipping the first row ("Estrazione del ...")
    df = pd.read_csv(io.StringIO('\n'.join(lines[1:])), sep=sep)
    
    col_map = {
        'idImpianto': 'id_pump',
        'Gestore': 'manager',
        'Bandiera': 'brand',
        'Tipo Impianto': 'station_type',
        'Nome Impianto': 'station_name',
        'Indirizzo': 'address',
        'Comune': 'city',
        'Provincia': 'province',
        'Latitudine': 'latitude',
        'Longitudine': 'longitude'
    }
    
    # Rename columns
    df.rename(columns=col_map, inplace=True)
    
    # Write to output file
    df.to_csv(output_path, sep='|', index=False, na_rep='NA')
    
    return len(df)

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python script.py <input_dir> <output_dir>")
        sys.exit(1)
        
    input_dir = Path(sys.argv[1])
    output_dir = Path(sys.argv[2])
    
    output_dir.mkdir(parents=True, exist_ok=True)
    
    for file_path in input_dir.glob('*.csv'):
        if file_path.name.startswith('._'):
            continue
            
        output_path = output_dir / file_path.name
        try:
            row_count = process_station_file(file_path, output_path)
            print(f"Processed: {file_path.name} | Rows Exported: {row_count}")
        except Exception as e:
            print(f"Failed {file_path.name}: {e}")

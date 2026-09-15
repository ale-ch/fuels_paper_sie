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
        
    sep = '|' if '|' in lines[1] else ','
    
    # Read the data, skipping the first extraction date line. 
    # 'on_bad_lines="skip"' prevents crashes from rows with extra separators.
    df = pd.read_csv(io.StringIO('\n'.join(lines[1:])), sep=sep, on_bad_lines='skip')
    
    # Explicitly drop the column by name
    if 'Nome Impianto' in df.columns:
        df.drop(columns=['Nome Impianto'], inplace=True)
        
    col_map = {
        'idImpianto': 'id_pump',
        'Gestore': 'manager',
        'Bandiera': 'brand',
        'Tipo Impianto': 'station_type',
        'Indirizzo': 'address',
        'Comune': 'city',
        'Provincia': 'province',
        'Latitudine': 'latitude',
        'Longitudine': 'longitude'
    }
    
    df.rename(columns=col_map, inplace=True)
    
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

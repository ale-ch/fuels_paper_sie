import sys
import pandas as pd
import io
from pathlib import Path

def process_file(file_path, output_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        lines = f.read().strip().split('\n')
        
    if not lines or len(lines) < 2:
        return
        
    date_str = lines[0].split()[-1]
    sep = '|' if '|' in lines[1] else ','
    
    df = pd.read_csv(io.StringIO('\n'.join(lines[1:])), sep=sep)
    
    fuel_map = {
        'Benzina': 'gasoline',
        'Gasolio': 'diesel',
        'Metano': 'methane',
        'GPL': 'lpg'
    }
    df['fuel_mapped'] = df['descCarburante'].map(fuel_map)
    df['target_col'] = 'price_' + df['fuel_mapped'] + df['isSelf'].apply(lambda x: '_self' if x == 1 else '')
    
    pivot_df = df.pivot(index='idImpianto', columns='target_col', values='prezzo').reset_index()
    pivot_df.rename(columns={'idImpianto': 'id_pump'}, inplace=True)
    
    expected_cols = [
        'id_pump', 'price_gasoline', 'price_gasoline_self',
        'price_diesel', 'price_diesel_self', 'price_methane',
        'price_methane_self', 'price_lpg', 'price_lpg_self'
    ]
    pivot_df = pivot_df.reindex(columns=expected_cols)
    pivot_df['date'] = date_str
    
    pivot_df.to_csv(output_path, sep='|', index=False, na_rep='NA')

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python script.py <input_dir> <output_dir>")
        sys.exit(1)
        
    input_dir = Path(sys.argv[1])
    output_dir = Path(sys.argv[2])
    
    output_dir.mkdir(parents=True, exist_ok=True)
    
    for file_path in input_dir.glob('*.csv'):
        output_path = output_dir / file_path.name
        try:
            process_file(file_path, output_path)
            print(f"Processed: {file_path.name}")
        except Exception as e:
            print(f"Failed {file_path.name}: {e}")

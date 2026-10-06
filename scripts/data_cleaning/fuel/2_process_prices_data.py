import sys
import pandas as pd
import io
from pathlib import Path

def process_file(file_path, output_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.read().strip().split('\n')
    except UnicodeDecodeError:
        with open(file_path, 'r', encoding='latin-1') as f:
            lines = f.read().strip().split('\n')
        
    if not lines or len(lines) < 2:
        return 0, 0, 0
        
    date_str = lines[0].split()[-1]
    sep = '|' if '|' in lines[1] else ',' if ',' in lines[1] else ';'
    
    df = pd.read_csv(io.StringIO('\n'.join(lines[1:])), sep=sep)
    input_unique_ids = df['idImpianto'].nunique()
    
    # Scan the ID column to count IDs that have NO Benzina and NO Gasolio records
    has_target_fuels = df.groupby('idImpianto')['descCarburante'].apply(
        lambda x: x.isin(['Benzina', 'Gasolio']).any()
    )
    excluded_ids_count = (~has_target_fuels).sum()
    
    # Filter out categories
    df = df[df['descCarburante'].isin(['Benzina', 'Gasolio'])].copy()
    
    fuel_map = {
        'Benzina': 'gasoline',
        'Gasolio': 'diesel'
    }
    df['fuel_mapped'] = df['descCarburante'].map(fuel_map)
    df['target_col'] = 'price_' + df['fuel_mapped'] + df['isSelf'].apply(lambda x: '_self' if str(x).strip() == '1' else '')
    
    pivot_df = df.pivot(index='idImpianto', columns='target_col', values='prezzo').reset_index()
    pivot_df.rename(columns={'idImpianto': 'id_pump'}, inplace=True)
    
    expected_cols = [
        'id_pump', 'price_gasoline', 'price_gasoline_self',
        'price_diesel', 'price_diesel_self'
    ]
    pivot_df = pivot_df.reindex(columns=expected_cols)
    pivot_df['date'] = date_str
    
    output_unique_ids = len(pivot_df)
    
    pivot_df.to_csv(output_path, sep='|', index=False, na_rep='NA')
    
    return input_unique_ids, output_unique_ids, excluded_ids_count

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
            in_count, out_count, excl_count = process_file(file_path, output_path)
            print(f"Processed: {file_path.name} | IDs Input: {in_count} | IDs Output: {out_count} | Excluded (Empty Benzina/Gasolio): {excl_count}")
        except Exception as e:
            print(f"Failed {file_path.name}: {e}")

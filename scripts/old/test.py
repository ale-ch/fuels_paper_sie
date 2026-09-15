import pandas as pd
import sys
import io

def process_gas_station_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        lines = f.read().strip().split('\n')
        
    if not lines:
        return ""
        
    date_str = lines[0].split()[-1]
    sep = '|' if '|' in lines[1] else ','
    
    # Load data skipping the first row
    df = pd.read_csv(io.StringIO('\n'.join(lines[1:])), sep=sep)
    
    fuel_map = {
        'Benzina': 'gasoline',
        'Gasolio': 'diesel',
        'Metano': 'methane',
        'GPL': 'lpg'
    }
    df['fuel_mapped'] = df['descCarburante'].map(fuel_map)
    
    # Generate target column names based on isSelf
    df['target_col'] = 'price_' + df['fuel_mapped'] + df['isSelf'].apply(lambda x: '_self' if x == 1 else '')
    
    # Pivot the data
    pivot_df = df.pivot(index='idImpianto', columns='target_col', values='prezzo').reset_index()
    pivot_df.rename(columns={'idImpianto': 'id_pump'}, inplace=True)
    
    # Enforce exact output columns
    expected_cols = [
        'id_pump', 'price_gasoline', 'price_gasoline_self',
        'price_diesel', 'price_diesel_self', 'price_methane',
        'price_methane_self', 'price_lpg', 'price_lpg_self'
    ]
    pivot_df = pivot_df.reindex(columns=expected_cols)
    pivot_df['date'] = date_str
    
    return pivot_df.to_csv(sep='|', index=False, na_rep='NA').strip()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py <path_to_data_file>")
        sys.exit(1)
        
    file_path = sys.argv[1]
    print(process_gas_station_file(file_path))

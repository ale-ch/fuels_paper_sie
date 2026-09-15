import pandas as pd
import io

def test_pivot_logic():
    # Sample data with mixed fuel types
    raw_data = """idImpianto|descCarburante|prezzo|isSelf
1|Benzina|2.209|0
1|Benzina|1.849|1
1|Gasolio|2.539|0
1|Gasolio|2.179|1
2|Benzina|1.950|1
3|GPL|0.781|0
4|Metano|1.529|0
4|Gasolio|2.100|0"""

    # Load input data
    df = pd.read_csv(io.StringIO(raw_data), sep='|')
    input_ids = set(df['idImpianto'])
    input_unique_count = len(input_ids)
    
    # Apply filtering
    df_filtered = df[df['descCarburante'].isin(['Benzina', 'Gasolio'])].copy()
    
    # Map and format columns
    fuel_map = {'Benzina': 'gasoline', 'Gasolio': 'diesel'}
    df_filtered['fuel_mapped'] = df_filtered['descCarburante'].map(fuel_map)
    df_filtered['target_col'] = 'price_' + df_filtered['fuel_mapped'] + df_filtered['isSelf'].apply(lambda x: '_self' if str(x).strip() == '1' else '')
    
    # Pivot
    pivot_df = df_filtered.pivot(index='idImpianto', columns='target_col', values='prezzo').reset_index()
    
    # Count output IDs
    output_ids = set(pivot_df['idImpianto'])
    output_unique_count = len(output_ids)
    
    # Verify results
    print(f"Input Unique IDs:  {input_unique_count}")
    print(f"Output Unique IDs: {output_unique_count}")
    print(f"Dropped IDs (No Benzina/Gasolio): {input_ids - output_ids}\n")
    
    print("--- Input Data ---")
    print(df.to_string(index=False))
    print("\n--- Output Data ---")
    print(pivot_df.to_string(index=False))

if __name__ == "__main__":
    test_pivot_logic()

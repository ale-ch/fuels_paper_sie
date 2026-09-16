import sys
import shutil
from pathlib import Path

def organize_fuels_data(root_dir):
    root = Path(root_dir).resolve()
    
    price_dir = root / 'price_data'
    stations_dir = root / 'stations_data'
    
    price_dir.mkdir(exist_ok=True)
    stations_dir.mkdir(exist_ok=True)
    
    for file_path in root.rglob('*'):
        if not file_path.is_file():
            continue
            
        name = file_path.name
        name_lower = name.lower()
        
        if name_lower.endswith('.tar') or name_lower.endswith('.tar.gz'):
            file_path.unlink()
            print(f"Deleted: {name}")
            continue
            
        if name_lower.startswith('prezzo'):
            target_path = price_dir / name
            shutil.move(str(file_path), str(target_path))
            print(f"Moved to price_data: {name}")
            continue
            
        if name_lower.startswith('anagrafica'):
            target_path = stations_dir / name
            shutil.move(str(file_path), str(target_path))
            print(f"Moved to stations_data: {name}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python organize_data.py <root_directory>")
        sys.exit(1)
        
    organize_fuels_data(sys.argv[1])

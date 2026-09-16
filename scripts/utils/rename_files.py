from pathlib import Path

base_dir = "/Users/alessiochiodin/Downloads/Dati_carburanti"

for file_path in Path(base_dir).rglob("*"):
    if file_path.is_file():
        new_name = file_path.name.replace(" (1)", "_2").replace("-", "_")
        if new_name != file_path.name:
            file_path.rename(file_path.parent / new_name)

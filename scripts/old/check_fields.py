import sys
import csv
from pathlib import Path

def scan_file_for_errors(file_path):
    try:
        f = open(file_path, 'r', encoding='utf-8')
        lines = f.readlines()
        f.close()
    except UnicodeDecodeError:
        f = open(file_path, 'r', encoding='latin-1')
        lines = f.readlines()
        f.close()

    if len(lines) < 2:
        return

    sep = '|' if '|' in lines[1] else ','
    print(f"SEPARATOR: {sep}") 
    reader = csv.reader(lines[1:], delimiter=sep)
    
    try:
        header = next(reader)
        expected_count = len(header)
    except StopIteration:
        return

    for idx, row in enumerate(reader, start=3):
        if not row:
            continue
            
        actual_count = len(row)
        if actual_count != expected_count:
            print(f"File: {file_path.name} | Line: {idx} | Expected: {expected_count} | Found: {actual_count}")
            print(f"Raw text: {lines[idx-1].strip()}")
            print(f"Parsed fields: {row}")
            print("-" * 80)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py <input_dir>")
        sys.exit(1)
        
    input_dir = Path(sys.argv[1])
    
    for file_path in input_dir.glob('*.csv'):
        if file_path.name.startswith('._'):
            continue
        scan_file_for_errors(file_path)

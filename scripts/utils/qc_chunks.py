import sys
from pathlib import Path
import pandas as pd


def check_id_pump_uniqueness(chunks_dir):
    chunks_dir = Path(chunks_dir)

    chunk_files = sorted(
        f for f in chunks_dir.glob("chunk_*.csv")
        if not f.name.startswith("._")
    )

    if not chunk_files:
        print("ERROR: No chunk files found.")
        sys.exit(1)

    print(f"Checking {len(chunk_files)} chunk files...")

    # id_pump -> files in which it occurs
    id_to_files = {}

    for file_path in chunk_files:
        print(f"Reading {file_path.name}...")

        df = pd.read_csv(
            file_path,
            sep="|",
            usecols=["id_pump"],
            dtype={"id_pump": str}
        )

        # Only need each ID once per file
        unique_ids = df["id_pump"].dropna().unique()

        for pump_id in unique_ids:
            id_to_files.setdefault(pump_id, []).append(file_path.name)

    # Find IDs appearing in multiple files
    duplicated_ids = {
        pump_id: files
        for pump_id, files in id_to_files.items()
        if len(files) > 1
    }

    print("\n" + "=" * 60)
    print("QUALITY CONTROL RESULTS")
    print("=" * 60)

    print(f"Total chunk files: {len(chunk_files)}")
    print(f"Total unique id_pump: {len(id_to_files)}")
    print(f"IDs found in multiple files: {len(duplicated_ids)}")

    if duplicated_ids:
        print("\nERROR: IDs found across multiple chunk files:\n")

        for pump_id, files in sorted(duplicated_ids.items()):
            print(f"{pump_id}: {', '.join(files)}")

        sys.exit(1)

    print("\nOK: No id_pump occurs across different chunk files.")
    sys.exit(0)


if __name__ == "__main__":

    if len(sys.argv) != 2:
        print("Usage: python qc_chunks.py <chunks_output_dir>")
        sys.exit(1)

    check_id_pump_uniqueness(sys.argv[1])

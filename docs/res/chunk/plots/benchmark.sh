#!/usr/bin/env bash

# benchmark.sh
# Compresses enwik* files with multiple algorithms via 7z and logs compression ratios.
# Usage: ./benchmark.sh <directory>

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

ALGORITHMS=("deflate" "bzip2" "lzma2")
FILES=("enwik4" "enwik5" "enwik6" "enwik7" "enwik8" "enwik9")
OUTPUT_CSV="benchmark_results.csv"
TMPDIR_BASE=$(mktemp -d)

# Map algorithm names to 7z method strings
declare -A METHOD_FLAG
METHOD_FLAG["deflate"]="-m0=deflate"
METHOD_FLAG["bzip2"]="-m0=bzip2"
METHOD_FLAG["lzma2"]="-m0=lzma2"

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <directory>" >&2
    exit 1
fi

DATA_DIR="${1%/}"   # strip trailing slash

if [[ ! -d "$DATA_DIR" ]]; then
    echo "Error: '$DATA_DIR' is not a directory." >&2
    exit 1
fi

# Verify 7z is available
if ! command -v 7z &>/dev/null; then
    echo "Error: '7z' not found in PATH. Please install p7zip." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Helper – compress one file and return compressed size in bytes
# ---------------------------------------------------------------------------
compress_file() {
    local algo="$1"
    local src="$2"          # full path to source file
    local out_archive="$3"  # destination archive path

    7z a \
        -t7z \
        "${METHOD_FLAG[$algo]}" \
        -mx=9 \
        "$out_archive" \
        "$src" \
        > /dev/null 2>&1

    # Portable byte-accurate file size
    wc -c < "$out_archive"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

# Build CSV header
header="algorithm"
for fname in "${FILES[@]}"; do
    header+=",${fname}"
done

echo "$header" > "$OUTPUT_CSV"
echo "Results will be written to: $OUTPUT_CSV"
echo

# Iterate algorithms (rows)
for algo in "${ALGORITHMS[@]}"; do
    row="$algo"
    echo "── Algorithm: ${algo} ──────────────────────────"

    for fname in "${FILES[@]}"; do
        src="${DATA_DIR}/${fname}"

        if [[ ! -f "$src" ]]; then
            echo "  [SKIP] '$src' not found – inserting 'N/A'"
            row+=",N/A"
            continue
        fi

        original_size=$(wc -c < "$src")

        # Unique temp archive per algo+file
        tmp_archive="${TMPDIR_BASE}/${algo}_${fname}.7z"

        echo -n "  Compressing ${fname} (${original_size} bytes) ... "

        compressed_size=$(compress_file "$algo" "$src" "$tmp_archive")

        # Compression ratio: original / compressed  (higher = better)
        ratio=$(awk "BEGIN { printf \"%.6f\", ${original_size} / ${compressed_size} }")

        echo "compressed=${compressed_size} bytes  ratio=${ratio}"

        row+=",${ratio}"

        # Remove the temp archive immediately to save disk space
        rm -f "$tmp_archive"
    done

    echo "$row" >> "$OUTPUT_CSV"
    echo
done

# Clean up temp directory
rm -rf "$TMPDIR_BASE"

echo "Done. Benchmark results saved to '${OUTPUT_CSV}'."

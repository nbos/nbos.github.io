import csv
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import LogLocator, FuncFormatter

def read_columns(csv_path, x_col=0, y_col=1):
    xs, ys = [], []
    with open(csv_path, "r", newline="", errors="ignore") as f:
        reader = csv.reader(f)
        for row in reader:
            if len(row) <= max(x_col, y_col):
                continue
            try:
                x = float(row[x_col])
                y = float(row[y_col])
                xs.append(x)
                ys.append(y)
            except ValueError:
                continue
    return xs, ys

def decimal_notation_formatter(x, pos):
    """Format log ticks using plain decimal notation (e.g. 9e3 -> 9000)."""
    if x == 0:
        return "0"
    if abs(x - int(x)) < 1e-8:
        return str(int(x))
    else:
        return f"{x:.4g}"

def main():
    csv_files = ["overlap-functions/length-naive.csv",
                 "overlap-functions/naive-spmi.csv",
                 "overlap-functions/spmi-length.csv"]
    labels = ["Code length - Joint count", "Joint count - SPMI", "SPMI - Code length"]
    
    plt.figure(figsize=(6, 4))

    line_styles = [
        ('k', '-'),
        ('0.7', '-'),
        ('k', ':'),
    ]

    all_x, all_y = [], []

    for idx, csv_path in enumerate(csv_files):
        x, y = read_columns(csv_path, x_col=0, y_col=1)
        if not x:
            print(f"Warning: no usable data in {csv_path}")
            continue
        all_x.extend(x)
        all_y.extend(y)

        color, linestyle = line_styles[idx % len(line_styles)]
        plt.plot(x, y, color=color, linestyle=linestyle, linewidth=1.5, label=labels[idx])

    if not all_x or not all_y:
        print("No valid data found in provided CSV files.")
        return

    xmin = min(v for v in all_x if v > 0)
    xmax = max(all_x)
    ymin = min(all_y)

    plt.xlabel('Dictionary size', fontsize=12)
    plt.ylabel('Overlap', fontsize=12)
    plt.grid(True, which='both', alpha=0.3)

    plt.axhline(y=ymin, color='k', linestyle='-', alpha=0.3)
    plt.axvline(x=xmin, color='k', linestyle='-', alpha=0.3)

    plt.xscale('log')
    plt.xlim(xmin, xmax)
    plt.ylim(bottom=ymin)

    ax = plt.gca()
    ax.xaxis.set_major_locator(LogLocator(base=10.0, subs=np.arange(1, 10)*0.1/0.1, numticks=100))
    ax.xaxis.set_major_formatter(FuncFormatter(decimal_notation_formatter))

    plt.setp(ax.get_xticklabels(), rotation=270, ha="center")

    plt.legend(fontsize=12)
    plt.tick_params(axis='both', which='major', labelsize=10)

    plt.tight_layout()
    plt.savefig("overlap-functions.svg", dpi=300, bbox_inches='tight')
    plt.show()
    print("Saved plot as overlap.svg")

if __name__ == "__main__":
    main()

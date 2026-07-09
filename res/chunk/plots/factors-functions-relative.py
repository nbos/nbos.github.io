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
    csv_files = ["../out/enwik7.csv", "../out/enwik7-spmi-loss.csv", "../out/enwik7-naive-loss.csv"]
    labels = ["Code length (1.0)", "SPMI", "Joint count (naive)"]

    plt.figure(figsize=(6, 5))

    line_styles = [
        (4.0, 'lightgray', '-'),
        (1.0, 'k', '-'),
        (1.0, 'k', '--'),
    ]

    # --- Load all series first ---
    series = []
    for csv_path in csv_files:
        x, y = read_columns(csv_path, x_col=0, y_col=1)
        if not x:
            print(f"Warning: no usable data in {csv_path}")
            series.append(([], []))
        else:
            series.append((x, y))

    # --- Build the baseline interpolator from the "Code length" series (index 0) ---
    base_x, base_y = series[0]
    if not base_x:
        print("No valid baseline data found in the first CSV file.")
        return

    base_x_arr = np.array(base_x)
    base_y_arr = np.array(base_y)
    # Sort by x for reliable interpolation
    sort_idx = np.argsort(base_x_arr)
    base_x_sorted = base_x_arr[sort_idx]
    base_y_sorted = base_y_arr[sort_idx]

    def baseline_at(x_vals):
        """Interpolate baseline y-values at the given x positions (log-space interp)."""
        return np.interp(np.log(x_vals), np.log(base_x_sorted), base_y_sorted)

    # --- Plot each series divided by the baseline ---
    all_x, all_y_rel = [], []

    for idx, (x, y) in enumerate(series):
        if not x:
            continue

        x_arr = np.array(x)
        y_arr = np.array(y)
        baseline_vals = baseline_at(x_arr)
        y_rel = y_arr / baseline_vals

        all_x.extend(x_arr.tolist())
        all_y_rel.extend(y_rel.tolist())

        w, color, linestyle = line_styles[idx % len(line_styles)]
        plt.plot(x_arr, y_rel, color=color, linestyle=linestyle,
                 linewidth=w, label=labels[idx])

    if not all_x or not all_y_rel:
        print("No valid data found in provided CSV files.")
        return

    xmin = min(v for v in all_x if v > 0)
    xmax = max(all_x)
    ymin = min(all_y_rel)

    plt.xlabel('Dictionary size', fontsize=12)
    plt.ylabel('Relative compression factor', fontsize=12)
    plt.grid(True, which='both', alpha=0.3)

    plt.axhline(y=ymin- 0.002, color='k', linestyle='-', alpha=0.3)
    plt.axvline(x=xmin, color='k', linestyle='-', alpha=0.3)

    plt.xscale('log')
    plt.xlim(xmin, xmax)
    plt.ylim(bottom=ymin - 0.002)

    ax = plt.gca()
    ax.xaxis.set_major_locator(LogLocator(base=10.0, subs=np.arange(1, 10)*0.1/0.1, numticks=100))
    ax.xaxis.set_major_formatter(FuncFormatter(decimal_notation_formatter))

    plt.setp(ax.get_xticklabels(), rotation=270, ha="center")

    plt.legend(fontsize=12)
    plt.tick_params(axis='both', which='major', labelsize=10)

    plt.tight_layout()
    plt.savefig("factors-functions-relative.svg", dpi=300, bbox_inches='tight')
    plt.show()
    print("Saved plot as factors-functions-relative.svg")

if __name__ == "__main__":
    main()

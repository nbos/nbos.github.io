import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import LogLocator, FuncFormatter

def read_word_lengths_with_running_avg(txt_path):
    """Read integers from file and compute running average using Welford's algorithm."""
    xs, ys = [], []
    running_mean = 0.0
    count = 0
    
    with open(txt_path, "r", errors="ignore") as f:
        for line_idx, line in enumerate(f):
            line = line.strip()
            if not line:
                continue
            try:
                value = int(line)
                count += 1
                # Welford's online algorithm for numerically stable running mean
                running_mean += (value - running_mean) / count
                
                xs.append(256 + line_idx)
                ys.append(running_mean)
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
    txt_files = ["enwik7-wl.txt",
                 "enwik7-naive-loss-wl.txt",
                 "enwik7-spmi-loss-wl.txt"]
    labels = ["Code length", "Joint count (naive)", "SPMI"]
    
    plt.figure(figsize=(6, 5))

    line_styles = [
        ('k', '-'),
        ('k', '--'),
        ('k', ':'),
    ]

    all_x, all_y = [], []

    for idx, txt_path in enumerate(txt_files):
        x, y = read_word_lengths_with_running_avg(txt_path)
        if not x:
            print(f"Warning: no usable data in {txt_path}")
            continue
        all_x.extend(x)
        all_y.extend(y)

        color, linestyle = line_styles[idx % len(line_styles)]
        plt.plot(x, y, color=color, linestyle=linestyle, linewidth=1.5, label=labels[idx])

    if not all_x or not all_y:
        print("No valid data found in provided text files.")
        return

    xmin = min(v for v in all_x if v > 0)
    xmax = max(all_x)
    ymin = min(all_y)

    plt.xlabel('Dictionary size', fontsize=12)
    plt.ylabel('Average Word Length', fontsize=12)
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
    plt.savefig("wl-functions.svg", dpi=300, bbox_inches='tight')
    plt.show()
    print("Saved plot as functions-wl.svg")

if __name__ == "__main__":
    main()

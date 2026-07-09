import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
from matplotlib.ticker import LogLocator, FuncFormatter, NullFormatter

def decimal_notation_formatter(x, pos):
    if x == 0:
        return "0"
    if abs(x - int(x)) < 1e-8:
        return str(int(x))
    else:
        return f"{x:.4g}"

# ── Manual offset controls ────────────────────────────────────────────────────
LABEL_DY          = 0.15   # vertical offset above each endpoint dot for text labels
Y_MARKER_X_OFFSET = 30     # horizontal offset (in data units) from x_min for the < markers
Y_LABEL_X_OFFSET  = 100    # horizontal offset (in data units) from x_min for Y value text
ENWIK9_LABEL_DX   = -0.15  # fractional nudge left for enwik9 label (multiplied by x_enwik9)

# Get script directory
script_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(script_dir, '../out')

csv_files = [
    os.path.join(data_dir, 'enwik4.csv'),
    os.path.join(data_dir, 'enwik5.csv'),
    os.path.join(data_dir, 'enwik6.csv'),
    os.path.join(data_dir, 'enwik7.csv')
]

for csv_path in csv_files:
    if not os.path.exists(csv_path):
        print(f"Error: File '{csv_path}' does not exist")
        exit(1)

all_data = []
labels = []

for csv_path in csv_files:
    try:
        df = pd.read_csv(csv_path, escapechar='\\')
        if df.shape[1] < 2:
            print(f"Error: CSV file '{csv_path}' must have at least 2 columns")
            continue

        x_data = pd.to_numeric(df.iloc[:, 0], errors='coerce').fillna(0).values
        y_data = pd.to_numeric(df.iloc[:, 1], errors='coerce').fillna(0).values

        valid_mask = ~(np.isnan(x_data) | np.isnan(y_data) | (x_data <= 0))
        x_data = x_data[valid_mask]
        y_data = y_data[valid_mask]

        if len(x_data) == 0:
            print(f"Warning: No valid data in '{csv_path}', skipping")
            continue

        all_data.append((x_data, y_data))
        basename = os.path.basename(csv_path)
        label = os.path.splitext(basename)[0]
        labels.append(label)
        print(f"Loaded '{csv_path}': {len(x_data)} data points")

    except Exception as e:
        print(f"Error reading '{csv_path}': {e}")
        continue

if not all_data:
    print("Error: No valid CSV files could be loaded")
    exit(1)

global_min_x = min(min(x_data) for x_data, y_data in all_data)
global_min_y = min(min(y_data) for x_data, y_data in all_data)

# Shift each dataset so it starts at the global minimum, then keep ONLY the last point
processed_data = []
endpoints_x = []
endpoints_y = []

for i, (x_data, y_data) in enumerate(all_data):
    x_shifted = x_data - x_data[0] + global_min_x
    y_shifted = y_data - y_data[0] + global_min_y
    processed_data.append((x_shifted, y_shifted))
    endpoints_x.append(x_shifted[-1])
    endpoints_y.append(y_shifted[-1])

endpoints_x = np.array(endpoints_x)
endpoints_y = np.array(endpoints_y)

# ── Log-linear fit through the four endpoints ─────────────────────────────────
log_ex = np.log10(endpoints_x)
coeffs = np.polyfit(log_ex, endpoints_y, 1)
a, b = coeffs

def fit_y(x):
    return a * np.log10(x) + b

# ── Extrapolate enwik8 & enwik9 X positions ───────────────────────────────────
log_spacings    = np.diff(np.log10(endpoints_x))
avg_log_spacing = np.mean(log_spacings)

x_enwik8 = endpoints_x[-1] * (10 **  avg_log_spacing)
x_enwik9 = endpoints_x[-1] * (10 ** (2 * avg_log_spacing))
y_enwik8 = fit_y(x_enwik8)
y_enwik9 = fit_y(x_enwik9)

print(f"\nAverage log10 spacing between enwik endpoints: {avg_log_spacing:.4f}")
print(f"Extrapolated enwik8: x = {x_enwik8:.4g}, y = {y_enwik8:.6f}")
print(f"Extrapolated enwik9: x = {x_enwik9:.4g}, y = {y_enwik9:.6f}")

# ── Axis limits (computed before drawing so we can extend the fit to borders) ──
all_x_pts = np.concatenate([x for x, y in processed_data] + [[x_enwik8, x_enwik9]])
all_y_pts = np.concatenate([y for x, y in processed_data] + [[y_enwik8, y_enwik9]])

x_min_data = np.min(all_x_pts)
x_max_data = np.max(all_x_pts)
y_min_data = np.min(all_y_pts)
y_max_data = np.max(all_y_pts)

x_plot_left  = x_min_data
x_plot_right = x_max_data * 1.3
y_plot_bottom = 2
y_plot_top    = y_max_data * 1.05

# Extend fit line all the way to the plot borders in log space
x_fit_full = np.logspace(np.log10(x_plot_left), np.log10(x_plot_right), 600)
y_fit_full = fit_y(x_fit_full)

# ── Plot ──────────────────────────────────────────────────────────────────────
plt.figure(figsize=(6, 5))
ax = plt.gca()

# Dotted fit line across the full plot width
ax.plot(x_fit_full, y_fit_full,
        color='gray', linewidth=2.0, linestyle='--', zorder=1)

# Single dot at each enwik4-7 endpoint (no curve, no intermediate points)
for i, (x_end, y_end) in enumerate(zip(endpoints_x, endpoints_y)):
    label = labels[i]
    ax.plot(x_end, y_end, 'o', color='black', markersize=5, zorder=3)

    # Label above the dot
    label_x = x_end
    # if label == 'enwik9':
    #     label_x = x_end * (1 + ENWIK9_LABEL_DX)
    ax.annotate(label,
                xy=(x_end, y_end),
                xytext=(label_x, y_end + LABEL_DY),
                textcoords='data',
                fontfamily='monospace',
                fontsize=12,
                ha='center',
                va='bottom')

# Left-axis markers + dot + label for extrapolated enwik8 & enwik9
for label, x_e, y_e in [('enwik8', x_enwik8, y_enwik8),
                          ('enwik9', x_enwik9, y_enwik9)]:
    # Triangle marker near the Y axis
    ax.plot(x_plot_left + Y_MARKER_X_OFFSET, y_e,
            marker='<', markersize=8,
            color=(0, 0, 0, 0), markeredgecolor='black', markeredgewidth=0.8,
            zorder=3)
    # Y value label to the right of the triangle
    ax.text(x_plot_left + Y_LABEL_X_OFFSET, y_e,
            f"{y_e:.2f} ({label}) (predicted)",
            fontfamily='monospace',
            fontsize=12,
            va='center',
            ha='left',
            zorder=3)
    # Dot at the extrapolated point
    ax.plot(x_e, y_e, 'o', color='black', markersize=5, zorder=3)
    # Label above the dot
    # label_x = x_e
    # if label == 'enwik9':
    #     label_x = x_e * (1 + ENWIK9_LABEL_DX)
    # ax.annotate(label,
    #             xy=(x_e, y_e),
    #             xytext=(label_x, y_e + LABEL_DY),
    #             textcoords='data',
    #             fontfamily='monospace',
    #             fontsize=12,
    #             ha='center',
    #             va='bottom')

# ── Axes & formatting ─────────────────────────────────────────────────────────
plt.xscale('log')
plt.xlim(left=x_plot_left,   right=x_plot_right)
plt.ylim(bottom=y_plot_bottom, top=y_plot_top)

# Major ticks at 1, 2, 5 × 10^n
ax.xaxis.set_major_locator(LogLocator(base=10.0, subs=[1, 2, 4, 6, 8], numticks=20))
ax.xaxis.set_major_formatter(FuncFormatter(decimal_notation_formatter))
# Minor ticks at everything else — visible but no label
ax.xaxis.set_minor_locator(LogLocator(base=10.0, subs=np.arange(1, 10), numticks=100))
ax.xaxis.set_minor_formatter(NullFormatter())

# ax.xaxis.set_major_locator(LogLocator(base=10.0, subs=np.arange(1, 10), numticks=100))
# ax.xaxis.set_major_formatter(FuncFormatter(decimal_notation_formatter))
plt.setp(ax.get_xticklabels(), rotation=270, ha="center")

plt.xlabel('Number of symbols', fontsize=12)
plt.ylabel('Compression factor', fontsize=12)

plt.grid(True, which='both', alpha=0.3)
plt.axhline(y=y_min_data, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=x_min_data, color='k', linestyle='-', alpha=0.3)

plt.tick_params(axis='both', which='major', labelsize=10)
plt.tick_params(axis='both', which='minor', labelsize=8)

output_path = os.path.join(script_dir, 'extrapolated.svg')
plt.tight_layout()
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f"\nPlot saved to: {output_path}")
plt.show()

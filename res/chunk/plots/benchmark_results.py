# benchmark_results.py

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import numpy as np
import os

# ── Paths ─────────────────────────────────────────────────────────────────────
script_dir = os.path.dirname(os.path.abspath(__file__))
csv_path   = os.path.join(script_dir, 'benchmark_results.csv')

if not os.path.exists(csv_path):
    print(f"Error: File '{csv_path}' does not exist")
    exit(1)

# ── Load data ─────────────────────────────────────────────────────────────────
df = pd.read_csv(csv_path, index_col='Algorithm')

x_labels = list(df.columns)           # enwik4 … enwik9
x_pos    = np.arange(len(x_labels))   # 0 … 5

# ── Line styles (all grayscale / differentiable) ──────────────────────────────
# Code length : solid black,  filled black circle  (special gray for enwik8/9)
# DEFLATE     : dashed dark-gray, filled square
# BZip2       : dash-dot medium-gray, filled triangle-up
# LZMA2       : dotted light-gray, filled diamond

styles = {
    'Code length':   dict(color='black',   linestyle='-',  marker='o', markersize=8, linewidth=1.8),
    'DEFLATE (ZIP)': dict(color='#444444', linestyle='--', marker='s', markersize=8, linewidth=1.8),
    'BZip2':         dict(color='#777777', linestyle='-.', marker='^', markersize=8, linewidth=1.8),
    'LZMA2 (7z)':    dict(color='#aaaaaa', linestyle=':',  marker='D', markersize=8, linewidth=1.8),
}

# Legend order: Code length first, then the rest
legend_order = ['Code length', 'DEFLATE (ZIP)', 'BZip2', 'LZMA2 (7z)']

# ── Plot ──────────────────────────────────────────────────────────────────────
plt.figure(figsize=(6, 4.5))
ax = plt.gca()

for algo in legend_order:
    row = df.loc[algo]
    s   = styles[algo]
    y   = row.values.astype(float)

    if algo == 'Code length':
        # Draw the line first
        ax.plot(x_pos, y,
                color=s['color'], linestyle=s['linestyle'],
                linewidth=s['linewidth'], zorder=2)

        # Draw each dot individually so we can colour enwik8/9 differently
        for xi, yi in zip(x_pos, y):
            x_label = x_labels[xi]
            dot_color = '#cccccc' if x_label in ('enwik8', 'enwik9') else 'black'
            ax.plot(xi, yi,
                    marker=s['marker'], markersize=s['markersize'],
                    color=dot_color,
                    # markeredgecolor='black',
                    markeredgewidth=0.6,
                    zorder=3)
    else:
        ax.plot(x_pos, y,
                color=s['color'], linestyle=s['linestyle'],
                marker=s['marker'], markersize=s['markersize'],
                linewidth=s['linewidth'],
                markeredgecolor=s['color'], markeredgewidth=0.6,
                zorder=2)

# ── Axes & formatting ─────────────────────────────────────────────────────────
ax.set_xticks(x_pos)
ax.set_xticklabels(x_labels, fontfamily='monospace', fontsize=10)
ax.tick_params(axis='both', which='major', labelsize=10)

# ax.set_xlabel('Dataset', fontsize=12)
ax.set_ylabel('Compression factor', fontsize=12)

ax.grid(True, which='both', alpha=0.3)

# ── Legend ────────────────────────────────────────────────────────────────────
legend_handles = []
for algo in legend_order:
    s = styles[algo]
    handle = mlines.Line2D(
        [], [],
        color=s['color'],
        linestyle=s['linestyle'],
        linewidth=s['linewidth'],
        marker=s['marker'],
        markersize=s['markersize'],
        markeredgecolor='black' if algo == 'Code length' else s['color'],
        markeredgewidth=0.6,
        label=algo,
    )
    legend_handles.append(handle)

ax.legend(handles=legend_handles,
          fontsize=12,
          framealpha=0.8,
          edgecolor='#cccccc')

# ── Save ──────────────────────────────────────────────────────────────────────
output_path = os.path.join(script_dir, 'benchmark_results.svg')
plt.tight_layout()
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f"Plot saved to: {output_path}")
plt.show()

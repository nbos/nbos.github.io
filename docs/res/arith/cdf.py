import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

# ----------------- Parameters -----------------
bits = (1, 2, 3)         # number of bits per line (top->bottom)
x_range = (0.0, 1.0)     # domain, closed at both ends
top = 0.85               # y-position of the top line
bottom = 0.25            # y-position of the bottom-most binary line
label_offset = 0.06      # vertical offset for labels from each binary line
tick_half_height = 0.065 # half height of division markers / half height of categorical boxes
line_lw = 3
tick_lw = 2
fontsize = 17
figsize = None

# Colors (binary vs categorical)
binary_color = 'black'
categorical_color = 'black'

# Only show letter labels if their box width >= this fraction of the x-range
# 0.02 hides very small bins (e.g., W, X, Y, Z, J, K, B, Q) by default.
min_label_width_frac = 0.03

# ----------------- Figure size -----------------
n_binary = len(bits)
n_total_lines = n_binary + 1  # +1 for the categorical boxes row
if figsize is None:
    figsize = (10, max(2.2, 0.7 * n_total_lines + 1.0))

fig, ax = plt.subplots(figsize=figsize)

# ----------------- Y positions -----------------
# Existing binary lines: spaced from top to bottom
if n_binary == 1:
    y_positions_binary = [0.55]
else:
    y_positions_binary = np.linspace(top, bottom, n_binary)

# Compute the spacing used by the binary stack and place the new categorical row equally below
if n_binary > 1:
    dy = (top - bottom) / (n_binary - 1)
else:
    dy = 0.30  # fallback spacing if only one binary line

y_categorical = y_positions_binary[-1] - dy  # equally spaced below the bottom binary line

x0, x1 = x_range
x_len = x1 - x0
min_label_width = min_label_width_frac * x_len

# ----------------- Draw binary lines -----------------
for level, y in zip(bits, y_positions_binary):
    n_segments = 2 ** level

    # Main horizontal line
    lc_main = ax.hlines(y, x0, x1, colors=binary_color, linewidth=line_lw, zorder=2)
    try:
        lc_main.set_capstyle('butt')
    except Exception:
        pass

    # Division markers (including both ends)
    xs = np.linspace(x0, x1, n_segments + 1)
    lc_ticks = ax.vlines(
        xs, y - tick_half_height, y + tick_half_height,
        colors=binary_color, linewidth=tick_lw,
        clip_on=False, zorder=3
    )
    try:
        lc_ticks.set_capstyle('butt')
    except Exception:
        pass

    # Binary labels at segment centers (above the line)
    mids = (xs[:-1] + xs[1:]) / 2.0
    for i, xm in enumerate(mids):
        label = format(i, f'0{level}b')
        ax.text(xm, y + label_offset, label, ha='center', va='bottom', fontsize=fontsize, zorder=4)

# ----------------- Categorical (A–Z) boxes -----------------
letters = [chr(c) for c in range(ord('A'), ord('Z') + 1)]
probs = np.array([
    0.0858, 0.0135, 0.0316, 0.0356, 0.1197, 0.0199, 0.0219, 0.0406, 0.0776, 0.0012,
    0.0064, 0.0441, 0.0304, 0.0715, 0.0768, 0.0240, 0.0039, 0.0672, 0.0634, 0.0893,
    0.0308, 0.0108, 0.0135, 0.0035, 0.0156, 0.0014
], dtype=float)

# Normalize defensively if slight rounding error
total_p = probs.sum()
if not np.isclose(total_p, 1.0):
    probs = probs / total_p

# CDF edges mapped to x_range
cdf_edges = np.concatenate(([0.0], np.cumsum(probs)))
xs_cat = x0 + x_len * cdf_edges

# Draw boxes and in-box labels
y_bottom = y_categorical - (1.5*tick_half_height)
box_height = 3.0 * tick_half_height

for i in range(len(letters)):
    left = xs_cat[i]
    right = xs_cat[i + 1]
    width = right - left

    # Rectangle (unfilled) for each categorical bin
    rect = Rectangle(
        (left, y_bottom), width, box_height,
        fill=False, edgecolor=categorical_color, linewidth=tick_lw,
        zorder=3, clip_on=False
    )
    ax.add_patch(rect)

    # Draw label only if box is wide enough; centered vertically and horizontally
    if width >= min_label_width:
        ax.text(
            left + width / 2.0, y_categorical, letters[i],
            ha='center', va='center', fontsize=fontsize, color=categorical_color, zorder=4
        )

# ----------------- Axes formatting -----------------
ax.set_xlim(x0, x1)

# Expand lower ylim if the categorical boxes/labels fall below 0
y_min_needed = min(0.0, y_bottom - 0.04)
ax.set_ylim(y_min_needed, 1.0)

ax.set_xticks([])
ax.set_yticks([])
for spine in ax.spines.values():
    spine.set_visible(False)

plt.tight_layout()
plt.savefig('cdf.svg')
plt.show()

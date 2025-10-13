import numpy as np
import matplotlib.pyplot as plt


bits=(1, 2, 3, 4, 5, 6)          # number of bits per line (top->bottom). 1->2 segments, 2->4, etc.
x_range=(0.0, 1.0)      # domain, closed at both ends
top=0.85                # y-position of the top line
bottom=0.15             # y-position of the bottom line
label_offset=0.025       # vertical offset for labels above each line
tick_half_height=0.055  # half height of division markers
line_lw=3
tick_lw=2
fontsize=30
figsize=None

if figsize is None:
    figsize = (10, max(2.2, 0.7 * len(bits) + 1.0))

fig, ax = plt.subplots(figsize=figsize)

# Compute y-positions (top -> bottom)
if len(bits) == 1:
    y_positions = [0.55]
else:
    y_positions = np.linspace(top, bottom, len(bits))

x0, x1 = x_range

for level, y in zip(bits, y_positions):
    n_segments = 2 ** level

    # Main horizontal line
    ax.hlines(y, x0, x1, colors='black', linewidth=line_lw, zorder=2)

    # Division markers (including both ends)
    xs = np.linspace(x0, x1, n_segments + 1)
    ax.vlines(
        xs, y - tick_half_height, y + tick_half_height, 
        colors='black', linewidth=tick_lw,
        clip_on=False, zorder=3
    )

    # Ensure square ends (avoids rounded ends that can look thinner when clipped)
    try:
        lc.set_capstyle('butt')
    except Exception:
        pass
    
    # Binary labels at segment centers
    if level < 4:
        mids = (xs[:-1] + xs[1:]) / 2.0
        for i, xm in enumerate(mids):
            label = format(i, f'0{level}b')
            ax.text(xm, y + label_offset, label, ha='center', va='bottom', fontsize=fontsize, zorder=4)

# Formatting: 0..1 on x, no ticks/labels/frames
ax.set_xlim(x0, x1)
ax.set_ylim(0, 1)
ax.set_xticks([])
ax.set_yticks([])
for spine in ax.spines.values():
    spine.set_visible(False)

plt.tight_layout()
plt.savefig('binary.svg')
plt.show()

import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
import numpy as np
from scipy.stats import norm

# =============================================================================
# PLOT PARAMETERS
# =============================================================================
# Figure dimensions
FIG_WIDTH = 12
FIG_HEIGHT = 3

# Colors
BG_COLOR = 'white'
OUTLINE_COLOR = 'black'
SEPARATOR_COLOR = '0.3'  # gray
TEXT_COLOR = 'black'

# Line properties
OUTLINE_WIDTH = 3
SEPARATOR_WIDTH = 1.0
TICK_WIDTH = 2.0

# Font properties
LABEL_FONT_SIZE = 25
AXIS_FONT_SIZE = 25

# Layout dimensions
RECT_HEIGHT = 1.0
TICK_LENGTH_FACTOR = 0.08  # tick length as fraction of rectangle height
Y_MARGIN_FACTOR = 1.2
Y_TOP_FACTOR = 1.02

# Subplot margins (left, bottom, right, top)
SUBPLOT_LEFT = 0.02
SUBPLOT_BOTTOM = 0.1
SUBPLOT_RIGHT = 0.98
SUBPLOT_TOP = 0.9

# Text positioning
MIN_LABEL_WIDTH = 0.09     # minimum segment width to show label (in probability units)
TICK_LABEL_OFFSET = 1.6   # label position below tick (as factor of tick length)

# Save parameters
SAVE_FORMAT = 'svg'
SAVE_BBOX = 'tight'
SAVE_PAD = 0.02
# =============================================================================

# Gaussian parameters
mu = 0      # mean
sigma = 3   # standard deviation

# Determine range of integers to show separators
# Go far enough to show the "squeeze to infinity" effect
z_min = -50
z_max = 50

# Generate integer points and their corresponding CDF values
integers = np.arange(z_min, z_max + 1)
cdf_values = norm.cdf(integers, loc=mu, scale=sigma)

# Figure/axes setup: elongated, grayscale, no axes decorations
fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), constrained_layout=False)

# Reduce subplot margins
plt.subplots_adjust(left=SUBPLOT_LEFT, bottom=SUBPLOT_BOTTOM,
                   right=SUBPLOT_RIGHT, top=SUBPLOT_TOP)

ax.set_xlim(0, 1)
tick_len = TICK_LENGTH_FACTOR * RECT_HEIGHT
ax.set_ylim(-tick_len * Y_MARGIN_FACTOR, RECT_HEIGHT * Y_TOP_FACTOR)
ax.set_facecolor(BG_COLOR)
fig.patch.set_facecolor(BG_COLOR)
ax.axis('off')  # remove default axes, ticks, labels

# Draw the elongated black outline rectangle [0, 1] x [0, rect_height]
outline = Rectangle((0, 0), 1.0, RECT_HEIGHT, fill=False,
                   edgecolor=OUTLINE_COLOR, linewidth=OUTLINE_WIDTH)
ax.add_patch(outline)

# Draw gray vertical separators at CDF values corresponding to integers
for i, z in enumerate(integers):
    cdf_val = cdf_values[i]
    
    # Draw separator if not at exact boundaries (avoid 0 and 1)
    if 0.0001 < cdf_val < 0.9999:
        ax.vlines(cdf_val, 0, RECT_HEIGHT, colors=SEPARATOR_COLOR, linewidth=SEPARATOR_WIDTH)

# Draw labels for segments between separators
for i in range(len(integers) - 1):
    z = integers[i]
    cdf_start = cdf_values[i]
    cdf_end = cdf_values[i + 1]
    segment_width = cdf_end - cdf_start
    
    # Only show label if segment is wide enough
    if segment_width >= MIN_LABEL_WIDTH:
        # Create appropriate label
        if z == mu:
            label = "⌊μ⌋"
        elif z > mu:
            label = f"⌊μ⌋+{z - mu}"
        else:
            label = f"⌊μ⌋{z - mu}"  # This creates μ-1, μ-2, etc.
        
        # Place label in center of segment
        ax.text(
            cdf_start + segment_width / 2, RECT_HEIGHT / 2, label,
            ha='center', va='center', color=TEXT_COLOR, fontsize=LABEL_FONT_SIZE
        )

# CHANGE THIS IF THE EDGES AREN'T SEAMLESS
correction = 0.333333
    
# Custom minimal "x-axis": single down ticks and labels at 0 and 1
for xpos, txt in [(0.0, '0'), (1.0, '1')]:
    # tick mark downward with minimal overlap to ensure visual continuity
    ax.plot([xpos, xpos], [OUTLINE_WIDTH * correction, -tick_len], color=OUTLINE_COLOR, linewidth=TICK_WIDTH, clip_on=False)
    # label below the tick
    ax.text(xpos, -tick_len * TICK_LABEL_OFFSET, txt,
           ha='center', va='top', color=TEXT_COLOR, fontsize=AXIS_FONT_SIZE)

plt.savefig('gauss-cdf.svg', format=SAVE_FORMAT, bbox_inches=SAVE_BBOX, pad_inches=SAVE_PAD)
plt.show()

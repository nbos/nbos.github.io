import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

# =============================================================================
# PLOT PARAMETERS
# =============================================================================
# Figure dimensions
FIG_WIDTH = 12
FIG_HEIGHT = 3

# Colors
BG_COLOR = 'white'
OUTLINE_COLOR = 'black'
SEPARATOR_COLOR = '0.4'  # gray
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
MIN_LABEL_WIDTH = 0.03     # minimum segment width to show label (in probability units)
TICK_LABEL_OFFSET = 1.6   # label position below tick (as factor of tick length)

# Save parameters
SAVE_FORMAT = 'svg'
SAVE_BBOX = 'tight'
SAVE_PAD = 0.02
# =============================================================================

# Data: categorical probabilities (sum to 1.0)
probs = [
    ('A', 0.0858), ('B', 0.0135), ('C', 0.0316), ('D', 0.0356), ('E', 0.1197),
    ('F', 0.0199), ('G', 0.0219), ('H', 0.0406), ('I', 0.0776), ('J', 0.0012),
    ('K', 0.0064), ('L', 0.0441), ('M', 0.0304), ('N', 0.0715), ('O', 0.0768),
    ('P', 0.0240), ('Q', 0.0039), ('R', 0.0672), ('S', 0.0634), ('T', 0.0893),
    ('U', 0.0308), ('V', 0.0108), ('W', 0.0135), ('X', 0.0035), ('Y', 0.0156),
    ('Z', 0.0014),
]

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

# Draw gray vertical separators at cumulative probabilities
x = 0.0

for i, (label, p) in enumerate(probs):
    next_x = x + p
    # vertical separator at the right boundary of this segment (except at x=1)
    if next_x < 1.0 - 1e-12:
        ax.vlines(next_x, 0, RECT_HEIGHT, colors=SEPARATOR_COLOR, linewidth=SEPARATOR_WIDTH)

    # label in the middle of the segment if wide enough
    if p >= MIN_LABEL_WIDTH:
        ax.text(
            x + p / 2, RECT_HEIGHT / 2, label,
            ha='center', va='center', color=TEXT_COLOR, fontsize=LABEL_FONT_SIZE
        )
    x = next_x

# CHANGE THIS IF THE EDGES AREN'T SEAMLESS
correction = 0.333333
    
# Custom minimal "x-axis": single down ticks and labels at 0 and 1
for xpos, txt in [(0.0, '0'), (1.0, '1')]:
    # tick mark downward with minimal overlap to ensure visual continuity
    ax.plot([xpos, xpos], [OUTLINE_WIDTH * correction, -tick_len], color=OUTLINE_COLOR, linewidth=TICK_WIDTH, clip_on=False)
    # label below the tick
    ax.text(xpos, -tick_len * TICK_LABEL_OFFSET, txt,
           ha='center', va='top', color=TEXT_COLOR, fontsize=AXIS_FONT_SIZE)

plt.savefig('alphabet.svg', format=SAVE_FORMAT, bbox_inches=SAVE_BBOX, pad_inches=SAVE_PAD)
plt.show()

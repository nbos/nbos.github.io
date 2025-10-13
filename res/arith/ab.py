import numpy as np
import matplotlib.pyplot as plt


def generate_segments_and_labels(level, split_ratio=2/3):
    """Generate segment boundaries and labels for nested A/B divisions."""
    segments = [(0.0, 1.0)]
    labels = ['']
    
    for i in range(level):
        new_segments = []
        new_labels = []
        
        for (a, b), label in zip(segments, labels):
            # Divide at split_ratio
            split_point = a + (b - a) * split_ratio
            
            # Left segment gets 'A', right segment gets 'B'
            new_segments.extend([(a, split_point), (split_point, b)])
            new_labels.extend([label + 'A', label + 'B'])
        
        segments = new_segments
        labels = new_labels
    
    return segments, labels


def estimate_label_width(label, fontsize, ax):
    """Estimate the width of a label in data coordinates."""
    # Rough estimate: each character is about fontsize/72 inches wide
    # Convert to data coordinates using the axes transformation
    char_width_inches = fontsize / 72.0 * 0.6  # 0.6 is a rough character width factor
    fig_width_inches = ax.figure.get_figwidth()
    x_range_data = ax.get_xlim()[1] - ax.get_xlim()[0]
    
    # Convert inches to data coordinates
    char_width_data = char_width_inches * x_range_data / fig_width_inches
    return len(label) * char_width_data


bits = (1, 2, 3, 4, 5, 6)          # number of bits per line (top->bottom)
x_range = (0.0, 1.0)               # domain, closed at both ends
top = 0.85                         # y-position of the top line
bottom = 0.15                      # y-position of the bottom line
label_offset = 0.025               # vertical offset for labels above each line
tick_half_height = 0.055           # half height of division markers
line_lw = 3
tick_lw = 2
fontsize = 30
figsize = None
split_ratio = 2/3                  # A/B split ratio (A gets 2/3, B gets 1/3)
min_segment_width = 0.08           # Minimum segment width to show label (adjustable)
label_padding_factor = 1.2         # Extra space factor for label comfort

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
    # Generate segments and labels for this level
    segments, labels = generate_segments_and_labels(level, split_ratio)
    
    # Main horizontal line
    ax.hlines(y, x0, x1, colors='black', linewidth=line_lw, zorder=2)

    # Get all unique division points (including boundaries)
    division_points = set([x0, x1])
    for a, b in segments:
        division_points.update([a, b])
    
    division_points = sorted(division_points)
    
    # Division markers
    ax.vlines(
        division_points, y - tick_half_height, y + tick_half_height, 
        colors='black', linewidth=tick_lw,
        clip_on=False, zorder=3
    )

    # A/B labels at segment centers (dynamic visibility based on segment size)
    for (a, b), label in zip(segments, labels):
        if label:  # Skip empty label for level 0
            segment_width = b - a
            
            # Estimate how much space the label needs
            label_width = estimate_label_width(label, fontsize, ax)
            required_width = label_width * label_padding_factor
            
            # Show label if segment is wide enough OR if it meets minimum width threshold
            if segment_width >= max(min_segment_width, required_width):
                center = (a + b) / 2.0
                ax.text(center, y + label_offset, label, ha='center', va='bottom', 
                       fontsize=fontsize, zorder=4)

# Formatting: 0..1 on x, no ticks/labels/frames
ax.set_xlim(x0, x1)
ax.set_ylim(0, 1)
ax.set_xticks([])
ax.set_yticks([])
for spine in ax.spines.values():
    spine.set_visible(False)

plt.tight_layout()
plt.savefig('ab.svg')
plt.show()

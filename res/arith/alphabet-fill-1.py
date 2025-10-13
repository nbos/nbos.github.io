import numpy as np
import matplotlib.pyplot as plt
import itertools

def generate_alphabet_powers(alphabet, power):
    """
    Generate all strings of given power from alphabet and their probabilities.
    
    alphabet: list of (symbol, probability) tuples
    power: integer power (length of strings)
    
    Returns: list of (string, probability) tuples sorted by cumulative probability
    """
    symbols, probs = zip(*alphabet)
    
    # Generate all combinations of length 'power'
    combinations = list(itertools.product(symbols, repeat=power))
    
    result = []
    for combo in combinations:
        string = ''.join(combo)
        # Probability is product of individual symbol probabilities
        prob = 1.0
        for symbol in combo:
            symbol_idx = symbols.index(symbol)
            prob *= probs[symbol_idx]
        result.append((string, prob))
    
    # Sort by the order they appear when iterating through combinations
    # (this gives a natural lexicographic-like ordering)
    return result

# ----------------- Alphabet and Power Definitions -----------------
alphabet_A = [('0', 0.5), ('1', 0.5)]
powers_A = [1,2,3,4]  # which powers to show for group A

p = 1/32
alphabet_B = [
    ('A', 2*p), ('B', p), ('C', p), ('D', p), ('E', 2*p), 
    ('F', p), ('G', p), ('H', p), ('I', 2*p), ('J', p),
    ('K', p), ('L', p), ('M', p), ('N', 2*p), ('O', 2*p), 
    ('P', p), ('Q', p), ('R', p), ('S', p), ('T', 2*p),
    ('U', p), ('V', p), ('W', p), ('X', p), ('Y', p), 
    ('Z', p), (' ', p), (' ', p), (' ', p), (' ', p),
    (' ', p), (' ', p)
]
powers_B = [1]  # which powers to show for group B

# ----------------- Zoom Parameters -----------------
zoom_start = 0.5  # start of zoom range [0, 1]
zoom_end = 1.0    # end of zoom range [0, 1]

# ----------------- General Parameters -----------------
x_range = (zoom_start, zoom_end)  # displayed domain
figsize = None

# ----------------- Layout Parameters -----------------
margin_top = 0.15        # distance from top of plot to topmost line
margin_bottom = 0.10     # distance from bottom of plot to bottommost line/labels
margin_between_groups = 0.05  # distance between bottom of group A and top of group B
line_spacing_A = 0.20    # vertical distance between lines in group A
line_spacing_B = 0.20    # vertical distance between lines in group B

# ----------------- Group A Parameters (labels above lines) -----------------
group_A_line_lw = 3
group_A_tick_lw = 2
group_A_color = 'black'
group_A_fontsize = 30
group_A_label_offset = 0.04      # vertical offset for labels from each line
group_A_tick_half_height = 0.035 # half height of division markers
group_A_min_label_width_frac = 0.1  # Only show labels if segment width >= this fraction

# ----------------- Group B Parameters (labels below lines) -----------------
group_B_line_lw = 3
group_B_tick_lw = 2
group_B_color = 'darkblue'
group_B_fontsize = 30
group_B_label_offset = 0.04      # vertical offset for labels from each line
group_B_tick_half_height = 0.035 # half height of division markers
group_B_min_label_width_frac = 0.03  # Only show labels if segment width >= this fraction

# ----------------- Figure size calculation -----------------
n_lines_A = len(powers_A)
n_lines_B = len(powers_B)
n_total_lines = n_lines_A + n_lines_B

if figsize is None:
    figsize = (12, max(3.0, 0.6 * n_total_lines + 1.5))

fig, ax = plt.subplots(figsize=figsize)

# ----------------- Y positions -----------------
x0, x1 = x_range
x_len = x1 - x0
zoom_scale = 1.0 / (zoom_end - zoom_start)

# Group A lines: start from top and space downward
y_top = 1.0 - margin_top
if n_lines_A == 1:
    y_positions_A = [y_top]
else:
    y_positions_A = [y_top - i * line_spacing_A for i in range(n_lines_A)]

# Group B lines: start below group A with margin_between_groups
if n_lines_A > 0:
    y_start_B = y_positions_A[-1] - margin_between_groups
else:
    y_start_B = y_top - margin_between_groups

if n_lines_B == 1:
    y_positions_B = [y_start_B]
else:
    y_positions_B = [y_start_B - i * line_spacing_B for i in range(n_lines_B)]

def get_visible_segments(cdf_edges, strings_probs, zoom_start, zoom_end):
    """
    Return segments and ticks that are visible in the zoom range.
    """
    visible_segments = []
    visible_ticks = []
    
    for i, (string, prob) in enumerate(strings_probs):
        seg_start = cdf_edges[i]
        seg_end = cdf_edges[i + 1]
        
        # Check if segment overlaps with zoom range
        if seg_end > zoom_start and seg_start < zoom_end:
            # Calculate visible portion of segment
            vis_start = max(seg_start, zoom_start)
            vis_end = min(seg_end, zoom_end)
            
            # Map to display coordinates
            display_start = x0 + (vis_start - zoom_start) / (zoom_end - zoom_start) * x_len
            display_end = x0 + (vis_end - zoom_start) / (zoom_end - zoom_start) * x_len
            
            visible_segments.append({
                'string': string,
                'original_start': seg_start,
                'original_end': seg_end,
                'display_start': display_start,
                'display_end': display_end,
                'display_width': display_end - display_start,
                'is_complete': (seg_start >= zoom_start and seg_end <= zoom_end)
            })
    
    # Calculate visible ticks
    for i, edge in enumerate(cdf_edges):
        if zoom_start <= edge <= zoom_end:
            display_x = x0 + (edge - zoom_start) / (zoom_end - zoom_start) * x_len
            visible_ticks.append(display_x)
    
    return visible_segments, visible_ticks

# ----------------- Draw Group A lines -----------------
for line_idx, (power, y) in enumerate(zip(powers_A, y_positions_A)):
    is_last_line_A = (line_idx == len(powers_A) - 1)
    
    strings_probs = generate_alphabet_powers(alphabet_A, power)
    strings, probs = zip(*strings_probs)
    probs = np.array(probs)
    
    # Normalize probabilities (defensive)
    if not np.isclose(probs.sum(), 1.0):
        probs = probs / probs.sum()
    
    # Calculate original CDF edges
    cdf_edges = np.concatenate(([0.0], np.cumsum(probs)))
    
    # Get visible segments and ticks
    visible_segments, visible_ticks = get_visible_segments(cdf_edges, strings_probs, zoom_start, zoom_end)
    
    # Main horizontal line
    lc_main = ax.hlines(y, x0, x1, colors=group_A_color, linewidth=group_A_line_lw, zorder=2)
    try:
        lc_main.set_capstyle('butt')
    except Exception:
        pass
    
    # Division markers
    if visible_ticks:
        if is_last_line_A:
            # Last line of group A: ticks only extend above the line
            tick_bottom = y
            tick_top = y + group_A_tick_half_height
        else:
            # Regular ticks extend both above and below
            tick_bottom = y - group_A_tick_half_height
            tick_top = y + group_A_tick_half_height
            
        lc_ticks = ax.vlines(
            visible_ticks, tick_bottom, tick_top,
            colors=group_A_color, linewidth=group_A_tick_lw,
            clip_on=False, zorder=3
        )
        try:
            lc_ticks.set_capstyle('butt')
        except Exception:
            pass
    
    # Labels above the line (only if segment is wide enough and complete)
    min_width = group_A_min_label_width_frac * x_len
    for seg in visible_segments:
        # Only show label if segment is complete and wide enough in display coordinates
        if seg['is_complete'] and seg['display_width'] >= min_width:
            x_center = (seg['display_start'] + seg['display_end']) / 2.0
            ax.text(x_center, y + group_A_label_offset, seg['string'], 
                   ha='center', va='bottom', fontsize=group_A_fontsize, 
                   color=group_A_color, zorder=4)

# ----------------- Draw Group B lines -----------------
for line_idx, (power, y) in enumerate(zip(powers_B, y_positions_B)):
    is_first_line_B = (line_idx == 0)
    
    strings_probs = generate_alphabet_powers(alphabet_B, power)
    strings, probs = zip(*strings_probs)
    probs = np.array(probs)
    
    # # Normalize probabilities (defensive)
    # if not np.isclose(probs.sum(), 1.0):
    #     probs = probs / probs.sum()
    
    # Calculate original CDF edges
    cdf_edges = np.concatenate(([0.0], np.cumsum(probs)))
    
    # Get visible segments and ticks
    visible_segments, visible_ticks = get_visible_segments(cdf_edges, strings_probs, zoom_start, zoom_end)
    
    # Main horizontal line
    lc_main = ax.hlines(y, x0, x1, colors=group_B_color, linewidth=group_B_line_lw, zorder=2)
    try:
        lc_main.set_capstyle('butt')
    except Exception:
        pass
    
    # Division markers
    if visible_ticks:
        if is_first_line_B:
            # First line of group B: ticks only extend below the line
            tick_bottom = y - group_B_tick_half_height
            tick_top = y
        else:
            # Regular ticks extend both above and below
            tick_bottom = y - group_B_tick_half_height
            tick_top = y + group_B_tick_half_height
            
        lc_ticks = ax.vlines(
            visible_ticks, tick_bottom, tick_top,
            colors=group_B_color, linewidth=group_B_tick_lw,
            clip_on=False, zorder=3
        )
        try:
            lc_ticks.set_capstyle('butt')
        except Exception:
            pass
    
    # Labels below the line (only if segment is wide enough and complete)
    min_width = group_B_min_label_width_frac * x_len
    for seg in visible_segments:
        # Only show label if segment is complete and wide enough in display coordinates
        if seg['is_complete'] and seg['display_width'] >= min_width:
            x_center = (seg['display_start'] + seg['display_end']) / 2.0
            ax.text(x_center, y - group_B_label_offset, seg['string'], 
                   ha='center', va='top', fontsize=group_B_fontsize, 
                   color=group_B_color, zorder=4)

# ----------------- Axes formatting -----------------
ax.set_xlim(x0, x1)

# Calculate y limits
y_max = 1.0
if n_lines_B > 0:
    # Account for labels below the lowest line in group B
    y_min = y_positions_B[-1] - group_B_label_offset - margin_bottom
else:
    y_min = y_positions_A[-1] - margin_bottom

ax.set_ylim(y_min, y_max)

ax.set_xticks([])
ax.set_yticks([])
for spine in ax.spines.values():
    spine.set_visible(False)

plt.tight_layout()
plt.savefig('alphabet-fill-1.svg')
# plt.show()

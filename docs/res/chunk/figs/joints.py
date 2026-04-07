import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from itertools import combinations

# ═══════════════════════════════════════════════════════════════════
# 1.  PARAMETERS
# ═══════════════════════════════════════════════════════════════════
N_BOXES = 10

groupings = [
    frozenset({0,1}),
    frozenset({2,3,4,5}),
    frozenset({2,3}),
    frozenset({4,5}),
    frozenset({6,7,8}),
    frozenset({6,7}),
    # frozenset({8,9}),
    # frozenset({1,2}),
    # frozenset({3,4}),
    # frozenset({5,6}),
    # frozenset({7,8}),
]

box_w = 0.5
box_h = 0.5
gap_v = 0.18   # vertical distance between every consecutive pair of horizontal lines
gap_h = 0.22   # horizontal distance between every consecutive pair of vertical lines
LW    = 3.0
CR    = 0.07   # corner radius (purely cosmetic, small enough to never affect spacing)

# ═══════════════════════════════════════════════════════════════════
# 2.  LEVEL ASSIGNMENT
#     level(g) = number of groups strictly contained IN g
#       → leaf groups  : level 0  (closest ring to boxes)
#       → parent groups: level > 0 (further rings)
#     Intersecting groups at equal levels are bumped until distinct.
# ═══════════════════════════════════════════════════════════════════
def assign_levels(gs):
    groups = list(gs)

    def depth(g):
        children = [h for h in groups if h != g and h < g]
        return (1 + max(depth(h) for h in children)) if children else 0

    # First pass: assign levels to non-nested groups only
    level = {g: 0 for g in groups}

    # Bump intersecting groups until all intersecting pairs have distinct levels
    changed = True
    while changed:
        changed = False
        for a, b in combinations(groups, 2):
            if a & b and not (a < b) and not (b < a):
                if level[a] == level[b]:
                    bump = b if groups.index(b) >= groups.index(a) else a
                    level[bump] += 1
                    changed = True

    # Second pass: assign parent levels based on settled child levels
    changed = True
    while changed:
        changed = False
        for g in groups:
            children = [h for h in groups if h != g and h < g]
            if children:
                new_level = 1 + max(level[h] for h in children)
                if new_level != level[g]:
                    level[g] = new_level
                    changed = True
    return level

levels    = assign_levels(groupings)
max_level = max(levels.values()) if levels else -1

# ═══════════════════════════════════════════════════════════════════
# 3.  VERTICAL GEOMETRY  — rock solid
#
#     We have these horizontal lines from bottom to top:
#       bot of outermost ring  : y = bot_y - r(max_level)
#       ...
#       bot of ring 0          : y = bot_y - r(0)
#       bottom of square boxes : y = bot_y               ← box edge
#       top  of square boxes   : y = bot_y + box_h       ← box edge
#       top  of ring 0         : y = bot_y + box_h + r(0)
#       ...
#       top  of outermost ring : y = bot_y + box_h + r(max_level)
#
#     Equal spacing means r(k) = (k+1) * gap_v, giving:
#       box_edge  → ring-0   : gap_v
#       ring-k    → ring-k+1 : gap_v   ✓
# ═══════════════════════════════════════════════════════════════════
def ring_r(k):
    return (k + 1) * gap_v

bot_y = 0.0
top_y = bot_y + box_h

# ═══════════════════════════════════════════════════════════════════
# 4.  HORIZONTAL GEOMETRY  — rock solid
#
#     All vertical lines in the figure, left to right:
#
#       For each gap between box i and box i+1:
#         [right edge of box i]  ← NOT a free line, it's the box wall
#         group boundary lines ordered as:
#           · ends  ascending level  (innermost closes first)
#           · starts descending level (outermost opens first)
#         [left edge of box i+1] ← NOT a free line, it's the box wall
#
#       For each outer edge (left of box 0, right of box N-1):
#         group boundaries that start/end there get ONE slot of gap_h
#         outside the box edge.
#
#     Every consecutive pair of free vertical lines is gap_h apart.
#     Box walls are NOT free lines — they're fixed at box_x and box_x+box_w.
#     The distance from box wall to first/last free line in a gap = gap_h.
# ═══════════════════════════════════════════════════════════════════

# --- 4a. Collect and sort events per gap ---
def group_span(g):
    return min(g), max(g)

# "outer" events: boundaries that fall outside the box array
# gap index -1  = left  of box 0   (groups starting at box 0)
# gap index N-1 = right of box N-1 (groups ending   at box N-1)
gap_events = {i: [] for i in range(-1, N_BOXES)}  # -1 and N-1 are outer gaps

for g, lv in levels.items():
    lo, hi = group_span(g)
    # left boundary: in gap (lo-1) if lo>0, else in the outer-left gap (-1 … box0)
    gap_events[lo - 1].append(("start", lv, g))
    # right boundary: in gap hi if hi<N-1, else in the outer-right gap
    gap_events[hi].append(("end", lv, g))

for i in gap_events:
    ends   = sorted([e for e in gap_events[i] if e[0] == "end"],
                    key=lambda e:  e[1])   # ascending level (inner first)
    starts = sorted([e for e in gap_events[i] if e[0] == "start"],
                    key=lambda e: -e[1])   # descending level (outer first)
    gap_events[i] = ends + starts

# --- 4b. Walk left to right, placing everything ---
# The outer-left gap: starts are to the LEFT of box 0.
# They are ordered right-to-left as: closest (lowest level) nearest box.
# outer-left events are all "start"; we place them so the rightmost is
# gap_h to the left of box 0's left edge.

box_x         = [0.0] * N_BOXES
group_left_x  = {}
group_right_x = {}

# --- outer left (gap index -1): only "start" events ---
outer_left_events = gap_events[-1]   # all should be "start"
# rightmost start is gap_h left of box 0; each further one is another gap_h left
# order: descending level means outermost is leftmost  →  index 0 is outermost
# We need: closest to box = last in list = highest index → x = box0 - gap_h
#          next             second-to-last               → x = box0 - 2*gap_h
n_ol = len(outer_left_events)
box_x[0] = 0.0
for k, (side, lv, g) in enumerate(outer_left_events):
    # k=0 is outermost start (descending level sort), placed furthest left
    x_line = box_x[0] - (n_ol - k) * gap_h
    group_left_x[id(g)] = x_line

# --- inner gaps (gap indices 0 .. N-2) ---
for i in range(N_BOXES - 1):
    events    = gap_events[i]
    n_lines   = len(events)
    gap_width = (n_lines + 1) * gap_h   # box_wall … line1 … lineN … box_wall

    x_after = box_x[i] + box_w
    for k, (side, lv, g) in enumerate(events):
        x_line = x_after + (k + 1) * gap_h
        if side == "end":
            group_right_x[id(g)] = x_line
        else:
            group_left_x[id(g)]  = x_line

    box_x[i + 1] = x_after + gap_width

# --- outer right (gap index N-1): only "end" events ---
outer_right_events = gap_events[N_BOXES - 1]   # all should be "end"
# order: ascending level means innermost is first → x = box_last_right + gap_h
for k, (side, lv, g) in enumerate(outer_right_events):
    x_line = box_x[-1] + box_w + (k + 1) * gap_h
    group_right_x[id(g)] = x_line

total_w = box_x[-1] + box_w

# ═══════════════════════════════════════════════════════════════════
# 5.  FIGURE SETUP
# ═══════════════════════════════════════════════════════════════════
# Horizontal extent: include outer group boundary lines + margin
if outer_left_events:
    x_min = box_x[0] - n_ol * gap_h
else:
    x_min = box_x[0]

n_or = len(outer_right_events)
if outer_right_events:
    x_max = box_x[-1] + box_w + n_or * gap_h
else:
    x_max = box_x[-1] + box_w

margin_x = 0.35
margin_y = 0.30
extra_v  = ring_r(max_level) + margin_y if max_level >= 0 else margin_y

scale = 1.7
fig_w = (x_max - x_min + 2 * margin_x) * scale
fig_h = (box_h + 2 * extra_v)          * scale

fig, ax = plt.subplots(figsize=(fig_w, fig_h))
ax.set_xlim(x_min - margin_x,   x_max + margin_x)
ax.set_ylim(bot_y - extra_v,    top_y + extra_v)
ax.set_aspect("equal")
ax.axis("off")
fig.patch.set_facecolor("white")

# ═══════════════════════════════════════════════════════════════════
# 6.  DRAWING
#
#     Group borders are drawn as explicit polylines (4 corners + close)
#     with rounded corners via FancyBboxPatch — but we pass the EXACT
#     outer boundary as the rectangle and use pad=0, then apply corner
#     rounding by shrinking the rect by CR and padding back by CR.
#     This guarantees the outer extent is exactly what we computed.
# ═══════════════════════════════════════════════════════════════════
def draw_group_rect(xl, yt, xr, yb, zorder):
    """
    Draw a rounded rectangle with exact outer boundary
    (xl, yb) bottom-left to (xr, yt) top-right.
    """
    w = xr - xl
    h = yt - yb
    # FancyBboxPatch(xy, width, height, boxstyle="round,pad=p")
    # draws a rect from xy to xy+(w,h) then expands by p with rounded corners.
    # To get exact outer boundary at (xl,yb)+(w,h), pass inner rect shrunk by CR.
    inner_x = xl + CR
    inner_y = yb + CR
    inner_w = w - 2 * CR
    inner_h = h - 2 * CR
    if inner_w > 0 and inner_h > 0:
        rect = patches.FancyBboxPatch(
            (inner_x, inner_y), inner_w, inner_h,
            boxstyle=f"round,pad={CR:.6f}",
            linewidth=LW, edgecolor="black", facecolor="none", zorder=zorder,
        )
    else:
        rect = patches.Rectangle(
            (xl, yb), w, h,
            linewidth=LW, edgecolor="black", facecolor="none", zorder=zorder,
        )
    ax.add_patch(rect)

# Draw outermost groups first so inner ones paint over them
for g, lv in sorted(levels.items(), key=lambda kv: -kv[1]):
    gid = id(g)
    r   = ring_r(lv)
    xl  = group_left_x[gid]
    xr  = group_right_x[gid]
    yt  = top_y + r
    yb  = bot_y - r
    draw_group_rect(xl, yt, xr, yb, zorder=3 + lv)

# Square boxes on top with white fill
for xp in box_x:
    rect = patches.Rectangle(
        (xp, bot_y), box_w, box_h,
        linewidth=LW, edgecolor="black", facecolor="white", zorder=8,
    )
    ax.add_patch(rect)

# ═══════════════════════════════════════════════════════════════════
# 7.  SAVE
# ═══════════════════════════════════════════════════════════════════
fig.savefig(
    "joints.svg",
    format="svg",
    bbox_inches="tight",
    facecolor="white",
    edgecolor="none",
)
plt.close(fig)
print("Saved joints.svg")

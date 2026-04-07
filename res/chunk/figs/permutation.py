"""Generate a multiset-permutation diagram → permutation.svg"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyArrowPatch
from matplotlib.path import Path
import numpy as np

# ── Parameters ──────────────────────────────────────────────────
groups      = [1, 3, 2, 1]              # multiset multiplicities
group_chars = ["a", "b", "c", "d"]      # one symbol per group
box_w       = 1.0                       # box width / height
box_h       = 1.0
gap_inner   = 0.12                      # spacing inside a group
gap_outer   = 1.00                      # spacing between groups
top_y       = 3.5                       # y of top-row boxes
bot_y       = 0.0                       # y of bottom-row boxes

# A hand-picked bijection that produces many visible crossings
perm = [4, 0, 5, 6, 1, 2, 3]

n = sum(groups)                         # total boxes = 7
assert len(perm) == n and sorted(perm) == list(range(n))

# ── Per-box labels (e.g. a, b, b, b, c, c, d) ──────────────────
labels = []
for gi, g in enumerate(groups):
    labels.extend([group_chars[gi]] * g)

# ── Top-row x positions (left edges) ────────────────────────────
top_x: list[float] = []
x = 0.0
for gi, g in enumerate(groups):
    for i in range(g):
        top_x.append(x)
        x += box_w
        if i < g - 1:
            x += gap_inner
    if gi < len(groups) - 1:
        x += gap_outer

total_w = top_x[-1] + box_w            # right edge of last top box

# ── Bottom-row x positions (uniform spacing, same total width) ──
gap_bot = (total_w - n * box_w) / (n - 1)
bot_x   = [i * (box_w + gap_bot) for i in range(n)]

# Where each top element lands in the bottom row
bot_labels = [""] * n
for i in range(n):
    bot_labels[perm[i]] = labels[i]

# ── Figure setup ────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(11, 7.5))
ax.set_xlim(-0.3, total_w + 0.3)
ax.set_ylim(-0.3, top_y + box_h + 0.3)
ax.set_aspect("equal")
ax.axis("off")
fig.patch.set_facecolor("white")

# ── Helper: draw one labelled box ───────────────────────────────
def draw_box(x: float, y: float, label: str) -> None:
    rect = patches.Rectangle(
        (x, y), box_w, box_h,
        linewidth=2, edgecolor="black", facecolor="white", zorder=3,
    )
    ax.add_patch(rect)
    ax.text(
        x + box_w / 2, y + box_h / 2, label,
        ha="center", va="center",
        fontsize=20, fontfamily="serif", fontstyle="italic", zorder=4,
    )

# ── Draw top-row boxes ──────────────────────────────────────────
for i, xp in enumerate(top_x):
    draw_box(xp, top_y, labels[i])

# ── Draw group braces above the top row ─────────────────────────
brace_y = top_y + box_h + 0.15         # just above the boxes
for gi, g in enumerate(groups):
    # find the first and last box index of this group
    start = sum(groups[:gi])
    end   = start + g - 1
    x_left  = top_x[start]
    x_right = top_x[end] + box_w
    mid_x   = (x_left + x_right) / 2

    # if g > 1:
    #     # horizontal line with small ticks
    #     ax.plot([x_left, x_right], [brace_y, brace_y],
    #             color="black", lw=1.0, solid_capstyle="butt", zorder=2)
    #     tick = 0.08
    #     ax.plot([x_left, x_left],   [brace_y - tick, brace_y + tick],
    #             color="black", lw=1.0, zorder=2)
    #     ax.plot([x_right, x_right], [brace_y - tick, brace_y + tick],
    #             color="black", lw=1.0, zorder=2)

    # # multiplicity label above the brace
    # ax.text(mid_x, brace_y + 0.22, str(g),
    #         ha="center", va="bottom",
    #         fontsize=14, fontfamily="serif", color="black", zorder=4)

# ── Draw bottom-row boxes ───────────────────────────────────────
for i, xp in enumerate(bot_x):
    draw_box(xp, bot_y, bot_labels[i])

# ── S-curve arrows from every top box → its permuted bottom box ─
for i in range(n):
    x0 = top_x[i]        + box_w / 2   # centre of top box
    y0 = top_y                          # bottom edge of top box
    x1 = bot_x[perm[i]]  + box_w / 2   # centre of target bottom box
    y1 = bot_y + box_h                  # top edge of bottom box

    # cubic Bézier giving a smooth S-shape
    mid = (y0 + y1) / 2
    path = Path(
        [(x0, y0), (x0, mid), (x1, mid), (x1, y1)],
        [Path.MOVETO, Path.CURVE4, Path.CURVE4, Path.CURVE4],
    )
    arrow = FancyArrowPatch(
        path=path,
        arrowstyle="-|>",
        mutation_scale=16,
        linewidth=2,
        color="black",
        zorder=2,
    )
    ax.add_patch(arrow)

# ── Save ────────────────────────────────────────────────────────
fig.savefig(
    "permutation.svg",
    format="svg",
    bbox_inches="tight",
    facecolor="white",
    edgecolor="none",
)
plt.close(fig)
print("Saved permutation.svg")

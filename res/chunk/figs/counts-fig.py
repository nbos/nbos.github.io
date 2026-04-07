import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# ── Parameters ──────────────────────────────────────────────────
groups      = [1, 3, 2, 1]
group_chars = ["a", "b", "c", "d"]
box_w       = 1.0
box_h       = 1.0
gap_inner   = 0.12
gap_outer   = 1.00
top_y       = 0.0

# ── Per-box labels ───────────────────────────────────────────────
labels = []
for gi, g in enumerate(groups):
    labels.extend([group_chars[gi]] * g)

# ── Top-row x positions ──────────────────────────────────────────
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

total_w = top_x[-1] + box_w

# ── Figure setup ─────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(11, 1.6))
ax.set_xlim(-0.3, total_w + 0.3)
ax.set_ylim(-0.3, box_h + 0.3)
ax.set_aspect("equal")
ax.axis("off")
fig.patch.set_facecolor("white")

# ── Helper: draw one labelled box ────────────────────────────────
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

# ── Draw top-row boxes ────────────────────────────────────────────
for i, xp in enumerate(top_x):
    draw_box(xp, top_y, labels[i])

# ── Save ──────────────────────────────────────────────────────────
fig.savefig(
    "counts-fig.svg",
    format="svg",
    bbox_inches="tight",
    facecolor="white",
    edgecolor="none",
)
plt.close(fig)
print("Saved counts-fig.svg")

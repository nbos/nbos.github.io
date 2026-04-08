import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from itertools import combinations

# ═══════════════════════════════════════════════════════════════════
# INTERNAL HELPERS
# ═══════════════════════════════════════════════════════════════════

def _assign_levels(gs):
    groups = list(gs)

    level = {g: 0 for g in groups}

    changed = True
    while changed:
        changed = False
        for a, b in combinations(groups, 2):
            if a & b and not (a < b) and not (b < a):
                if level[a] == level[b]:
                    bump = b if groups.index(b) >= groups.index(a) else a
                    level[bump] += 1
                    changed = True

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


def _ring_r(k, gap_v):
    return (k + 1) * gap_v


def _group_span(g):
    return min(g), max(g)


def _draw_group_rect(ax, xl, yt, xr, yb, zorder, lw, cr):
    w = xr - xl
    h = yt - yb
    inner_x = xl + cr
    inner_y = yb + cr
    inner_w = w - 2 * cr
    inner_h = h - 2 * cr
    if inner_w > 0 and inner_h > 0:
        rect = patches.FancyBboxPatch(
            (inner_x, inner_y), inner_w, inner_h,
            boxstyle=f"round,pad={cr:.6f}",
            linewidth=lw, edgecolor="black", facecolor="none", zorder=zorder,
        )
    else:
        rect = patches.Rectangle(
            (xl, yb), w, h,
            linewidth=lw, edgecolor="black", facecolor="none", zorder=zorder,
        )
    ax.add_patch(rect)


# ═══════════════════════════════════════════════════════════════════
# PUBLIC API
# ═══════════════════════════════════════════════════════════════════

def draw_joints(
    n_boxes,
    groupings,
    output_path,
    *,
    box_w=0.5,
    box_h=0.5,
    gap_v=0.18,
    gap_h=0.22,
    lw=3.0,
    cr=0.07,
    margin_x=0.35,
    margin_y=0.30,
    scale=1.7,
):
    """
    Render a box-groupings diagram and save it to *output_path*.

    Parameters
    ----------
    n_boxes      : int            – total number of boxes
    groupings    : list[frozenset]– each frozenset is a group of box indices
    output_path  : str            – destination file path (format inferred from extension)
    box_w/box_h  : float          – width / height of each box
    gap_v        : float          – vertical spacing between ring levels
    gap_h        : float          – horizontal spacing between vertical boundary lines
    lw           : float          – line width
    cr           : float          – corner radius
    margin_x/y   : float          – figure margins
    scale        : float          – inches-per-data-unit scaling factor
    """

    # ── 1. Level assignment ──────────────────────────────────────────
    levels    = _assign_levels(groupings)
    max_level = max(levels.values()) if levels else -1

    # ── 2. Vertical geometry ─────────────────────────────────────────
    bot_y = 0.0
    top_y = bot_y + box_h

    # ── 3. Collect and sort gap events ───────────────────────────────
    gap_events = {i: [] for i in range(-1, n_boxes)}

    for g, lv in levels.items():
        lo, hi = _group_span(g)
        gap_events[lo - 1].append(("start", lv, g))
        gap_events[hi].append(("end", lv, g))

    for i in gap_events:
        ends   = sorted([e for e in gap_events[i] if e[0] == "end"],
                        key=lambda e:  e[1])
        starts = sorted([e for e in gap_events[i] if e[0] == "start"],
                        key=lambda e: -e[1])
        gap_events[i] = ends + starts

    # ── 4. Horizontal geometry ───────────────────────────────────────
    box_x         = [0.0] * n_boxes
    group_left_x  = {}
    group_right_x = {}

    outer_left_events = gap_events[-1]
    n_ol = len(outer_left_events)
    box_x[0] = 0.0
    for k, (side, lv, g) in enumerate(outer_left_events):
        group_left_x[id(g)] = box_x[0] - (n_ol - k) * gap_h

    for i in range(n_boxes - 1):
        events    = gap_events[i]
        n_lines   = len(events)
        gap_width = (n_lines + 1) * gap_h
        x_after   = box_x[i] + box_w
        for k, (side, lv, g) in enumerate(events):
            x_line = x_after + (k + 1) * gap_h
            if side == "end":
                group_right_x[id(g)] = x_line
            else:
                group_left_x[id(g)]  = x_line
        box_x[i + 1] = x_after + gap_width

    outer_right_events = gap_events[n_boxes - 1]
    for k, (side, lv, g) in enumerate(outer_right_events):
        group_right_x[id(g)] = box_x[-1] + box_w + (k + 1) * gap_h

    # ── 5. Figure extents ────────────────────────────────────────────
    x_min = box_x[0] - n_ol * gap_h if outer_left_events else box_x[0]
    n_or  = len(outer_right_events)
    x_max = box_x[-1] + box_w + n_or * gap_h if outer_right_events else box_x[-1] + box_w

    extra_v = _ring_r(max_level, gap_v) + margin_y if max_level >= 0 else margin_y

    fig_w = (x_max - x_min + 2 * margin_x) * scale
    fig_h = (box_h + 2 * extra_v)          * scale

    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    ax.set_xlim(x_min - margin_x, x_max + margin_x)
    ax.set_ylim(bot_y - extra_v,  top_y + extra_v)
    ax.set_aspect("equal")
    ax.axis("off")
    fig.patch.set_facecolor("white")

    # ── 6. Draw groups then boxes ────────────────────────────────────
    for g, lv in sorted(levels.items(), key=lambda kv: -kv[1]):
        gid = id(g)
        r   = _ring_r(lv, gap_v)
        _draw_group_rect(
            ax,
            xl=group_left_x[gid],
            yt=top_y + r,
            xr=group_right_x[gid],
            yb=bot_y - r,
            zorder=3 + lv,
            lw=lw,
            cr=cr,
        )

    for xp in box_x:
        ax.add_patch(patches.Rectangle(
            (xp, bot_y), box_w, box_h,
            linewidth=lw, edgecolor="black", facecolor="white", zorder=8,
        ))

    # ── 7. Save ──────────────────────────────────────────────────────
    fmt = output_path.rsplit(".", 1)[-1].lower() if "." in output_path else "svg"
    fig.savefig(output_path, format=fmt, bbox_inches="tight",
                facecolor="white", edgecolor="none")
    plt.close(fig)
    print(f"Saved {output_path}")


# ═══════════════════════════════════════════════════════════════════
# DIRECT EXECUTION  – reproduces the original one-shot behaviour
# ═══════════════════════════════════════════════════════════════════
if __name__ == "__main__":
    draw_joints(
        n_boxes=10,
        groupings=[
            frozenset({0, 1}),
            frozenset({2, 3, 4, 5}),
            frozenset({2, 3}),
            frozenset({4, 5}),
            frozenset({6, 7, 8}),
            frozenset({6, 7}),
        ],
        output_path="joints.svg",
    )

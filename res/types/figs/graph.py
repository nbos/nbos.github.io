"""graph.py – reusable directed-graph diagram generator

Public API
----------
draw_graph(
    vertex_pos     : dict[str, np.ndarray],
    edges          : list[tuple[str, str]],
    filename       : str,
    vertex_labels  : dict[str, str] | None = None,
    **kwargs                                        # override any tunable
) -> None
"""

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch
from matplotlib.path import Path
import numpy as np
from collections import defaultdict


# ── Default tuneable parameters ──────────────────────────────────
_DEFAULTS = dict(
    MARGIN          = 0.15,             # SVG padding (inches)
    VR              = 0.50,             # vertex-circle radius
    EDGE_GAP        = np.radians(20),   # angular spacing between parallel edges
    BULGE           = 0.30,             # perpendicular spread of parallel Bézier edges
    LOOP_RADIUS     = 0.55,             # radius of the self-loop arc circle
    LOOP_ANG_SPREAD = np.radians(50),   # half-angle of the loop arc on the vertex circle
    LOOP_GAP        = np.radians(28),   # angular step between multiple self-loops
    FIG_SIZE        = 7,                # reference figure size in inches (square basis)
    LIM_PAD         = 0.4,              # extra whitespace beyond vertex circles
    FONT_SIZE       = 20,
    LINE_WIDTH      = 1.2,
    ARROW_SCALE     = 14,
)


# ═══════════════════════════════════════════════════════════════════
# Internal helpers
# ═══════════════════════════════════════════════════════════════════

def _arc_cubic_approx(cx, cy, r, a0, a1):
    """
    Return a sequence of cubic Bézier segments that faithfully approximate
    a circular arc from angle *a0* to *a1* (both in radians) on a circle
    centred at (cx, cy) with radius *r*.

    The standard one-segment approximation is accurate to < 0.03 % for
    arcs up to 90 °; larger arcs are split automatically.

    Returns
    -------
    pts   : list of (x, y)  – including the start point
    codes : list of Path codes
    """
    total  = a1 - a0
    n_segs = max(1, int(np.ceil(abs(total) / (np.pi / 2))))
    da     = total / n_segs

    pts   = []
    codes = []

    alpha = (4 / 3) * np.tan(da / 4)

    for i in range(n_segs):
        t0 = a0 + i * da
        t1 = t0 + da

        x0, y0 = cx + r * np.cos(t0), cy + r * np.sin(t0)
        x1, y1 = cx + r * np.cos(t1), cy + r * np.sin(t1)

        tx0, ty0 = -np.sin(t0), np.cos(t0)
        tx1, ty1 = -np.sin(t1), np.cos(t1)

        cp1 = (x0 + alpha * r * tx0, y0 + alpha * r * ty0)
        cp2 = (x1 - alpha * r * tx1, y1 - alpha * r * ty1)

        if i == 0:
            pts.append((x0, y0))
            codes.append(Path.MOVETO)

        pts   += [cp1, cp2, (x1, y1)]
        codes += [Path.CURVE4, Path.CURVE4, Path.CURVE4]

    return pts, codes


def _loop_arc_path(v_pos, VR, ang_out, ang_in):
    """
    Build a Path for a self-loop whose ends leave/arrive perpendicular
    to the vertex circle (radially outward).

    The two attachment points p0, p1 lie on the vertex circle at ang_out
    and ang_in.  Because the arc must leave each point radially, the loop
    circle's centre must lie on both outward radial rays.  Those rays meet
    at a point that can be found in closed form:

        half_spread  = (ang_in - ang_out) / 2
        bisector_ang = (ang_out + ang_in) / 2
        dist         = VR / sin(half_spread)   (vertex centre → loop centre)
        loop_radius  = VR * cos(half_spread) / sin(half_spread)
                     = VR / tan(half_spread)
    """
    mid_ang     = (ang_out + ang_in) / 2.0
    half_spread = abs(ang_in - ang_out) / 2.0

    sin_h = np.sin(half_spread)
    cos_h = np.cos(half_spread)

    dist   = VR / sin_h                     # vertex centre → loop centre
    loop_r = VR * cos_h / sin_h             # = VR / tan(half_spread)

    loop_c = v_pos + dist * np.array([np.cos(mid_ang), np.sin(mid_ang)])

    p0 = v_pos + VR * np.array([np.cos(ang_out), np.sin(ang_out)])
    p1 = v_pos + VR * np.array([np.cos(ang_in),  np.sin(ang_in)])

    a_out = np.arctan2(p0[1] - loop_c[1], p0[0] - loop_c[0])
    a_in  = np.arctan2(p1[1] - loop_c[1], p1[0] - loop_c[0])

    # Sweep the long way around — away from the vertex centre
    far_ang = np.arctan2(loop_c[1] - v_pos[1], loop_c[0] - v_pos[0])

    def _normalise(a, base):
        while a < base:              a += 2 * np.pi
        while a >= base + 2 * np.pi: a -= 2 * np.pi
        return a

    a_in_norm  = _normalise(a_in,   a_out)
    far_norm   = _normalise(far_ang, a_out)

    sweep_end = a_in_norm if far_norm < a_in_norm else a_in_norm - 2 * np.pi

    return _arc_cubic_approx(loop_c[0], loop_c[1], loop_r, a_out, sweep_end)

# ═══════════════════════════════════════════════════════════════════
# Public function
# ═══════════════════════════════════════════════════════════════════

def draw_graph(
    vertex_pos,
    edges,
    filename,
    vertex_labels=None,
    **kwargs,
):
    """
    Render a directed graph and save it to *filename* (SVG by default;
    the format is inferred from the extension).

    Parameters
    ----------
    vertex_pos    : dict mapping vertex name → np.ndarray([x, y])
    edges         : list of (src, dst) vertex-name pairs (duplicates allowed)
    filename      : output path, e.g. "graph.svg" or "out/automaton.png"
    vertex_labels : optional dict mapping vertex name → display string;
                    defaults to the vertex name itself
    **kwargs      : override any key in _DEFAULTS, e.g. VR=0.6, BULGE=0.4
    """
    # ── Merge tuneable parameters ─────────────────────────────────
    P = {**_DEFAULTS, **kwargs}
    VR              = P["VR"]
    MARGIN          = P["MARGIN"]
    EDGE_GAP        = P["EDGE_GAP"]
    BULGE           = P["BULGE"]
    LOOP_RADIUS     = P["LOOP_RADIUS"]
    LOOP_ANG_SPREAD = P["LOOP_ANG_SPREAD"]
    LOOP_GAP        = P["LOOP_GAP"]
    FIG_SIZE        = P["FIG_SIZE"]
    LIM_PAD         = P["LIM_PAD"]
    FONT_SIZE       = P["FONT_SIZE"]
    LINE_WIDTH      = P["LINE_WIDTH"]
    ARROW_SCALE     = P["ARROW_SCALE"]

    if vertex_labels is None:
        vertex_labels = {}

    # ── Group edges ───────────────────────────────────────────────
    pair_edges = defaultdict(list)
    loop_edges = defaultdict(list)

    for ei, (s, d) in enumerate(edges):
        if s == d:
            loop_edges[s].append(ei)
        else:
            canonical = tuple(sorted([s, d]))
            direction = +1 if (s, d) == canonical else -1
            pair_edges[canonical].append((ei, direction))

    # ── Per-edge geometry tables ──────────────────────────────────
    attach_ang   = {}
    bulge_offset = {}
    edge_canon   = {}

    for (u, v), elist in pair_edges.items():
        n      = len(elist)
        diff   = vertex_pos[v] - vertex_pos[u]
        base_u = np.arctan2(diff[1], diff[0])
        base_v = base_u + np.pi

        for i, (ei, _) in enumerate(elist):
            off = (i - (n - 1) / 2) * EDGE_GAP
            attach_ang[(ei, u)] = base_u + off
            attach_ang[(ei, v)] = base_v - off
            bulge_offset[ei]    = (i - (n - 1) / 2) * BULGE
            edge_canon[ei]      = (u, v)

    # ── Loop attachment angles ────────────────────────────────────
    loop_attach = {}

    for v, eidxs in loop_edges.items():
        n        = len(eidxs)
        centroid = np.mean(list(vertex_pos.values()), axis=0)
        outward  = vertex_pos[v] - centroid
        base_dir = np.arctan2(outward[1], outward[0])

        for i, ei in enumerate(eidxs):
            centre_ang      = base_dir + (i - (n - 1) / 2) * LOOP_GAP
            loop_attach[ei] = (centre_ang - LOOP_ANG_SPREAD,
                               centre_ang + LOOP_ANG_SPREAD)

    # ── Geometry helpers ──────────────────────────────────────────
    def pt_on_circle(vertex, ei):
        a = attach_ang[(ei, vertex)]
        c = vertex_pos[vertex]
        return c + VR * np.array([np.cos(a), np.sin(a)])

    def bezier_control(ei):
        u, v   = edge_canon[ei]
        p_u    = pt_on_circle(u, ei)
        p_v    = pt_on_circle(v, ei)
        mid    = (p_u + p_v) * 0.5
        diff   = p_v - p_u
        length = np.linalg.norm(diff)
        if length < 1e-9:
            return mid
        perp = np.array([-diff[1], diff[0]]) / length
        return mid + bulge_offset[ei] * perp

    # ── Figure setup ──────────────────────────────────────────────
    xs    = [p[0] for p in vertex_pos.values()]
    ys    = [p[1] for p in vertex_pos.values()]
    x_pad = VR + LIM_PAD
    y_pad = VR + LIM_PAD
    xlim  = (min(xs) - x_pad, max(xs) + x_pad)
    ylim  = (min(ys) - y_pad, max(ys) + y_pad)

    x_span = xlim[1] - xlim[0]
    y_span = ylim[1] - ylim[0]

    # Scale figure so 1 data-unit = FIG_SIZE/original_span inches,
    # matching the original's (7 inches) / (2*(R + VR + LIM_PAD)) ratio.
    original_span = 2 * (2.5 + 0.5 + 0.4)   # = 6.8, the original's axis span
    scale = FIG_SIZE / original_span          # inches per data-unit
    fig_w = x_span * scale
    fig_h = y_span * scale

    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    ax.set_xlim(*xlim)
    ax.set_ylim(*ylim)
    ax.set_aspect("equal")
    ax.axis("off")
    fig.patch.set_facecolor("white")

    # ── Draw edges ────────────────────────────────────────────────
    for ei, (s, d) in enumerate(edges):

        if s == d:
            ang_out, ang_in = loop_attach[ei]
            pts, codes = _loop_arc_path(
                vertex_pos[s], VR, ang_out, ang_in
            )
            arrow = FancyArrowPatch(
                path=Path(pts, codes),
                arrowstyle="-|>",
                mutation_scale=ARROW_SCALE,
                linewidth=LINE_WIDTH,
                color="black",
                zorder=2,
            )

        else:
            p0 = pt_on_circle(s, ei)
            p1 = pt_on_circle(d, ei)
            cp = bezier_control(ei)
            arrow = FancyArrowPatch(
                path=Path(
                    [p0, cp, p1],
                    [Path.MOVETO, Path.CURVE3, Path.CURVE3],
                ),
                arrowstyle="-|>",
                mutation_scale=ARROW_SCALE,
                linewidth=LINE_WIDTH,
                color="black",
                zorder=2,
            )

        ax.add_patch(arrow)

    # ── Draw vertices ─────────────────────────────────────────────
    for name, p in vertex_pos.items():
        ax.add_patch(plt.Circle(
            p, VR, lw=LINE_WIDTH, ec="black", fc="white", zorder=4
        ))
        label = vertex_labels.get(name, name)
        ax.text(
            *p, label,
            ha="center", va="center",
            fontsize=FONT_SIZE,
            fontfamily="serif",
            fontstyle="italic",
            zorder=5,
        )

    # ── Save ──────────────────────────────────────────────────────
    fmt = filename.rsplit(".", 1)[-1] if "." in filename else "svg"
    fig.savefig(
        filename, format=fmt,
        bbox_inches="tight", pad_inches=MARGIN,
        facecolor="white", edgecolor="none",
    )
    plt.close(fig)
    print(f"Saved {filename}")

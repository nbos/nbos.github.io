"""Generate a directed graph diagram with Eulerian path overlay → graph-path.svg"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch
from matplotlib.path import Path
import matplotlib.patches as mpatches
import numpy as np
from collections import defaultdict

# ── Tuneable parameters ──────────────────────────────────────────
MARGIN          = 0.15
R               = 2.5
VR              = 0.72
EDGE_GAP        = np.radians(20)
BULGE           = 0.30
LOOP_RADIUS     = 0.55
LOOP_ANG_SPREAD = np.radians(50)
LOOP_GAP        = np.radians(28)

PATH_COLOR  = "black"
PATH_LW     = 2.2
PATH_DOTS   = (0, (1.0, 2.5))

# How far the in-vertex arc control point is pulled inward from the
# chord midpoint, as a fraction of VR.  0 = straight chord, 1 = centre.
ARC_INWARD = 0.45

# ── Vertex positions ─────────────────────────────────────────────
vertex_pos = {
    "a": np.array([-R,  R]),
    "b": np.array([ R,  R]),
    "c": np.array([ R, -R]),
    "d": np.array([-R, -R]),
    "e": np.array([ 0.,  0.]),
}
vertex_labels = {"a": "a", "b": "b", "c": "c", "d": "d", "e": " "}

# ── Directed edges ───────────────────────────────────────────────
edges = (
    [("a", "e"), ("e", "a")]
  + [("b", "e"), ("e", "b")] * 3
  + [("c", "e"), ("e", "c")] * 2
  + [("d", "e"), ("e", "d")]
)

# ═══════════════════════════════════════════════════════════════════
# 1.  Group edges by canonical pair
# ═══════════════════════════════════════════════════════════════════
pair_edges  = defaultdict(list)
loop_edges  = defaultdict(list)

for ei, (s, d) in enumerate(edges):
    if s == d:
        loop_edges[s].append(ei)
    else:
        canonical = tuple(sorted([s, d]))
        direction = +1 if (s, d) == canonical else -1
        pair_edges[canonical].append((ei, direction))

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

# ═══════════════════════════════════════════════════════════════════
# 2.  Geometry helpers
# ═══════════════════════════════════════════════════════════════════

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

def loop_cubic_controls(ei):
    v = edges[ei][0]
    ang_out, ang_in = loop_attach[ei]
    p0  = vertex_pos[v] + VR  * np.array([np.cos(ang_out), np.sin(ang_out)])
    p1  = vertex_pos[v] + VR  * np.array([np.cos(ang_in),  np.sin(ang_in)])
    lev = VR + LOOP_RADIUS
    cp1 = vertex_pos[v] + lev * np.array([np.cos(ang_out), np.sin(ang_out)])
    cp2 = vertex_pos[v] + lev * np.array([np.cos(ang_in),  np.sin(ang_in)])
    return p0, cp1, cp2, p1

def edge_attach_angle(ei, vertex):
    return attach_ang[(ei, vertex)]

# ═══════════════════════════════════════════════════════════════════
# 3.  Hardcoded Eulerian path
#     e-b-e-c-e-c-e-d-e-a-e-b-e-b-e
#     edge sequence: [3,2,9,8,11,10,13,12,1,0,5,4,7,6]
# ═══════════════════════════════════════════════════════════════════

path_vertices = ["e","b","e","c","e","c","e","d","e","a","e","b","e","b","e"]
path_edges    = [3, 2, 9, 8, 11, 10, 13, 12, 1, 0, 5, 4, 7, 6]

for step, ei in enumerate(path_edges):
    s, d = edges[ei]
    assert s == path_vertices[step] and d == path_vertices[step + 1], (
        f"step {step}: edge {ei}=({s},{d}) doesn't match "
        f"{path_vertices[step]}→{path_vertices[step+1]}"
    )

# ═══════════════════════════════════════════════════════════════════
# 4.  In-vertex connector: inward-bulging quadratic Bézier
#
#   p0  — point on circle boundary at the ARRIVING edge's angle
#   p1  — point on circle boundary at the DEPARTING edge's angle
#   cp  — chord midpoint pulled toward the vertex centre by ARC_INWARD
#
#   This gives a smooth shallow curve whose concave side faces OUTWARD
#   (away from the centre), exactly as requested.
# ═══════════════════════════════════════════════════════════════════

def inward_bezier_path(vertex, ei_in, ei_out):
    """Quadratic Bézier inside the vertex circle, bulging toward centre."""
    centre = vertex_pos[vertex]
    a_in   = edge_attach_angle(ei_in,  vertex)
    a_out  = edge_attach_angle(ei_out, vertex)

    p0 = centre + VR * np.array([np.cos(a_in),  np.sin(a_in)])
    p1 = centre + VR * np.array([np.cos(a_out), np.sin(a_out)])

    # Midpoint of the chord, then pulled inward (toward centre)
    chord_mid = (p0 + p1) * 0.5
    inward    = centre - chord_mid          # vector pointing to centre
    inward_len = np.linalg.norm(inward)

    if inward_len < 1e-9:
        # Degenerate: both edges leave from the same angle — tiny stub
        cp = chord_mid
    else:
        # Pull the control point inward by ARC_INWARD * VR so the curve
        # always has a consistent depth regardless of angular separation
        cp = chord_mid + (inward / inward_len) * ARC_INWARD * VR

    return Path([p0, cp, p1],
                [Path.MOVETO, Path.CURVE3, Path.CURVE3])


# ═══════════════════════════════════════════════════════════════════
# 5.  Figure
# ═══════════════════════════════════════════════════════════════════

fig, ax = plt.subplots(figsize=(7, 7))
lim = R + VR + 0.4
ax.set_xlim(-lim, lim)
ax.set_ylim(-lim, lim)
ax.set_aspect("equal")
ax.axis("off")
fig.patch.set_facecolor("white")

# ── 5a. Black arrow edges  (zorder 2) ────────────────────────────
for ei, (s, d) in enumerate(edges):
    if s == d:
        p0, cp1, cp2, p1 = loop_cubic_controls(ei)
        arrow = FancyArrowPatch(
            path=Path([p0, cp1, cp2, p1],
                      [Path.MOVETO, Path.CURVE4, Path.CURVE4, Path.CURVE4]),
            arrowstyle="-|>", mutation_scale=14,
            linewidth=1.2, color="black", zorder=2,
        )
    else:
        p0 = pt_on_circle(s, ei)
        p1 = pt_on_circle(d, ei)
        cp = bezier_control(ei)
        arrow = FancyArrowPatch(
            path=Path([p0, cp, p1],
                      [Path.MOVETO, Path.CURVE3, Path.CURVE3]),
            arrowstyle="-|>", mutation_scale=14,
            linewidth=1.2, color="black", zorder=2,
        )
    ax.add_patch(arrow)

# ── 5b. Dotted path overlays on edges  (zorder 3) ────────────────
def draw_edge_overlay(ax, ei):
    s, d = edges[ei]
    if s == d:
        p0, cp1, cp2, p1 = loop_cubic_controls(ei)
        verts = [p0, cp1, cp2, p1]
        codes = [Path.MOVETO, Path.CURVE4, Path.CURVE4, Path.CURVE4]
    else:
        p0 = pt_on_circle(s, ei)
        p1 = pt_on_circle(d, ei)
        cp = bezier_control(ei)
        verts = [p0, cp, p1]
        codes = [Path.MOVETO, Path.CURVE3, Path.CURVE3]

    ax.add_patch(mpatches.PathPatch(
        Path(verts, codes),
        facecolor="none", edgecolor=PATH_COLOR,
        linewidth=PATH_LW, linestyle=PATH_DOTS,
        capstyle="round", zorder=3,
    ))

for ei in path_edges:
    draw_edge_overlay(ax, ei)

# ── 5c. Vertex circles  (zorder 4) ───────────────────────────────
for name, p in vertex_pos.items():
    ax.add_patch(plt.Circle(p, VR, lw=1.5, ec="black", fc="white", zorder=4))

# ── 5d. In-vertex inward Bézier connectors  (zorder 5) ───────────
for i in range(1, len(path_vertices) - 1):
    v      = path_vertices[i]
    ei_in  = path_edges[i - 1]
    ei_out = path_edges[i]
    ax.add_patch(mpatches.PathPatch(
        inward_bezier_path(v, ei_in, ei_out),
        facecolor="none", edgecolor=PATH_COLOR,
        linewidth=PATH_LW, linestyle=PATH_DOTS,
        capstyle="round", zorder=5,
    ))

# ── 5e. Labels  (zorder 6) ───────────────────────────────────────
for name, p in vertex_pos.items():
    ax.text(*p, vertex_labels.get(name, name),
            ha="center", va="center",
            fontsize=22, fontfamily="serif", fontstyle="italic", zorder=6)

# ── Save ─────────────────────────────────────────────────────────
fig.savefig("graph-path.svg", format="svg",
            bbox_inches="tight", pad_inches=MARGIN,
            facecolor="white", edgecolor="none")
plt.close(fig)
print("Saved graph-path.svg")

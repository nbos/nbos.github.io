import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse
from scipy.optimize import brentq

# Reproducibility
np.random.seed(5)

# Style
dot_size = 40
line_width = 2.3
halo_scale = 1.8
edge_color = '#666666'

n = 8
x_left, x_right = 0.1, 0.9
y_positions = np.linspace(0.1, 0.9, n)

# Choose subsets
left_subset_idx  = np.array([3, 4, 5])
right_subset_idx = np.array([1, 2, 3, 4])

# Ellipse horizontal radius
rx = 0.05

# Compute vertical ellipse bounds so that top/bottom have equal gaps
# inside/outside to the nearest dots
def vertical_ellipse_bounds(y, subset_idx):
    i_min = subset_idx.min()
    i_max = subset_idx.max()
    below_neighbor = i_min - 1
    above_neighbor = i_max + 1
    y_bottom = 0.5 * (y[i_min] + y[below_neighbor])
    y_top    = 0.5 * (y[i_max] + y[above_neighbor])
    yc = 0.5 * (y_bottom + y_top)
    ry = 0.5 * (y_top - y_bottom)
    return yc, ry

yl_c, yl_ry = vertical_ellipse_bounds(y_positions, left_subset_idx)
yr_c, yr_ry = vertical_ellipse_bounds(y_positions, right_subset_idx)

base_p = 0.08
high_p = 0.99

A = np.zeros((n, n), dtype=bool)

for i in range(n):
    for j in range(n):
        p = base_p
        if (i in left_subset_idx) and (j in right_subset_idx):
            p = high_p
        if np.random.rand() < p:
            A[i, j] = True

def ensure_nonzero_degrees(A):
    n_left, n_right = A.shape
    deg_left  = A.sum(axis=1)
    deg_right = A.sum(axis=0)
    for i in np.where(deg_left == 0)[0]:
        choices = np.arange(n_right)
        if i < n_right:
            choices = choices[choices != i]
        if choices.size == 0:
            choices = np.arange(n_right)
        j = choices[np.argmin(deg_right[choices])]
        A[i, j] = True
        deg_left[i]  += 1
        deg_right[j] += 1
    for j in np.where(deg_right == 0)[0]:
        choices = np.arange(n_left)
        if j < n_left:
            choices = choices[choices != j]
        if choices.size == 0:
            choices = np.arange(n_left)
        i = choices[np.argmin(deg_left[choices])]
        A[i, j] = True
        deg_left[i]  += 1
        deg_right[j] += 1
    return A

def avoid_single_across(A):
    n_left, n_right = A.shape
    deg_left  = A.sum(axis=1)
    deg_right = A.sum(axis=0)
    for i in range(n_left):
        if deg_left[i] == 1:
            j_only = np.flatnonzero(A[i])[0]
            if j_only == i:
                candidates = np.setdiff1d(np.arange(n_right), [i, j_only])
                if candidates.size == 0:
                    candidates = np.setdiff1d(np.arange(n_right), [j_only])
                j2 = candidates[np.argmin(deg_right[candidates])]
                if not A[i, j2]:
                    A[i, j2] = True
                    deg_left[i]  += 1
                    deg_right[j2] += 1
    for j in range(n_right):
        if deg_right[j] == 1:
            i_only = np.flatnonzero(A[:, j])[0]
            if i_only == j:
                candidates = np.setdiff1d(np.arange(n_left), [j, i_only])
                if candidates.size == 0:
                    candidates = np.setdiff1d(np.arange(n_left), [i_only])
                i2 = candidates[np.argmin(deg_left[candidates])]
                if not A[i2, j]:
                    A[i2, j] = True
                    deg_left[i2] += 1
                    deg_right[j]  += 1
    return A

A = ensure_nonzero_degrees(A)
A = avoid_single_across(A)

# ── Tangent-line helpers ──────────────────────────────────────────────────────

def ellipse_support(xc, yc, a, b, theta):
    c, s = np.cos(theta), np.sin(theta)
    return xc * c + yc * s + np.sqrt((a * c) ** 2 + (b * s) ** 2)

def ellipse_tangent_point(xc, yc, a, b, theta):
    c, s = np.cos(theta), np.sin(theta)
    support_dist = np.sqrt((a * c) ** 2 + (b * s) ** 2)
    tx = xc + (a ** 2 * c) / support_dist
    ty = yc + (b ** 2 * s) / support_dist
    return tx, ty

def find_common_tangent_theta(c1, r1, c2, r2, quadrant):
    def f(theta):
        return ellipse_support(*c1, *r1, theta) - ellipse_support(*c2, *r2, theta)
    if quadrant == 'Q1':
        thetas = np.linspace(1e-3, np.pi / 2 - 1e-3, 2048)
    elif quadrant == 'Q3':
        thetas = np.linspace(np.pi + 1e-3, 3 * np.pi / 2 - 1e-3, 2048)
    else:
        raise ValueError("quadrant must be 'Q1' or 'Q3'")
    vals = f(thetas)
    for k in range(len(thetas) - 1):
        if vals[k] == 0:
            return thetas[k]
        if vals[k] * vals[k + 1] < 0:
            try:
                return brentq(f, thetas[k], thetas[k + 1], maxiter=200)
            except Exception:
                continue
    return thetas[np.argmin(np.abs(vals))]

def plot_tangent_segment(ax, c1, r1, c2, r2, theta, linewidth, zorder):
    x1, y1 = ellipse_tangent_point(*c1, *r1, theta)
    x2, y2 = ellipse_tangent_point(*c2, *r2, theta)
    ax.plot([x1, x2], [y1, y2], color='black', linewidth=linewidth, zorder=zorder)

# Pre-compute tangent angles once (shared by both figures)
c1 = (x_left,  yl_c)
r1 = (rx, yl_ry)
c2 = (x_right, yr_c)
r2 = (rx, yr_ry)
theta_TR = find_common_tangent_theta(c1, r1, c2, r2, quadrant='Q1')
theta_BL = find_common_tangent_theta(c1, r1, c2, r2, quadrant='Q3')

# ── Shared drawing helper ─────────────────────────────────────────────────────

def apply_axes_style(ax):
    ax.set_xlim(0.0, 1.0)
    ax.set_ylim(0.0, 1.0)
    ax.set_title(None)
    ax.set_xlabel(None)
    ax.set_ylabel(None)
    ax.axis('off')

def draw_edges(ax):
    for i in range(n):
        for j in range(n):
            if A[i, j]:
                ax.plot(
                    [x_left, x_right],
                    [y_positions[i], y_positions[j]],
                    color=edge_color,
                    linewidth=0.4 * line_width,
                    zorder=1,
                )

def draw_halos(ax):
    for x in (x_left, x_right):
        ax.scatter(
            np.full(n, x), y_positions,
            s=dot_size * halo_scale ** 2,
            color='white', edgecolors='white',
            zorder=3,
        )

def draw_dots(ax):
    for x in (x_left, x_right):
        ax.scatter(np.full(n, x), y_positions, s=dot_size, color='black', zorder=5)

def draw_ellipses(ax):
    for xc, yc, ry in ((x_left, yl_c, yl_ry), (x_right, yr_c, yr_ry)):
        ax.add_patch(Ellipse(
            xy=(xc, yc),
            width=2 * rx,
            height=2 * ry,
            angle=0.0,
            fill=False,
            edgecolor='black',
            linewidth=line_width,
            zorder=2,
        ))

def draw_tangents(ax):
    for theta in (theta_TR, theta_BL):
        plot_tangent_segment(ax, c1, r1, c2, r2, theta, linewidth=line_width, zorder=4)

# ── Figure 1: plain bipartite graph (no subsets) ─────────────────────────────

fig1, ax1 = plt.subplots()
draw_edges(ax1)
draw_halos(ax1)
draw_dots(ax1)
apply_axes_style(ax1)
plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
fig1.savefig('bipartite.svg')

# ── Figure 2: bipartite graph with subsets, ellipses & tangents ───────────────

fig2, ax2 = plt.subplots()
draw_edges(ax2)
draw_ellipses(ax2)      # zorder 2 — below halos
draw_halos(ax2)         # zorder 3 — covers edge ends near dots
draw_tangents(ax2)      # zorder 4 — above halos, below dots
draw_dots(ax2)          # zorder 5 — topmost
apply_axes_style(ax2)
plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
fig2.savefig('bipartite-joint.svg')

plt.show()

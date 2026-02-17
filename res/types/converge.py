import numpy as np
import matplotlib.pyplot as plt

# ---------- Parameters ----------
n = 100      # number of bins (possible values: 0 .. n-1)
N = 10000    # number of random samples per run
m = 3        # number of independent runs to overlay

# ---------- External sampler data ----------
# Paste your external sampler's output here: a list of indices in {0, …, n-1}.
# The script will compute the running bin-count variance and draw it in black.
external_samples = [
    # e.g.  3, 7, 0, 99, 42, ...
]

# ---------- Colour palette (no black) ----------
# Enough distinct, non-black colours for up to `m` random runs.
_palette = [
    "#1f77b4",  # muted blue
    "#ff7f0e",  # orange
    "#2ca02c",  # green
    "#d62728",  # red
    "#9467bd",  # purple
    "#8c564b",  # brown
    "#e377c2",  # pink
    "#7f7f7f",  # gray
    "#bcbd22",  # olive
    "#17becf",  # cyan
]

# ---------- Simulation ----------
fig, ax = plt.subplots(figsize=(10, 6))

rng = np.random.default_rng(seed=None)       # fresh entropy each execution

for run in range(m):
    counts = np.zeros(n, dtype=np.int64)      # histogram bins
    variances = np.empty(N)

    sum_x2 = 0                                # Σ counts[i]²
    sum_x0 = n                                # number of bins (constant)

    for t in range(1, N + 1):
        val = rng.integers(0, n)              # uniform draw from {0, …, n-1}

        sum_x2 += 2 * counts[val] + 1
        counts[val] += 1

        T = t
        variance = sum_x2 / sum_x0 - (T * T) / (sum_x0 * sum_x0)
        variances[t - 1] = variance

    ax.plot(
        np.arange(1, N + 1),
        variances,
        color=_palette[run % len(_palette)],
        alpha=0.7,
        linewidth=1,
        label=f"Run {run + 1}",
    )

# ---------- External sampler curve (black) ----------
if external_samples:
    ext_arr = np.asarray(external_samples, dtype=np.int64)

    # Validate
    if ext_arr.min() < 0 or ext_arr.max() >= n:
        raise ValueError(
            f"external_samples contains values outside [0, {n - 1}]: "
            f"min={ext_arr.min()}, max={ext_arr.max()}"
        )

    N_ext = len(ext_arr)
    counts = np.zeros(n, dtype=np.int64)
    variances = np.empty(N_ext)

    sum_x2 = 0
    sum_x0 = n

    for t in range(1, N_ext + 1):
        val = ext_arr[t - 1]

        sum_x2 += 2 * counts[val] + 1
        counts[val] += 1

        T = t
        variance = sum_x2 / sum_x0 - (T * T) / (sum_x0 * sum_x0)
        variances[t - 1] = variance

    ax.plot(
        np.arange(1, N_ext + 1),
        variances,
        color="black",
        linewidth=2,
        linestyle="-",
        label=f"External sampler ({N_ext} draws)",
    )

# ---------- Theoretical curve ----------
# Each count ~ Binomial(T, 1/n), so
#   E[pop. variance of counts] = T·(n−1) / n²
T_max = max(N, len(external_samples)) if external_samples else N
T_vals = np.arange(1, T_max + 1)
theoretical = T_vals * (n - 1) / (n ** 2)

ax.plot(
    T_vals,
    theoretical,
    color="crimson",
    linewidth=2,
    linestyle="--",
    label=r"Theory  $\mathbb{E}[\sigma^2] = \frac{T\,(n-1)}{n^2}$",
)

# ---------- Cosmetics ----------
ax.set_xlabel("Number of samples  $T$", fontsize=12)
ax.set_ylabel(r"Variance of bin counts  $\sigma^2$", fontsize=12)
ax.set_title(
    f"Law of Large Numbers — bin-count variance "
    f"($n = {n}$ bins, $N = {N}$ draws, $m = {m}$ runs)",
    fontsize=13,
)
ax.legend(fontsize=11)
ax.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

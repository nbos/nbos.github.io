import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm

# Parameters
n_sigmas = 40
scale = 0.8
x_range = (-4, 4)

# Generate sigma values below 1
sigmas = [scale**i for i in range(0,n_sigmas)]

# Define x values
x = np.linspace(x_range[0], x_range[1], 1000)

# Create plot
plt.figure(figsize=(6, 4))

# Plot standard normal (scaled normal)
std_normal_pdf = norm.pdf(x, 0, 1)
plt.plot(x, std_normal_pdf, 'k-', linewidth=1.5, label=r'$\alpha\cdot PDF(x)$')

# Plot curves for sigma < 1
for i, sigma in enumerate(sigmas):
    cdf_upper = norm.cdf(x + 1/(2*sigma), 0, 1)
    cdf_lower = norm.cdf(x - 1/(2*sigma), 0, 1)
    f_x = sigma * (cdf_upper - cdf_lower)
    label = r'$\alpha\cdot CDF(x)|_{x-0.5}^{x+0.5}$' if i == 0 else None
    plt.plot(x, f_x, '-', color='0.5', linewidth=1, label=label)

plt.xlabel('z', fontsize=15)
plt.gca().set_yticks([])  # Remove Y-axis ticks
plt.gca().set_ylabel('')  # Remove Y-axis label
# plt.grid(True, alpha=0.3)
plt.xlim(x_range)
plt.ylim(0, None)
plt.legend(loc='upper right', fontsize=12)
plt.tick_params(axis='both', labelsize=11)

plt.tight_layout()
plt.savefig('ftc1-0.svg', bbox_inches='tight')
plt.show()

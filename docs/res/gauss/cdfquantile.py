import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

# Generate data for the standard normal distribution
x_values = np.linspace(-3.8, 3.8, 500)  # x values for the CDF
cdf_values = norm.cdf(x_values)         # CDF of the standard normal distribution

p_values = np.linspace(0.0001, 0.9999, 500)  # Probabilities for the quantile function
quantile_values = norm.ppf(p_values)        # Standard normal quantile function

# Create a side-by-side plot
fig, axes = plt.subplots(1, 2, figsize=(8, 4))  # Two plots side by side

# Plot the CDF on the left
axes[0].plot(x_values, cdf_values, 'k-', linewidth=1.5)  # Black line
axes[0].grid(True, alpha=0.3)
axes[0].set_xlabel('z', fontsize=12)
axes[0].set_ylabel('Cumulative Probability', fontsize=12)
axes[0].set_title('CDF', fontsize=12)
axes[0].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axes[0].axvline(x=0, color='k', linestyle='-', alpha=0.3)
axes[0].tick_params(axis='both', which='major', labelsize=10)

# Plot the Quantile Function on the right
axes[1].plot(p_values, quantile_values, 'k-', linewidth=1.5)  # Black line
axes[1].grid(True, alpha=0.3)
axes[1].set_xlabel('Cumulative Probability', fontsize=12)
axes[1].set_ylabel('z', fontsize=12)
axes[1].set_title('Quantile Function', fontsize=12)
axes[1].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axes[1].axvline(x=0.5, color='k', linestyle='-', alpha=0.3)
axes[1].tick_params(axis='both', which='major', labelsize=10)

# Adjust layout
plt.tight_layout()  # Adjust layout to prevent label clipping
plt.savefig('cdfquantile.svg', bbox_inches='tight')
plt.show()

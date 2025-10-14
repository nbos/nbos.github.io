import numpy as np
import matplotlib.pyplot as plt
from scipy.special import log_ndtr, ndtri_exp

# Generate data for the standard normal distribution
x_values = np.linspace(-3.5, 3.5, 500)  # x values for the CDF
cdf_values = log_ndtr(x_values)         # CDF of the standard normal distribution

p_values = np.linspace(-8, -0.0001, 500)  # Probabilities for the quantile function
quantile_values = ndtri_exp(p_values)        # Standard normal quantile function

# Create a side-by-side plot
fig, axes = plt.subplots(1, 2, figsize=(8, 4))  # Two plots side by side

# Plot the CDF on the left
axes[0].plot(x_values, cdf_values, 'k-', linewidth=1.5)  # Black line
axes[0].grid(True, alpha=0.3)
axes[0].set_xlabel('z', fontsize=12)
axes[0].set_ylabel('ln-prob', fontsize=12)
axes[0].set_title('log-CDF', fontsize=12)
axes[0].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axes[0].axvline(x=0, color='k', linestyle='-', alpha=0.3)
axes[0].tick_params(axis='both', which='major', labelsize=10)

# Plot the Quantile Function on the right
axes[1].plot(p_values, quantile_values, 'k-', linewidth=1.5)  # Black line
axes[1].grid(True, alpha=0.3)
axes[1].set_xlabel('ln-prob', fontsize=12)
axes[1].set_ylabel('z', fontsize=12)
axes[1].set_title('Quantile-exp', fontsize=12)
axes[1].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axes[1].axvline(x=0, color='k', linestyle='-', alpha=0.3)
axes[1].tick_params(axis='both', which='major', labelsize=10)

# Move the y-label and ticks to the right for the Quantile Function
axes[1].yaxis.tick_right()  # Put the y-ticks on the right side
axes[1].yaxis.set_label_position("right")  # Move y-axis label to the right

# Adjust layout
plt.tight_layout()  # Adjust layout to prevent label clipping
plt.savefig('logcdfquantileexp.svg', bbox_inches='tight')
plt.show()

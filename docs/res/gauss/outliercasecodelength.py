import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

# Function to compute the updated value for a given n
def compute_value(n):
    # MLE parameters
    mu = 1 / n
    sigma_squared = 1 / n - 1 / n**2
    if sigma_squared <= 0:
        return np.nan  # Avoid invalid values for sigma_squared

    sigma = np.sqrt(sigma_squared)
    
    # CDF values
    cdf_0_1 = norm.cdf(0.5, loc=mu, scale=sigma)
    cdf_neg_0_1 = norm.cdf(-0.5, loc=mu, scale=sigma)
    cdf_1_1 = norm.cdf(1.5, loc=mu, scale=sigma)
    cdf_0_9 = norm.cdf(0.5, loc=mu, scale=sigma)
    
    # Compute the terms
    term1 = cdf_0_1 - cdf_neg_0_1
    term2 = cdf_1_1 - cdf_0_9
    
    if term1 <= 0 or term2 <= 0:
        return np.nan  # Avoid taking log of non-positive values

    # Final computation with log base 2
    result = -(n - 1) * np.log2(term1) - np.log2(term2)
    return result

# Range of n values (slightly above 1 to 10)
n_values = np.linspace(1.001, 100, 500)
results = [compute_value(n) for n in n_values]

# Plot the result
plt.figure(figsize=(6, 4))
plt.plot(n_values, results, label="Function Value", color="black")
plt.xlabel("n",fontsize=12)
plt.ylabel("Information (bits)",fontsize=12)

# Add some styling
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

# Increase tick label size
plt.tick_params(axis='both', which='major', labelsize=10)
plt.grid(alpha=0.3)
plt.tight_layout()  # Adjust layout to prevent label clipping
plt.savefig('outliercasecodelength.svg', bbox_inches='tight')
plt.show()

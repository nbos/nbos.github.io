import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm

# Parameters
n_values = range(1, 13)
x = np.linspace(-2, 15, 1000)

# Create plot
plt.figure(figsize=(6, 4))

# Plot curves for each n
for n in n_values:
    mean = n / 2
    variance = (n**2 + 2*n) / 12
    std = np.sqrt(variance)
    pdf = norm.pdf(x, mean, std)
    plt.plot(x, pdf, 'k-', linewidth=1)

plt.xlabel('x', fontsize=15)
plt.ylabel('PDF(x)', fontsize=12)
plt.xlim(-2, 15)
plt.ylim(0, None)
plt.tick_params(axis='both', labelsize=11)

plt.tight_layout()
plt.savefig('multimodals.svg', bbox_inches='tight')
plt.show()

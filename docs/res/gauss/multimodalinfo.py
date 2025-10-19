import matplotlib.pyplot as plt
import numpy as np

# Parameters
n_values = np.linspace(0, 10000, 10000)

# Calculate the function
y = n_values * (0.5 * np.log(np.pi * n_values * (n_values + 2) / 6) + (3 * n_values) / (2 * (n_values + 2)))

# Create plot
plt.figure(figsize=(6, 4))

# Plot the function
plt.plot(n_values, y, 'k-', linewidth=1)

plt.xlabel('n', fontsize=12)
plt.ylabel('Information', fontsize=12)
plt.xlim(0, None)
plt.ylim(0, None)
plt.tick_params(axis='both', labelsize=11)

plt.tight_layout()
plt.savefig('multimodalinfo.svg', bbox_inches='tight')
plt.show()

import numpy as np
import matplotlib.pyplot as plt

# Define the functions
def log_n(n):
    return np.log(n)

def difference_function(n):
    return (n + 1) * np.log(n + 1) - n * np.log(n)

# Generate n values
n = np.linspace(0, 100, 1000)

# Compute the functions
log_values = log_n(n)
diff_values = difference_function(n)

# Create the plot with updated styling
plt.figure(figsize=(6, 4))  # Smaller figure size

# Plot log(n)
plt.plot(n, log_values, 'k-', label='log(n)', linewidth=1.5)  # Black solid line

# Plot (n+1)log(n+1) - nlog(n)
plt.plot(n, diff_values, '--', color='black', label='(n+1)log(n+1) - nlog(n)', linewidth=1.2)  # Black dashed line

# Add labels and grid
plt.xlim(0,100)
plt.xlabel('n', fontsize=12)
plt.grid(True, alpha=0.3)  # More subtle grid lines

# Add horizontal and vertical reference lines
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

# Add a legend
plt.legend(fontsize=12, loc='lower right')

# Increase tick label size
plt.tick_params(axis='both', which='major', labelsize=10)

# Adjust layout and save the plot as a high-resolution image
plt.tight_layout()
plt.savefig('ologn.svg', bbox_inches='tight')
plt.show()

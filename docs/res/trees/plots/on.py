import numpy as np
import matplotlib.pyplot as plt
import math

# Define the functions
def linear_n(n):
    return n

def difference_function(n):
    return math.log(math.factorial(int(2*n))) - math.log(math.factorial(int(n+1))) - math.log(math.factorial(int(n)))

# Generate n values
n = np.linspace(1, 100, 100)  # integers work better with factorial; start at 1 to avoid log(0)

# Compute the functions
linear_values = linear_n(n)
diff_values = np.array([difference_function(ni) for ni in n])

# Create the plot with updated styling
plt.figure(figsize=(6, 4))

# Plot n
plt.plot(n, linear_values, 'k-', label='$n$', linewidth=1.5)

# Plot log((2n)!) - log((n+1)!) - log(n!)
# plt.plot(n, diff_values, '--', color='black', label='$\\log\\frac{(2n)!}{(n+1)! \\cdot n!}$', linewidth=1.2)
plt.plot(n, diff_values, '--', color='black', label='$\\log(2n)! - \\log(n+1)! - \\log(n)!$', linewidth=1.2)

# Add labels and grid
plt.xlim(0, 100)
plt.xlabel('n', fontsize=12)
plt.grid(True, alpha=0.3)

# Add horizontal and vertical reference lines
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

# Add a legend
plt.legend(fontsize=12, loc='lower right')

# Increase tick label size
plt.tick_params(axis='both', which='major', labelsize=10)

# Adjust layout and save
plt.tight_layout()
plt.savefig('on.svg', bbox_inches='tight')
plt.show()

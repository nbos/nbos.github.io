import numpy as np
import matplotlib.pyplot as plt

def pdf_function(n):
    # Calculate the coefficient part
    coefficient = 1 / np.sqrt((2*np.pi/n) - (2*np.pi/(n**2)))
    
    # Calculate the exponential part
    exponent = -((n-1)**2)/(2*n-2)
    
    return coefficient * np.exp(exponent)

# Create an array of n values
n_values = np.linspace(1.001, 10, 1000)

# Calculate PDF values
pdf_values = pdf_function(n_values)

# Create the plot
plt.figure(figsize=(6, 4))  # Smaller figure size
plt.plot(n_values, pdf_values, 'k-', label='Probability density at 1', linewidth=1.5)  # Black line
plt.grid(True, alpha=0.3)
plt.xlabel('n', fontsize=12)
plt.ylabel('PDF(1)', fontsize=12)

# Add some styling
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

# Increase tick label size
plt.tick_params(axis='both', which='major', labelsize=10)
plt.tight_layout()  # Adjust layout to prevent label clipping
plt.savefig('pdf1.svg', bbox_inches='tight')
plt.show()

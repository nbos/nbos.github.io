import numpy as np
import matplotlib.pyplot as plt

# Define the Gaussian PDF (standard normal)
def gaussian_pdf(x, mu=0, sigma=1):
    return (1 / (np.sqrt(2 * np.pi) * sigma)) * np.exp(-((x - mu) ** 2) / (2 * sigma ** 2))

# Generate x values in the range [0, 10]
x = np.linspace(0, 6, 500)

# Compute the base PDF
pdf = gaussian_pdf(x)

# Define scaling factors
scaling_factors = [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000]

# Line styles to rotate through
line_styles = ['-', '--', '-.']

# Create the plot
plt.figure(figsize=(6, 4))  # Compact figure size

# Plot the scaled PDFs
for i, scale in enumerate(scaling_factors):
    plt.plot(x, pdf * scale, line_styles[i % len(line_styles)], color='black', linewidth=1.2)

# Fix y-axis range and set x-axis range
plt.xlim(0, 6)
plt.ylim(0, 0.5)

# Add x-axis label and grid
plt.xlabel('z', fontsize=12)
plt.grid(True, alpha=0.3)  # Subtle grid

# Remove unnecessary elements
plt.tick_params(axis='both', which='major', labelsize=10)
plt.axhline(0, color='k', linestyle='-', alpha=0.3)
plt.gca().tick_params(axis='y', which='both', left=False, labelleft=False)

# Adjust layout
plt.tight_layout()

# Save and display the plot
plt.savefig('pdfscales.svg', bbox_inches='tight')
plt.show()

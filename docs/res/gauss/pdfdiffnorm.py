import numpy as np
import matplotlib.pyplot as plt

# Define the Gaussian PDF
def gaussian_pdf(x, mu=0, sigma=1):
    return (1 / (np.sqrt(2 * np.pi) * sigma)) * np.exp(-((x - mu)**2) / (2 * sigma**2))

# Define the derivative of the Gaussian PDF
def gaussian_pdf_derivative(x, mu=0, sigma=1):
    return -((x - mu) / (sigma**2)) * gaussian_pdf(x, mu, sigma)

# Define the normalized derivative (ignoring sign)
def normalized_derivative(x, mu=0, sigma=1):
    pdf = gaussian_pdf(x, mu, sigma)
    pdf_derivative = gaussian_pdf_derivative(x, mu, sigma)
    return 0.1 * np.abs(pdf_derivative) / pdf

# Generate x values
x = np.linspace(-6, 6, 500)

# Compute PDF, derivative, and normalized derivative
pdf = gaussian_pdf(x)
pdf_derivative = gaussian_pdf_derivative(x)
normalized_deriv = normalized_derivative(x)

# Create the plot with updated styling
plt.figure(figsize=(6, 4))  # Smaller figure size

# Plot the PDF
plt.plot(x, pdf, 'k-', label='Standard Gaussian PDF', linewidth=1.5)  # Black solid line for PDF

# Plot the PDF derivative
plt.plot(x, pdf_derivative, '--', color='black', label="PDF Derivative", linewidth=1.2)  # Red dashed line for derivative

# Plot the normalized derivative
# Uncomment this line if you want to include the normalized derivative
plt.plot(x, normalized_deriv, '-.', color='black', label='Normalized Derivative', linewidth=1.2)  # Blue dash-dot line

# Add labels and grid
plt.xlabel('z', fontsize=12)
plt.grid(True, alpha=0.3)  # More subtle grid lines

# Add horizontal and vertical reference lines
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

# Add a legend
plt.legend(fontsize=10)

# Increase tick label size
plt.tick_params(axis='both', which='major', labelsize=10)

# Adjust layout and save the plot as a high-resolution image
plt.tight_layout()
plt.savefig('pdfdiffnorm.svg', bbox_inches='tight')
plt.show()

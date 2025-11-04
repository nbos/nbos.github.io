import matplotlib.pyplot as plt
import numpy as np

# Generate eight random (x, y) points
np.random.seed(2)  # For reproducibility
points = np.random.uniform(0, 1, (8, 2))
x = points[:, 0]
y = points[:, 1]

# Calculate 10% bounding box around the data extrema
x_min, x_max = x.min(), x.max()
y_min, y_max = y.min(), y.max()

x_range = x_max - x_min
y_range = y_max - y_min

# Add 10% buffer
x_buffer = 0.2 * x_range if x_range > 0 else 0.2
y_buffer = 0.2 * y_range if y_range > 0 else 0.2

x_plot_min = x_min - x_buffer
x_plot_max = x_max + x_buffer
y_plot_min = y_min - y_buffer
y_plot_max = y_max + y_buffer

# Prepare x values for smooth plotting within the bounded range
x_plot = np.linspace(x_plot_min, x_plot_max, 1000)

# Fit and plot polynomials of different degrees
degrees = [1, 3, 7]
linestyles = [':', '--', '-']
labels = ['Linear', 'Degree 3', 'Degree 7']

fig, ax = plt.subplots()

# Plot the data points
ax.scatter(x, y, color='black', label='Data points')

# Fit and plot each polynomial
for deg, ls, label in zip(degrees, linestyles, labels):
    coeffs = np.polyfit(x, y, deg)
    y_fit = np.polyval(coeffs, x_plot)
    ax.plot(x_plot, y_fit, color='black', linestyle=ls, label=label)

# Set axis limits to the bounded region
ax.set_xlim(x_plot_min, x_plot_max)
ax.set_ylim(y_plot_min, y_plot_max)

# Add legend
ax.legend(fontsize=12)

# Remove title, axes, and labels
ax.set_title(None)
ax.set_xlabel(None)
ax.set_ylabel(None)
ax.axis('off')

plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
plt.savefig('overfit.svg')
plt.show()

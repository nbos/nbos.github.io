import matplotlib.pyplot as plt
import numpy as np

# =============================================================================
# PARAMETERS
# =============================================================================

# Mathematical parameters
MU = 0          # Mean
SIGMA = 1.75       # Standard deviation
X_RANGE = 4     # Range in standard deviations from mean (±X_RANGE*SIGMA)
N_POINTS = 1000

# Display parameters
FIGURE_WIDTH = 6
FIGURE_HEIGHT = 3
CURVE_COLOR = 'black'
CURVE_LINEWIDTH = 2
VLINE_COLOR = 'grey'
VLINE_LINEWIDTH = 0.8
VLINE_ALPHA = 0.6
AXIS_LINEWIDTH = 2
FONT_SIZE = 10

# =============================================================================
# PLOT GENERATION
# =============================================================================

# Calculate x limits based on mu and sigma
X_MIN = MU - X_RANGE * SIGMA
X_MAX = MU + X_RANGE * SIGMA

# Create x values for the Gaussian curve
x = np.linspace(X_MIN, X_MAX, N_POINTS)

# General normal distribution PDF
y = (1 / (SIGMA * np.sqrt(2 * np.pi))) * np.exp(-0.5 * ((x - MU) / SIGMA)**2)

# Create the plot
plt.figure(figsize=(FIGURE_WIDTH, FIGURE_HEIGHT))

# Add vertical lines at each integer value within the range
for i in range(int(np.floor(X_MIN)), int(np.ceil(X_MAX)) + 1):
    # Calculate y value at this x position using the parametrized distribution
    y_at_x = (1 / (SIGMA * np.sqrt(2 * np.pi))) * np.exp(-0.5 * ((i - MU) / SIGMA)**2)
    # Draw vertical line from bottom to curve
    plt.vlines(x=i, ymin=0, ymax=y_at_x, color=VLINE_COLOR, 
               linewidth=VLINE_LINEWIDTH, alpha=VLINE_ALPHA)

# Plot the curve after the vertical lines so it appears on top
plt.plot(x, y, color=CURVE_COLOR, linewidth=CURVE_LINEWIDTH)

# Set up x-axis labels - show μ at the mean position
plt.xticks([MU], [r'$\mu$'], fontsize=FONT_SIZE)
plt.yticks([])

# Remove ticks and hide all spines except bottom (x-axis)
plt.tick_params(axis='x', length=0)
plt.tick_params(axis='y', length=0)
for spine in plt.gca().spines.values():
    spine.set_visible(False)

# Show only the bottom spine (x-axis line)
plt.gca().spines['bottom'].set_visible(True)
plt.gca().spines['bottom'].set_color('black')
plt.gca().spines['bottom'].set_linewidth(AXIS_LINEWIDTH)

# Set limits - force Y axis to start at 0
plt.xlim(X_MIN, X_MAX)
plt.ylim(bottom=0)

plt.savefig('gauss-pdf.svg', bbox_inches='tight', pad_inches=0)
plt.show()

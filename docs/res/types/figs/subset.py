import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse

# Settings
dot_size = 36
line_width = 2.3
margin = 0.01  # Adjustable margin parameter

# 9 dots in a horizontal line
n = 9
x_positions = np.linspace(0.2, 0.8, n)
y = 0.5  # All dots at same y position

# Ellipse around dots index 2, 3, 4
# Calculate horizontal bounds (halfway to neighboring dots)
x_left = 0.5 * (x_positions[1] + x_positions[2])
x_right = 0.5 * (x_positions[4] + x_positions[5])
x_center = 0.5 * (x_left + x_right)
rx = 0.5 * (x_right - x_left)
ry = 0.05  # Vertical radius

# Calculate tight bounds with adjustable margin
x_min = x_positions[0] - margin
x_max = x_positions[-1] + margin
y_min = y - ry - margin
y_max = y + ry + margin

# Create figure with normal size
fig, ax = plt.subplots(figsize=(6, 1))

# Ellipse
ellipse = Ellipse(
    xy=(x_center, y),
    width=2 * rx,
    height=2 * ry,
    angle=0.0,
    fill=False,
    edgecolor='black',
    linewidth=line_width,
    zorder=2
)
ax.add_patch(ellipse)

# Draw dots
ax.scatter(x_positions, np.full(n, y), s=dot_size, color='black', zorder=5)

# Styling
ax.set_xlim(x_min, x_max)
ax.set_ylim(y_min, y_max)
ax.set_aspect('equal')
ax.axis('off')
plt.subplots_adjust(left=0, right=1, top=1, bottom=0)

plt.savefig('subset.svg', bbox_inches='tight', pad_inches=0.2)
plt.show()

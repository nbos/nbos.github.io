import matplotlib.pyplot as plt
import pandas as pd

# Load the data from the CSV file
data = pd.read_csv('./randomuniform.csv', header=None, names=['n', 'information', 'code_length'])

# Extract the columns
n = data['n']
information = data['information']
code_length = data['code_length']

# Create the plot with updated styling
plt.figure(figsize=(6, 4))  # Smaller figure size

# Plot "Information"
plt.plot(n, information, '--', color='black', label='Information', linewidth=1.5)  # Black solid line for Information

# Plot "Code Length"
plt.plot(n, code_length, 'k-', label="Code Length", linewidth=1.2)  # Black dashed line for Code Length

# Add labels, grid, and reference lines
plt.xlabel('n', fontsize=12)
plt.ylabel('bits', fontsize=12)

# Add major grid lines for Y-axis ticks
plt.grid(visible=True, which='major', axis='y', color='gray', linestyle='--', linewidth=0.5, alpha=0.7)

# Optionally, you can enable minor ticks for more granularity
plt.minorticks_on()
plt.grid(visible=True, which='minor', axis='y', color='gray', linestyle=':', linewidth=0.5, alpha=0.5)

# Add horizontal and vertical reference lines
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

# Add a legend
plt.legend(fontsize=10)

# Increase tick label size
plt.tick_params(axis='both', which='major', labelsize=10)

# Adjust layout and save the plot as a high-resolution image
plt.tight_layout()
plt.savefig('randomuniform.svg', bbox_inches='tight')
plt.show()

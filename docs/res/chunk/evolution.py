import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import sys
import os
from matplotlib.ticker import LogLocator, FuncFormatter

def decimal_notation_formatter(x, pos):
    """Format log ticks using plain decimal notation (e.g. 9e3 -> 9000)."""
    if x == 0:
        return "0"
    # if x is an integer, print without decimals
    if abs(x - int(x)) < 1e-8:
        return str(int(x))
    else:
        # if it's not an exact integer, show as decimal
        return f"{x:.4g}"  # four significant figures

# Check command line arguments
if len(sys.argv) < 2:
    print("Usage: python script.py <csv_file1> <csv_file2> ... <csv_fileN>")
    sys.exit(1)

csv_files = sys.argv[1:]

# Validate all files exist
for csv_path in csv_files:
    if not os.path.exists(csv_path):
        print(f"Error: File '{csv_path}' does not exist")
        sys.exit(1)

all_data = []
labels = []

# Read all CSV files
for csv_path in csv_files:
    try:
        df = pd.read_csv(csv_path, escapechar='\\')
        
        # Check if CSV has at least 2 columns
        if df.shape[1] < 2:
            print(f"Error: CSV file '{csv_path}' must have at least 2 columns")
            continue
            
        # Extract X (first column) and Y (second column)
        x_data = pd.to_numeric(df.iloc[:, 0], errors='coerce').fillna(0).values
        y_data = pd.to_numeric(df.iloc[:, 1], errors='coerce').fillna(0).values
        
        # Remove any rows where X or Y is invalid
        valid_mask = ~(np.isnan(x_data) | np.isnan(y_data) | (x_data <= 0))
        x_data = x_data[valid_mask]
        y_data = y_data[valid_mask]
        
        if len(x_data) == 0:
            print(f"Warning: No valid data in '{csv_path}', skipping")
            continue
            
        all_data.append((x_data, y_data))
        
        # Get label from filename (without .csv extension)
        basename = os.path.basename(csv_path)
        label = os.path.splitext(basename)[0]
        labels.append(label)
        
        print(f"Loaded '{csv_path}': {len(x_data)} data points")
        
    except Exception as e:
        print(f"Error reading '{csv_path}': {e}")
        continue

if not all_data:
    print("Error: No valid CSV files could be loaded")
    sys.exit(1)

# Find global minimum X and Y values across all files
global_min_x = min(min(x_data) for x_data, y_data in all_data)
global_min_y = min(min(y_data) for x_data, y_data in all_data)

print(f"Global minimum: X={global_min_x}, Y={global_min_y}")

# Process each dataset
processed_data = []

for i, (x_data, y_data) in enumerate(all_data):
    # Shift data to start at global minimum
    x_shifted = x_data - x_data[0] + global_min_x
    y_shifted = y_data - y_data[0] + global_min_y
    
    # Log-scale subsampling if more than 500 points
    if len(x_shifted) > 500:
        # Create log-spaced indices for subsampling
        log_min = np.log10(x_shifted[0])
        log_max = np.log10(x_shifted[-1])
        
        # Create 400 log-spaced X values
        target_x_values = np.logspace(log_min, log_max, 400)
        
        # Find nearest indices in original data
        indices = []
        for target_x in target_x_values:
            idx = np.argmin(np.abs(x_shifted - target_x))
            if idx not in indices:  # Avoid duplicates
                indices.append(idx)
        
        indices = sorted(indices)
        x_subsampled = x_shifted[indices]
        y_subsampled = y_shifted[indices]
        
        print(f"Subsampled '{labels[i]}' from {len(x_shifted)} to {len(x_subsampled)} points")
    else:
        x_subsampled = x_shifted
        y_subsampled = y_shifted
    
    processed_data.append((x_subsampled, y_subsampled))

# Create the plot
plt.figure(figsize=(6, 5))

# Plot all lines in solid black with line width 1.5
line_endpoints = []
for i, (x_data, y_data) in enumerate(processed_data):
    plt.plot(x_data, y_data, color='black', linewidth=1.5, solid_capstyle='round')

    # Add triangle marker at Y axis for end Y value
    plt.plot(x_data[0] + 18, y_data[-1], marker='<', markersize=8, 
             color=(0,0,0,0), markeredgecolor='black', markeredgewidth=0.8)
    
    # Store endpoint for label positioning
    line_endpoints.append((x_data[-1], y_data[-1], labels[i]))

# Set log scale for X axis
plt.xscale('log')

# Calculate axis limits
all_x = np.concatenate([x_data for x_data, y_data in processed_data])
all_y = np.concatenate([y_data for x_data, y_data in processed_data])

x_min, x_max = np.min(all_x), np.max(all_x)
y_min, y_max = np.min(all_y), np.max(all_y)

# Extend Y axis below minimum to show more tick labels
y_range = y_max - y_min
y_bottom = 1.5  # Extend 20% below minimum

plt.xlim(left=x_min, right=x_max)
plt.ylim(bottom=y_bottom, top=y_max * 1.1)  # Add 10% padding on top

# Configure axis with LogLocator and decimal formatter
ax = plt.gca()

# # Remove top and right spines to make plot look like it doesn't end
# ax.spines['top'].set_visible(False)
# ax.spines['right'].set_visible(False)

# Use LogLocator to show ticks at 1e3, 2e3, 3e3 ...
ax.xaxis.set_major_locator(LogLocator(base=10.0, subs=np.arange(1, 10)*0.1/0.1, numticks=100))
ax.xaxis.set_major_formatter(FuncFormatter(decimal_notation_formatter))

# Set X tick labels vertical (270 degrees rotation)
plt.setp(ax.get_xticklabels(), rotation=270, ha="center")

# Add labels in monospace, positioned above and to the left of last data point
for x_end, y_end, label in line_endpoints:
    # Position label slightly above and to the left
    x_offset_factor = 0.9  # Move left by reducing X
    
    plt.annotate(label, 
                xy=(x_end, y_end),
                xytext=(x_end * x_offset_factor, y_end + 0.2),
                textcoords='data',
                fontfamily='monospace',
                fontsize=12,
                ha='right',
                va='bottom')

# Set axis labels
plt.xlabel('Number of symbols', fontsize=12)
plt.ylabel('Compression factor', fontsize=12)

# Enable grid
plt.grid(True, which='both', alpha=0.3)
#plt.minorticks_on()

# "zero" axes
plt.axhline(y=y_min, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=x_min, color='k', linestyle='-', alpha=0.3)

plt.tick_params(axis='both', which='major', labelsize=10)
plt.tick_params(axis='both', which='minor', labelsize=8)

# Save plot
output_path = 'evolution.svg'
plt.tight_layout()
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f"Plot saved to: {output_path}")
plt.show()

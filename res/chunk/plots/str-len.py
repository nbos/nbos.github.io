import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
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

# Get script directory
script_dir = os.path.dirname(os.path.abspath(__file__))

# Hardcoded file list - files in current directory
csv_files = [
    os.path.join(script_dir, '../out/enwik4.csv'),
    os.path.join(script_dir, '../out/enwik5.csv'),
    os.path.join(script_dir, '../out/enwik6.csv'),
    os.path.join(script_dir, '../out/enwik7.csv')
]

# Initial values for each file
initial_values = {
    'enwik4': 10**4,
    'enwik5': 10**5,
    'enwik6': 10**6,
    'enwik7': 10**7
}

label_offsets = {
    'enwik4': (100, -0.1),
    'enwik5': (500, -0.1),
    'enwik6': (2000, -0.1),
    'enwik7': (0, -0.1)
}

# Validate all files exist
for csv_path in csv_files:
    if not os.path.exists(csv_path):
        print(f"Error: File '{csv_path}' does not exist")
        exit(1)

all_data = []
labels = []

# Read all CSV files
for csv_path in csv_files:
    try:
        df = pd.read_csv(csv_path, escapechar='\\')
        
        # Check if CSV has at least 11 columns (0-10)
        if df.shape[1] < 11:
            print(f"Error: CSV file '{csv_path}' must have at least 11 columns")
            continue
            
        # Extract X (first column) and column 10 data (11th column, 0-indexed)
        x_data = pd.to_numeric(df.iloc[:, 0], errors='coerce').fillna(0).values
        col10_data = pd.to_numeric(df.iloc[:, 10], errors='coerce').fillna(0).values
        
        # Get label and initial value
        basename = os.path.basename(csv_path)
        label = os.path.splitext(basename)[0]
        initial_value = initial_values[label]
        
        # Calculate cumulative ratio: subtract col10 values cumulatively from initial value
        current_value = initial_value
        ratios = []
        
        for col10_val in col10_data:
            current_value = current_value - col10_val
            ratios.append(current_value / initial_value)
        
        y_data = np.array(ratios)
        
        # Remove any rows where X or Y is invalid
        valid_mask = ~(np.isnan(x_data) | np.isnan(y_data) | (x_data <= 0))
        x_data = x_data[valid_mask]
        y_data = y_data[valid_mask]
        
        if len(x_data) == 0:
            print(f"Warning: No valid data in '{csv_path}', skipping")
            continue
            
        all_data.append((x_data, y_data))
        labels.append(label)
        
        print(f"Loaded '{csv_path}': {len(x_data)} data points")
        
    except Exception as e:
        print(f"Error reading '{csv_path}': {e}")
        continue

if not all_data:
    print("Error: No valid CSV files could be loaded")
    exit(1)

# Find global minimum X value across all files
global_min_x = min(min(x_data) for x_data, y_data in all_data)

print(f"Global minimum X: {global_min_x}")

# Process each dataset
processed_data = []

for i, (x_data, y_data) in enumerate(all_data):
    # Shift X data to start at global minimum
    x_shifted = x_data - x_data[0] + global_min_x
    
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
        y_subsampled = y_data[indices]
        
        print(f"Subsampled '{labels[i]}' from {len(x_shifted)} to {len(x_subsampled)} points")
    else:
        x_subsampled = x_shifted
        y_subsampled = y_data
    
    processed_data.append((x_subsampled, y_subsampled))

# Create the plot
plt.figure(figsize=(6, 5))

# Plot all lines in solid black with line width 1.5
line_endpoints = []
for i, (x_data, y_data) in enumerate(processed_data):
    plt.plot(x_data, y_data, color='black', linewidth=1.5, solid_capstyle='round')

    # Store endpoint for label positioning
    line_endpoints.append((x_data[-1], y_data[-1], labels[i]))

# Set log scale for X axis
plt.xscale('log')

# Calculate axis limits
all_x = np.concatenate([x_data for x_data, y_data in processed_data])

x_min, x_max = np.min(all_x), np.max(all_x)

# Set Y axis to [0, 1] range
plt.xlim(left=x_min, right=x_max)
plt.ylim(bottom=0, top=1)

# Configure axis with LogLocator and decimal formatter
ax = plt.gca()

# Use LogLocator to show ticks at 1e3, 2e3, 3e3 ...
ax.xaxis.set_major_locator(LogLocator(base=10.0, subs=np.arange(1, 10)*0.1/0.1, numticks=100))
ax.xaxis.set_major_formatter(FuncFormatter(decimal_notation_formatter))

# Set X tick labels vertical (270 degrees rotation)
plt.setp(ax.get_xticklabels(), rotation=270, ha="center")

# Add labels in monospace, positioned relative to last data point with manual offsets
for x_end, y_end, label in line_endpoints:
    # Get manual offset for this label
    dx, dy = label_offsets.get(label, (0, 0))
    
    # Position label slightly above and to the left, plus manual offset
    x_offset_factor = 0.9  # Move left by reducing X
    
    plt.annotate(label, 
                xy=(x_end, y_end),
                xytext=(x_end * x_offset_factor + dx, y_end + 0.02 + dy),
                textcoords='data',
                fontfamily='monospace',
                fontsize=12,
                ha='right',
                va='bottom')

# Set axis labels
plt.xlabel('Number of symbols', fontsize=12)
plt.ylabel('String Length Relative to Start', fontsize=12)

# Enable grid
plt.grid(True, which='both', alpha=0.3)

# "zero" axes
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=x_min, color='k', linestyle='-', alpha=0.3)

plt.tick_params(axis='both', which='major', labelsize=10)
plt.tick_params(axis='both', which='minor', labelsize=8)

# Save plot
output_path = os.path.join(script_dir, 'str-len.svg')
plt.tight_layout()
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f"Plot saved to: {output_path}")
plt.show()

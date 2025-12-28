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
data_dir = os.path.join(script_dir, '.')

# Hardcoded file list
csv_files = [
    os.path.join(data_dir, 'overlap/enwik4-enwik7.csv'),
    os.path.join(data_dir, 'overlap/enwik5-enwik7.csv'),
    os.path.join(data_dir, 'overlap/enwik6-enwik7.csv')
]


label_offsets = {
    'enwik4': (500, -0.07),
    'enwik5': (2100, -0.07),
    'enwik6': (800, -0.11)
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
        # Remove "o-" prefix if present
        if label.startswith('o-'):
            label = label[2:]
        labels.append(label)
        
        print(f"Loaded '{csv_path}': {len(x_data)} data points")
        
    except Exception as e:
        print(f"Error reading '{csv_path}': {e}")
        continue

if not all_data:
    print("Error: No valid CSV files could be loaded")
    exit(1)

# Process each dataset
processed_data = []

for i, (x_data, y_data) in enumerate(all_data):
    # No shifting - use raw data
    x_raw = x_data
    y_raw = y_data
    
    # Log-scale subsampling if more than 500 points
    if len(x_raw) > 500:
        # Create log-spaced indices for subsampling
        log_min = np.log10(x_raw[0])
        log_max = np.log10(x_raw[-1])
        
        # Create 400 log-spaced X values
        target_x_values = np.logspace(log_min, log_max, 400)
        
        # Find nearest indices in original data
        indices = []
        for target_x in target_x_values:
            idx = np.argmin(np.abs(x_raw - target_x))
            if idx not in indices:  # Avoid duplicates
                indices.append(idx)
        
        indices = sorted(indices)
        x_subsampled = x_raw[indices]
        y_subsampled = y_raw[indices]
        
        print(f"Subsampled '{labels[i]}' from {len(x_raw)} to {len(x_subsampled)} points")
    else:
        x_subsampled = x_raw
        y_subsampled = y_raw
    
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
all_y = np.concatenate([y_data for x_data, y_data in processed_data])

x_min, x_max = np.min(all_x), np.max(all_x)

# Set Y axis range to 0-1
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
                xytext=(x_end * x_offset_factor + dx, y_end + 0.04 + dy),
                textcoords='data',
                fontfamily='monospace',
                fontsize=12,
                ha='right',
                va='bottom')

# Set axis labels
plt.xlabel('Number of symbols', fontsize=12)
plt.ylabel(r"Overlap with $\mathtt{enwik7}$'s dictionary", fontsize=12)

# Enable grid
plt.grid(True, which='both', alpha=0.3)
#plt.minorticks_on()

# "zero" axes
y_min = np.min(all_y)
plt.axhline(y=y_min, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=x_min, color='k', linestyle='-', alpha=0.3)

plt.tick_params(axis='both', which='major', labelsize=10)
plt.tick_params(axis='both', which='minor', labelsize=8)

# plt.title(r"Overlap with $\mathtt{enwik7}$'s dictionary")

# Save plot
output_path = os.path.join(script_dir, 'overlap.svg')
plt.tight_layout()
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f"Plot saved to: {output_path}")
plt.show()

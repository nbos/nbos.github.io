import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import sys
from matplotlib.ticker import LogLocator, FuncFormatter
from matplotlib.ticker import MaxNLocator

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

# Parse command line arguments
if len(sys.argv) < 3 or len(sys.argv) % 2 != 1:
    print("Usage: python script.py label1 filepath1 label2 filepath2 label3 filepath3 ...")
    print("Example: python script.py enwik4 ../out/enwik4-eval-enwik7.csv enwik5 ../out/enwik5-eval-enwik7.csv")
    sys.exit(1)

# Extract label-filepath pairs
input_labels = []
csv_files = []
for i in range(1, len(sys.argv), 2):
    input_labels.append(sys.argv[i])
    csv_files.append(sys.argv[i + 1])

print(f"Processing {len(csv_files)} files:")
for label, filepath in zip(input_labels, csv_files):
    print(f"  {label}: {filepath}")

# Validate all files exist
for csv_path in csv_files:
    if not os.path.exists(csv_path):
        print(f"Error: File '{csv_path}' does not exist")
        exit(1)

all_data = []
labels = []

# Read all CSV files
for i, csv_path in enumerate(csv_files):
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
        labels.append(input_labels[i])
        
        print(f"Loaded '{csv_path}': {len(x_data)} data points")
        
    except Exception as e:
        print(f"Error reading '{csv_path}': {e}")
        continue

if not all_data:
    print("Error: No valid CSV files could be loaded")
    exit(1)

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

# Get the last label (for eval dataset)
last_label = labels[-1]

# Plot all lines
line_endpoints = []
for i, (x_data, y_data) in enumerate(processed_data):
    # Use dotted style for last file, solid for others
    if i == len(processed_data) - 1:
        plt.plot(x_data, y_data, color='black', linewidth=1.5, 
                linestyle=':', solid_capstyle='round', label=f'{last_label} (eval = train)')
    else:
        plt.plot(x_data, y_data, color='black', linewidth=1.5, solid_capstyle='round')
    
    # Store endpoint for label positioning (skip last file)
    if i != len(processed_data) - 1:
        line_endpoints.append((x_data[-1], y_data[-1], labels[i]))

# Set log scale for X axis
plt.xscale('log')

# Calculate axis limits
all_x = np.concatenate([x_data for x_data, y_data in processed_data])
all_y = np.concatenate([y_data for x_data, y_data in processed_data])

x_min = np.min(all_x)
y_min = np.min(all_y)

# Get X max from last file (not second-to-last)
x_max = np.max(processed_data[-1][0])

# Calculate y_max only from data up to x_max
y_values_in_range = []
for x_data, y_data in processed_data:
    mask = x_data <= x_max
    y_values_in_range.extend(y_data[mask])
y_max = np.max(y_values_in_range)

# Extend Y axis below minimum to show more tick labels
y_bottom = 1.5

plt.xlim(left=x_min, right=x_max)
plt.ylim(bottom=y_bottom, top=y_max)

# Configure axis with LogLocator and decimal formatter
ax = plt.gca()
ax.yaxis.set_major_locator(MaxNLocator(nbins=10))

# Use LogLocator to show ticks at 1e3, 2e3, 3e3 ...
ax.xaxis.set_major_locator(LogLocator(base=10.0, subs=np.arange(1, 10)*0.1/0.1, numticks=100))
ax.xaxis.set_major_formatter(FuncFormatter(decimal_notation_formatter))

# Set X tick labels vertical (270 degrees rotation)
plt.setp(ax.get_xticklabels(), rotation=270, ha="center")

# Calculate consistent visual offset for labels in log-scale
# For log scale, constant visual offset means: x_label = x_end * scale_factor
x_scale_factor = 1.4  # Move left in log space (same ratio for all)
y_offset = -0.25  # Constant offset in linear Y space (same difference for all)

# Add labels in monospace, positioned with consistent visual offset
for x_end, y_end, label in line_endpoints:
    # In log space, multiply by scale factor for consistent visual distance
    x_label = x_end * x_scale_factor
    y_label = y_end + y_offset
    
    plt.annotate(label, 
                xy=(x_end, y_end),
                xytext=(x_label, y_label),
                textcoords='data',
                fontfamily='monospace',
                fontsize=12,
                ha='right',
                va='bottom')

# Add legend for last file
plt.legend(loc='best', fontsize=14, framealpha=0.9)

# Set axis labels - use last label for Y axis
plt.xlabel('Number of symbols', fontsize=12)
plt.ylabel(rf"Compression factor ($\mathtt{{{last_label}}}$)", fontsize=12)

# Enable grid
plt.grid(True, which='both', alpha=0.3)

# "zero" axes
plt.axhline(y=y_min, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=x_min, color='k', linestyle='-', alpha=0.3)

plt.tick_params(axis='both', which='major', labelsize=10)
plt.tick_params(axis='both', which='minor', labelsize=8)

# Generate output filename from all input file basenames concatenated with "-"
basenames = [os.path.splitext(os.path.basename(f))[0] for f in csv_files]
output_basename = '-'.join(basenames)
output_filename = f'{output_basename}.svg'

# Get script directory
script_dir = os.path.dirname(os.path.abspath(__file__))
output_path = os.path.join(script_dir, output_filename)

# Save plot
plt.tight_layout()
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f"Plot saved to: {output_path}")
plt.show()

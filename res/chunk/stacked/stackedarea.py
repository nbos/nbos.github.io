import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import sys
import os
from matplotlib.ticker import ScalarFormatter, FuncFormatter

# Check command line arguments
if len(sys.argv) != 2:
    print("Usage: python script.py <path_to_csv>")
    sys.exit(1)

csv_path = sys.argv[1]
title = os.path.splitext(os.path.basename(csv_path))[0]

# Check if file exists
if not os.path.exists(csv_path):
    print(f"Error: File '{csv_path}' does not exist")
    sys.exit(1)

# Read CSV file
try:
    df = pd.read_csv(csv_path, escapechar='\\')
    print("Successfully parsed CSV with escape character handling")
except Exception as e:
    print(f"CSV parsing failed: {e}")
    sys.exit(1)

print(f"CSV parsed successfully. Shape: {df.shape}")

# Subsample if more than 500 rows
if len(df) > 500:
    step = len(df) // 400  # Target around 400 points
    df = df.iloc[::step]
    print(f"Subsampled to {len(df)} rows")

# Check if CSV has enough columns
if df.shape[1] < 8:
    print(f"Error: CSV file must have at least 8 columns, but only has "
          f"{df.shape[1]}")
    print("Available columns:", 
          df.columns.tolist() if hasattr(df, 'columns') else 'N/A')
    sys.exit(1)

# Extract X data from first column
try:
    x_data = pd.to_numeric(df.iloc[:, 0], errors='coerce').fillna(0).values
except Exception as e:
    print(f"Error extracting X data from first column: {e}")
    sys.exit(1)

# Extract data - columns 5,7,8 (0-indexed: 4,6,7) - dropping m and n columns
try:
    y_data = df.iloc[:, [4, 6, 7]].values
    # Convert to numeric, replacing any non-numeric values with 0
    y_data = pd.DataFrame(y_data).apply(pd.to_numeric, 
                                        errors='coerce').fillna(0).values
    y_data = y_data / 8 # bytes
except Exception as e:
    print(f"Error extracting numeric data from selected columns: {e}")
    sys.exit(1)

labels = ['Rules', 'Counts', 'String']

# Create figure with styling similar to reference
plt.figure(figsize=(6, 4))
plt.title(title, fontsize=14, fontfamily='monospace', pad=20)

# Color scheme with adjusted grays for high alpha
colors = ['#000000', '#606060', '#E0E0E0']

# Create stacked area chart using fill_between
cumsum = np.zeros(len(x_data))

for i, (data, label, color) in enumerate(zip(y_data.T, labels, colors)):
    plt.fill_between(x_data, cumsum, cumsum + data, 
                    label=label, color=color, alpha=0.9)
    cumsum += data

# Add complete outline around the whole stack
outline_x = np.concatenate([x_data, x_data[::-1]])
outline_y = np.concatenate([cumsum, np.zeros(len(x_data))])
plt.plot(outline_x, outline_y, color='black', linewidth=0.5, alpha=1.0)
    
# Add left-pointing triangle marker with point just touching Y axis
first_y_value = np.sum(y_data[0])
x_range = x_data[-1] - x_data[0]
x_offset = x_range * 0.02
plt.plot(x_data[0] + x_offset, first_y_value, marker='<', markersize=12, 
         color='white', markeredgecolor='black', markeredgewidth=1)

# Formatting with new axis labels
plt.xlabel('Number of symbols', fontsize=12)
plt.ylabel('Code length (bytes)', fontsize=12)

# Enable minor ticks for more grid lines
plt.minorticks_on()

# Enhanced grid with both major and minor ticks
plt.grid(True, which='major', alpha=0.5, linestyle='-')
plt.grid(True, which='minor', alpha=0.3, linestyle=':')
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)

# Set axis limits to start exactly at first X value and Y=0
plt.xlim(left=x_data[0])
plt.ylim(bottom=0)

def format_func(x, p):
    if x == 0:
        return '0'
    elif x < 1000:
        return f'{x:.0f}B'
    elif x < 1000000:
        val = x/1000
        return f'{val:.0f}KB' if val == int(val) else f'{val:.1f}KB'
    elif x < 1000000000:
        val = x/1000000
        return f'{val:.0f}MB' if val == int(val) else f'{val:.1f}MB'
    elif x < 1000000000000:
        val = x/1000000000
        return f'{val:.0f}GB' if val == int(val) else f'{val:.1f}GB'
    else:
        val = x/1000000000000
        return f'{val:.0f}TB' if val == int(val) else f'{val:.1f}TB'

formatter = FuncFormatter(format_func)
plt.gca().yaxis.set_major_formatter(formatter)

# Legend with box - reversed order to match visual stacking (top to bottom)
handles, legend_labels = plt.gca().get_legend_handles_labels()
plt.legend(list(reversed(handles)), list(reversed(legend_labels)), 
           fontsize=12, frameon=True, edgecolor='black', 
           facecolor='white')

plt.tick_params(axis='both', which='major', labelsize=10)
plt.tick_params(axis='both', which='minor', labelsize=8)

# Generate output path (same directory as input, but .svg extension)
base_path = os.path.splitext(csv_path)[0]
output_path = base_path + '.svg'

# Save and display
plt.tight_layout()
plt.savefig(output_path, dpi=300, bbox_inches='tight')
print(f"Plot saved to: {output_path}")
plt.show()

import matplotlib.pyplot as plt

# Dataset information with SI units
datasets = [
    ("enwik4", "10", "KB"),
    ("enwik5", "100", "KB"),
    ("enwik6", "1", "MB"),
    ("enwik7", "10", "MB"),
    ("enwik8", "100", "MB"),
    ("enwik9", "1", "GB")
]

# Prepare table data with aligned formatting
table_data = []
max_num_width = max(len(num) for _, num, _ in datasets)  # Find max width of numbers

for filename, number, unit in datasets:
    # Right-align the number and left-align the unit
    aligned_size = f"{number:>{max_num_width}} {unit}"
    row = [filename, aligned_size]
    table_data.append(row)

# Create a figure with explicit dimensions
fig, ax = plt.subplots(figsize=(8, 4))  # Set explicit figure size
ax.axis('off')

# Create the table
table = ax.table(cellText=table_data,
                colLabels=[r'$\bf Filename$', r'$\bf Size\ (bytes)$'],
                cellLoc='center',
                loc='center',
                colWidths=[0.3, 0.4])  # Adjusted column widths for two columns

# Style the table to match the original
table.auto_set_font_size(False)
table.set_fontsize(13)
table.scale(0.8, 3)

# Style the header row - bold text only
for i in range(2):
    table[(0, i)].set_text_props(weight='bold')
    
# Save with proper padding
plt.savefig('datasets.svg', bbox_inches='tight', pad_inches=0, 
            facecolor='none', edgecolor='none')
plt.show()

import matplotlib.pyplot as plt
import math

# Sample natural numbers of growing size
numbers = [1, 2, 5, 50, 100, 10000, 1000000]

# Prepare table data
table_data = []
for n in numbers:
    binary_rep = bin(n)[2:]  # Remove '0b' prefix
    binary_len = len(binary_rep)
    log2_val = round(math.log2(n), 2)
    
    row = [f"{n:,}", binary_rep, str(binary_len), f"{log2_val:.2f}"]
    table_data.append(row)

# Create a figure with explicit dimensions
fig, ax = plt.subplots(figsize=(8,6))  # Set explicit figure size
ax.axis('off')

# Create the table
table = ax.table(cellText=table_data,
                colLabels=[r'$\bf n$', r'$\bf (n)_2$', r'$\bf len(n)_2$', r'$\bf \mathrm{\bf log}_2(n)$'],
                cellLoc='center',
                loc='center',
                colWidths=[0.3, 0.55, 0.25, 0.3])  # Adjusted column widths

# Style the table to match the original
table.auto_set_font_size(False)
table.set_fontsize(13)
table.scale(0.8, 3)

# Style the header row - bold text only
for i in range(4):
    table[(0, i)].set_text_props(weight='bold')
    
# Save with proper padding
plt.savefig('binarylengths.svg', bbox_inches='tight', pad_inches=0, 
            facecolor='none', edgecolor='none')
plt.show()

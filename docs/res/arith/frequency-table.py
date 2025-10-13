import matplotlib.pyplot as plt

# Original data
data = [
    ('A', 0.0858), ('B', 0.0135), ('C', 0.0316), ('D', 0.0356), ('E', 0.1197),
    ('F', 0.0199), ('G', 0.0219), ('H', 0.0406), ('I', 0.0776), ('J', 0.0012),
    ('K', 0.0064), ('L', 0.0441), ('M', 0.0304), ('N', 0.0715), ('O', 0.0768),
    ('P', 0.0240), ('Q', 0.0039), ('R', 0.0672), ('S', 0.0634), ('T', 0.0893),
    ('U', 0.0308), ('V', 0.0108), ('W', 0.0135), ('X', 0.0035), ('Y', 0.0156),
    ('Z', 0.0014)
]

# Split into 3 columns (9, 9, 8 items)
col1 = data[0:9]
col2 = data[9:18]
col3 = data[18:26]

# Pad the third column to match the length of the first two
col3 += [('', '')] * (9 - len(col3))

# Create table data
table_data = []
for i in range(9):
    row = [
        col1[i][0], f"{col1[i][1]:.4f}",  # Column 1
        col2[i][0], f"{col2[i][1]:.4f}",  # Column 2
        col3[i][0], f"{col3[i][1]:.4f}" if col3[i][0] else ''  # Column 3
    ]
    table_data.append(row)

# Create a much smaller figure that's closer to the actual table size
fig, ax = plt.subplots(figsize=(6, 3))
ax.axis('off')

# Create the table and make it fill the entire axes
table = ax.table(cellText=table_data,
                colLabels=['Symbol', 'Probability', 'Symbol', 'Probability', 'Symbol', 'Probability'],
                cellLoc='center',
                loc='center',
                colWidths=[0.08, 0.12, 0.08, 0.12, 0.08, 0.12])

# Style the table
table.auto_set_font_size(False)
table.set_fontsize(11)
table.scale(2, 2)

# Style the header row - bold text only
for i in range(6):
    table[(0, i)].set_text_props(weight='bold')

# Save with absolutely no padding
plt.savefig('frequency-table.svg', bbox_inches='tight', pad_inches=0, 
            facecolor='none', edgecolor='none')
plt.show()

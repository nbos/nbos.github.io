import matplotlib.pyplot as plt

# Prepare table data - selected rows from the full table
table_data = [
    ['1', '828', '2', '573.634', '570.048', '570.048', '0.629'],
    ['2', '4193', '31', '14378.1', '14278.7', '14278.7', '0.696'],
    ['3', '331', '15', '888.741', '855.711', '855.711', '3.860'],
    ['⋮', '⋮', '⋮', '⋮', '⋮', '⋮', '⋮'],
    # ['82', '92', '3', '100.359', '95.651', '95.651', '4.922'],
    ['83', '290', '9', '630.094', '610.039', '610.039', '3.288'],
    ['84', '3764', '4', '5217.54', '5205.2', '5205.2', '0.237'],
    ['85', '14', '11', '32.788', '23.112', '23.112', '41.867'],
    ['86', '104', '2', '72.010', '69.461', '69.461', '3.671'],
    # ['87', '102', '3', '109.905', '105.114', '105.114', '4.558'],
    ['⋮', '⋮', '⋮', '⋮', '⋮', '⋮', '⋮']
    # ['99', '531', '312', '2984.82', '2619.12', '2619.12', '13.963'],
    # ['100', '59', '4', '81.412', '75.304', '75.304', '8.112']
]

# Create a figure with explicit dimensions
fig, ax = plt.subplots(figsize=(10, 8))  # Adjusted size for more columns
ax.axis('off')

# Create the table
table = ax.table(cellText=table_data,
                colLabels=['#', 'N', 'm', 'N log(N)...', 
                          'log(N!)...', '(verif.)', 'delta (%)'],
                cellLoc='center',
                loc='center',
                colWidths=[0.08, 0.12, 0.08, 0.18, 0.18, 0.18, 0.18])  # Adjusted column widths

# Style the table to match the original
table.auto_set_font_size(False)
table.set_fontsize(18)
table.scale(1.2, 4.0)

# Style the header row - bold text only
for i in range(7):  # 7 columns
    table[(0, i)].set_text_props(weight='bold')
    table[(6, i)].set_facecolor('#f0f0f0')  # Row 84
    table[(7, i)].set_facecolor('#f0f0f0')  # Row 85
    
# Save with proper padding
plt.savefig('table_abbrev.svg', bbox_inches='tight', pad_inches=0, 
            facecolor='none', edgecolor='none')
plt.show()

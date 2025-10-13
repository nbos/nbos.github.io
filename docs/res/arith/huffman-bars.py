import matplotlib.pyplot as plt
import numpy as np

# Original data
data = [
    ('A', 0.0858), ('B', 0.0135), ('C', 0.0316), ('D', 0.0356), ('E', 0.1197),
    ('F', 0.0199), ('G', 0.0219), ('H', 0.0406), ('I', 0.0776), ('J', 0.0012),
    ('K', 0.0064), ('L', 0.0441), ('M', 0.0304), ('N', 0.0715), ('O', 0.0768),
    ('P', 0.0240), ('Q', 0.0039), ('R', 0.0672), ('S', 0.0634), ('T', 0.0893),
    ('U', 0.0308), ('V', 0.0108), ('W', 0.0135), ('X', 0.0035), ('Y', 0.0156),
    ('Z', 0.0014)
]

# New data with h^x values
h = 0.5
new_data = [
    ('A', h**4), ('C', h**5), ('D', h**5), ('E', h**3), ('G', h**5), ('H', h**5),
    ('I', h**4), ('L', h**4), ('M', h**5), ('P', h**5), ('N', h**4), ('O', h**4),
    ('R', h**4), ('S', h**4), ('T', h**3), ('B', h**6), ('F', h**6), ('U', h**5),
    ('V', h**6), ('W', h**6), ('Y', h**6), ('K', h**7), ('Q', h**8), ('X', h**9),
    ('J', h**10), ('Z', h**10)
]

# Extract letters and values
letters = [item[0] for item in data]
values1 = [item[1] for item in data]

# Create a dictionary for the new data for easy lookup
new_data_dict = dict(new_data)

# Create values2 list in the same order as letters
values2 = [new_data_dict[letter] for letter in letters]

# Create positions for the bars
x = np.arange(len(letters))
width = 0.4

# Create minimalistic histogram with grouped bars
plt.figure(figsize=(10, 6))
plt.bar(x - 0.1, values1, width, color='black', edgecolor='black')
plt.bar(x + 0.1, values2, width, color='lightgray', edgecolor='gray')

# Set x-axis labels
plt.xticks(x, letters, fontsize=14)
plt.yticks(fontsize=14)

# Remove ticks and spines
plt.tick_params(axis='x', length=0)
plt.tick_params(axis='y', length=0)
for spine in plt.gca().spines.values():
    spine.set_visible(False)

# Adjust spacing
plt.xlim(-1, 26)
plt.ylim(-0.003, 0.13)
plt.savefig('huffman-bars.svg', bbox_inches='tight', pad_inches=0)
plt.show()

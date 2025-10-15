import matplotlib.pyplot as plt

data = [
    ('A', 0.0858), ('B', 0.0135), ('C', 0.0316), ('D', 0.0356), ('E', 0.1197),
    ('F', 0.0199), ('G', 0.0219), ('H', 0.0406), ('I', 0.0776), ('J', 0.0012),
    ('K', 0.0064), ('L', 0.0441), ('M', 0.0304), ('N', 0.0715), ('O', 0.0768),
    ('P', 0.0240), ('Q', 0.0039), ('R', 0.0672), ('S', 0.0634), ('T', 0.0893),
    ('U', 0.0308), ('V', 0.0108), ('W', 0.0135), ('X', 0.0035), ('Y', 0.0156),
    ('Z', 0.0014)
]

# Extract letters and values
letters = [item[0] for item in data]
values = [item[1] for item in data]

# Create minimalistic histogram
plt.figure(figsize=(10, 6))
plt.bar(letters, values, color='black', edgecolor='black', width=0.4)

# Change font size for x-axis labels
plt.xticks(fontsize=14)
plt.yticks(fontsize=14)

# Remove ticks and spines
plt.tick_params(axis='x', length=0)
plt.tick_params(axis='y', length=0)
for spine in plt.gca().spines.values():
    spine.set_visible(False)

# Adjust spacing and add Y axis label
plt.xlim(-1, 26)
plt.ylim(-0.003,0.13)
plt.savefig('frequency-bars.svg', bbox_inches='tight', pad_inches=0)
plt.show()

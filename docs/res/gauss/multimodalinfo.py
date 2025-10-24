import matplotlib.pyplot as plt
import numpy as np

def multimodal_info_function(n):
    """Calculate the multimodal information function"""
    return n * (0.5 * np.log(np.pi * n * (n + 2) / 6)
                + (3 * n) / (2 * (n + 2)))

# Create two subplots side by side
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(8, 4))

# Left plot (n from 2 to 10)
n_values1 = np.linspace(2, 10, 1000)
info_values1 = multimodal_info_function(n_values1)
ax1.plot(n_values1, info_values1, 'k-', linewidth=1.5)
ax1.grid(True, alpha=0.3)
ax1.set_xlabel('n', fontsize=12)
ax1.set_xlim(2, 10)
ax1.set_ylabel('Information', fontsize=12)
ax1.set_ylim(0, None)
ax1.tick_params(axis='both', which='major', labelsize=10)
ax1.axhline(y=0, color='k', linestyle='-', alpha=0.3)
ax1.axvline(x=0, color='k', linestyle='-', alpha=0.3)

# Right plot (n from 2 to 1000)
n_values2 = np.linspace(2, 1000, 1000)
info_values2 = multimodal_info_function(n_values2)
ax2.plot(n_values2, info_values2, 'k-', linewidth=1.5)
ax2.grid(True, alpha=0.3)
ax2.set_xlabel('n', fontsize=12)
ax2.set_xlim(2, 1000)
ax2.set_ylabel('Information', fontsize=12)
ax2.set_ylim(0, None)
ax2.tick_params(axis='both', which='major', labelsize=10)
ax2.axhline(y=0, color='k', linestyle='-', alpha=0.3)
ax2.axvline(x=0, color='k', linestyle='-', alpha=0.3)

plt.tight_layout()
plt.savefig('multimodalinfo.svg', bbox_inches='tight')
plt.show()

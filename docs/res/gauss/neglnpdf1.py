import numpy as np
import matplotlib.pyplot as plt

def information_function(n):
    # Calculate (n-1)/2 + (1/2)*log(2π(n-1)) - log(n)
    return (n-1)/2 + 0.5*np.log(2*np.pi*(n-1)) - np.log(n)

# Create two subplots side by side
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(8, 4))

# Left plot (n from 0 to 10)
n_values1 = np.linspace(1.001, 10, 1000)
info_values1 = information_function(n_values1)
ax1.plot(n_values1, info_values1, 'k-', label='E(code length)', linewidth=1.5)
ax1.grid(True, alpha=0.3)
ax1.set_xlabel('n', fontsize=12)
ax1.set_ylabel('Information (nat)', fontsize=12)
ax1.tick_params(axis='both', which='major', labelsize=10)
ax1.axhline(y=0, color='k', linestyle='-', alpha=0.3)
ax1.axvline(x=0, color='k', linestyle='-', alpha=0.3)
ax1.set_aspect(1.0/ax1.get_data_ratio(), adjustable='box')

# Right plot (n from 0 to 100)
n_values2 = np.linspace(1.001, 100, 1000)
info_values2 = information_function(n_values2)
ax2.plot(n_values2, info_values2, 'k-',label='E(code length)', linewidth=1.5)
ax2.grid(True, alpha=0.3)
ax2.set_xlabel('n', fontsize=12)
ax2.set_ylabel('Information (nat)', fontsize=12)
ax2.tick_params(axis='both', which='major', labelsize=10)
ax2.axhline(y=0, color='k', linestyle='-', alpha=0.3)
ax2.axvline(x=0, color='k', linestyle='-', alpha=0.3)
ax2.set_aspect(1.0/ax2.get_data_ratio(), adjustable='box')

plt.tight_layout()
plt.savefig('neglnpdf1.svg', bbox_inches='tight')
plt.show()

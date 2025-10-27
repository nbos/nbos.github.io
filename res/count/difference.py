import numpy as np
import matplotlib.pyplot as plt
from scipy.special import gammaln

## Create figure and adjust styling
plt.figure(figsize=(6, 4))
n = np.linspace(0.001, 100, 500)  # Start above 0 to avoid log(0)

## Calculate functions
log_fact = gammaln(n + 1) / np.log(2)  # log₂(n!) using gamma function
n_log_n = n * np.log2(n)  # n log₂(n) term
remainder = n_log_n - log_fact  # actual difference between log(n!) and n log(n)

## Plot with monochrome styling
plt.plot(n, log_fact, 'k-', linewidth=1.5, label=r'$log\ \Gamma(n+1)\ =\ \log(n!)$')
plt.plot(n, n_log_n, '--', color='black', linewidth=1.2, 
         label=r'$n\ \log n$')
plt.plot(n, remainder, '-.', color='black', linewidth=1.2, 
         label=r'$n\ \log n - \log(n!)$')

## Formatting elements
plt.xlabel('n', fontsize=12)
plt.ylabel('bits', fontsize=12)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

## Legend with box
plt.legend(fontsize=9, frameon=True, edgecolor='black', facecolor='white')

plt.tick_params(axis='both', which='major', labelsize=10)

## Save and display
plt.tight_layout()
plt.savefig('difference.svg', dpi=300, bbox_inches='tight')
plt.show()

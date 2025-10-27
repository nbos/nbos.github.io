import numpy as np
import matplotlib.pyplot as plt
from scipy.special import gammaln

## Create figure and adjust styling
plt.figure(figsize=(6, 4))
n = np.linspace(0.001, 5, 500)  # Start above 0 to avoid log(0)

## Calculate functions
log_fact = gammaln(n + 1) / np.log(2) # log(n!) using gamma function
stirling = (n * np.log2(n) - n * np.log2(np.e) + 0.5 * np.log2(2 * np.pi * n))

## Plot with monochrome styling
plt.plot(n, log_fact, 'k-', linewidth=1.5, label=r'$log\ \Gamma(n+1)\ =\ \log(n!)$')
plt.plot(n, stirling, '--', color='black', linewidth=1.2, 
         label=r'$n\ \log n - n\ \log e + \log(2\pi n) / 2$')

## Formatting elements
plt.xlabel('n', fontsize=12)
plt.ylabel('bits', fontsize=12)
plt.grid(True, alpha=0.3)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.legend(fontsize=10)
plt.tick_params(axis='both', which='major', labelsize=10)

## Save and display
plt.tight_layout()
plt.savefig('stirling.svg', dpi=300, bbox_inches='tight')
plt.show()

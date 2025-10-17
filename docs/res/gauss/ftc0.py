import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm
  
# Define x values
x = np.linspace(-4, 4, 1000)

# Standard normal PDF (scaled normal)
std_normal_pdf = norm.pdf(x, 0, 1)

plt.figure(figsize=(6, 4))

# Plot standard normal
plt.plot(x, std_normal_pdf, 'k-', linewidth=1.5, label=r'$PDF(x)$')

# Plot for sigma = 1
sigma = 1
a = sigma
cdf_upper = norm.cdf(x + 1/(2*sigma), 0, 1)
cdf_lower = norm.cdf(x - 1/(2*sigma), 0, 1)
f_x_1 = a * (cdf_upper - cdf_lower)
plt.plot(x, f_x_1, '--', color='black', linewidth=1.5, label=r'$CDF(x)|_{x-0.5}^{x+0.5}$')

# # Plot for sigma = 2
# sigma = 2
# a = sigma
# cdf_upper = norm.cdf(x + 1/(2*sigma), 0, 1)
# cdf_lower = norm.cdf(x - 1/(2*sigma), 0, 1)
# f_x_2 = a * (cdf_upper - cdf_lower)
# plt.plot(x, f_x_2, '--', color='black', linewidth=1, label='σ = 2')

plt.xlabel('x', fontsize=15)
plt.legend(loc='upper right', fontsize=12)
# plt.grid(True, alpha=0.3)
plt.tick_params(axis='both', labelsize=11)
plt.xlim(-4, 4)
plt.ylim(0, None)
    
plt.tight_layout()
plt.savefig('ftc0.svg', bbox_inches='tight')
plt.show()

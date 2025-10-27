import numpy as np
import math
from scipy.special import gammaln
import random
from tabulate import tabulate

def experiment():
    # Randomly sample hyperparameters uniformly in log-domain
    log_N_min, log_N_max = np.log(10), np.log(10000)
    log_alphabet_min, log_alphabet_max = np.log(2), np.log(1000)
    
    # Sample N from 10 to 10000 in log space
    log_N = random.uniform(log_N_min, log_N_max)
    N = int(np.round(np.exp(log_N)))
    
    # Sample alphabet_size from 2 to 1000 in log space
    log_alphabet = random.uniform(log_alphabet_min, log_alphabet_max)
    alphabet_size = int(np.round(np.exp(log_alphabet)))
    
    # Ensure alphabet_size is at most N
    alphabet_size = min(alphabet_size, N)
    
    # Generate a random sequence
    alphabet = list(range(alphabet_size))
    sequence = random.choices(alphabet, k=N)
    
    # Count occurrences of each symbol
    counts = np.zeros(alphabet_size, dtype=int)
    for symbol in sequence:
        counts[symbol] += 1
    
    # Filter out symbols that don't appear in the sequence
    nonzero_counts = counts[counts > 0]
    
    # Calculate value 1: N log(N) - n0 log(n0) - n1 log(n1) - ...
    value1 = N * math.log(N) if N > 0 else 0
    for count in nonzero_counts:
        value1 -= count * math.log(count)
    
    # Calculate value 2: log(N!) - log(n0!) - log(n1!) - ...
    value2 = gammaln(N + 1)
    for count in nonzero_counts:
        value2 -= gammaln(count + 1)
    
    # Calculate value 3: adaptive information content
    remaining_counts = counts.copy()
    remaining_total = N
    value3 = 0
    
    for symbol in sequence:
        # Calculate probability of observing this symbol with current model
        if remaining_counts[symbol] > 0:
            prob = remaining_counts[symbol] / remaining_total
            value3 -= math.log(prob)
            
            # Update the model by removing the observed symbol
            remaining_counts[symbol] -= 1
            remaining_total -= 1
    
    return {
        'N': N,
        'Alphabet Size': alphabet_size,
        'Used Symbols': len(nonzero_counts),
        'Value 1': value1,
        'Value 2': value2,
        'Value 3': value3
    }

def main():
    np.random.seed(42)
    random.seed(42)
    
    results = []
    for i in range(100):
        results.append(experiment())
    
    # Print results in a nice table with full precision
    headers = ['#', 'N', 'm', 'N log(N) - ...', 'log(N!) - ...', '(verif.)', 'delta (%)']
    table_data = []
    
    for i, result in enumerate(results):
        table_data.append([
            i+1, 
            result['N'], 
            result['Used Symbols'],
            result['Value 1'], 
            result['Value 2'], 
            result['Value 3'],
            100 * (result['Value 1'] - result['Value 2']) / result['Value 2']
        ])
    
    print(tabulate(table_data, headers=headers, tablefmt="grid", floatfmt="g"))
    
if __name__ == "__main__":
    main()

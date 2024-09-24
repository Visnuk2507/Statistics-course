import numpy as np
import matplotlib.pyplot as plt

# Simulate a fair dice
# Number of simulated realizations
n = 30
# Draw independently from the set (1,2,3,4,5,6) with equal probability
xFair = np.random.choice(range(1, 7), size=n, replace=True)
# Count the number of each outcome using the bincount function
counts = np.bincount(xFair)
# Plot the pdf
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(8,4))
ax1.bar(range(1,7), [1/6]*6, color='red')
# Plot the empirical pdf
ax1.bar(range(1,7), counts[1:7]/n)
# Plot the cdf
ax2.bar(range(1,7), np.cumsum([1/6]*6), color='red')
# Add the empirical cdf
ax2.bar(range(1,7), np.cumsum(counts[1:7]/n))






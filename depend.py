import numpy as np

# Cost function:
def cost(alloc, prefs):
    index = np.arange(len(alloc)) * prefs.shape[1] + alloc
    return sum(prefs.flat[index]^2) #sum of squares of preferences

def create_sizes_pool(n, k):
    sizes_pool = np.zeros(k, dtype=int)

    n_full = n // 5
    remainder = n % 5

    sizes_pool[:n_full] = 5
    if remainder == 4:
        sizes_pool[n_full] = 4
    elif remainder != 0:
        sizes_pool[:remainder] += 1
    
    return sizes_pool



from depend import np

# Function to define the options of sizes
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

# First option: just randomize the pool
def deal_sizes1(n, k):
    sizes_pool = create_sizes_pool(n, k)
    np.random.shuffle(sizes_pool)
    sizes = sizes_pool

    return sizes

# Second option: randomize, but give preference to put 0 people in groups with low preference
def deal_sizes2(n, k, prefs):
    sizes_pool = create_sizes_pool(n, k)
    zeros = sizes_pool == 0
    sizes = np.zeros(k, dtype=int)

    pref_means = np.mean(prefs, axis=0)
    weights = pref_means / np.sum(pref_means)
    index_zeros = np.random.choice(k, zeros.sum(), p = weights)

    sizes_pool_nonzero = sizes_pool[~zeros]
    np.random.shuffle(sizes_pool_nonzero)
    sizes[~np.isin(np.arange(k), index_zeros)] = sizes_pool_nonzero

    return sizes

# We could do more options, giving preferences to put 6 people in groups with high preference, for example

# A deterministic option:
def deal_sizesfixed(k, groups0 = [], groups4 = [], groups6 = []):
    sizes = np.full(k, 5)
    sizes[groups0] = 0
    sizes[groups4] = 4
    sizes[groups6] = 6

    return sizes
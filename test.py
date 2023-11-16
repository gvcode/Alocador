import numpy as np
from depend import cost, create_sizes_pool, deal_sizes2

k = 14 #groups options
n = 61 #people
prefs = np.array([np.random.permutation(range(1, k + 1)) for _ in range(n)])

print(np.round(np.mean(prefs, axis=0),1))
print(deal_sizes2(prefs, n, k))

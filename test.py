import numpy as np
from deals import create_sizes_pool, deal_sizes2
from depend import cost


k = 14 #groups options
n = 61 #people
prefs = np.array([np.random.permutation(range(1, k + 1)) for _ in range(n)])
print(prefs)

print(create_sizes_pool(n, k))

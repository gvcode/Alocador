# --- Imports and functions ---
from depend import np
from depend import cost
from method_random import method_random

# --- Data reading --- 
# Option 1 - simulate:
n = 61 #people
k = 14 #groups options

#groups = np.arange(k)  #groups labeled from 0 to k-1
#people = np.arange(n)  #people labeled from 0 to n-1

prefs = np.array([np.random.permutation(range(1, k + 1)) for _ in range(n)])

# Option 2 - read actual data:
#data = pd.read_excel("dados.xlsx")
#data transformation to get a format similar to `alloc`

# --- Allocating --- 
# Some function(s) that receive the preferences and number of groups, and create an allocation

# Option 1 - randomize:
alloc = method_random(n, k, prefs, iters = 10)

obj = cost(alloc, prefs)
print(obj)

# --- Imports and functions ---
from depend import np
from depend import cost
from method_random import method_random

# --- Data reading --- 
# Option 1 - simulate:
n_opt = 4 #groups options
n_obs = 20 #people

prefs = np.array([np.random.permutation(range(1, n_opt + 1)) for _ in range(n_obs)])

# Option 2 - read actual data:
#data = pd.read_excel("dados.xlsx")
#data transformation to get a format similar to `alloc`

# --- Allocating --- 
# Some function(s) that receive the preferences and number of groups, and create an allocation

# Option 1 - randomize:
alloc = method_random(prefs, n_opt, n_obs, k = 10)

obj = cost(alloc, prefs)
print(obj)

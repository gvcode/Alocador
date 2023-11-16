from depend import np
from depend import cost

def method_random(prefs, n_opt, n_obs, k): #k = number of iterations
    obj = float('inf')
    for _ in range(k):
        alloc_try = np.concatenate([np.full(5, i) for i in range(n_opt)])
        np.random.shuffle(alloc_try)
        obj_try = cost(alloc_try, prefs)
        if obj_try < obj:
            obj = obj_try
            alloc = alloc_try
    return alloc

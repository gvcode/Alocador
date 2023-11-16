from depend import np
from depend import cost
from deals import deal_sizes2

def method_random1(n, k, prefs, iters):
    obj = float('inf')
    sizes = deal_sizes2(n, k, prefs)
    for _ in range(iters):
        alloc_try = np.concatenate([np.full(s, i) for (i, s) in zip(range(k), sizes)])
        np.random.shuffle(alloc_try)
        obj_try = cost(alloc_try, prefs)
        if obj_try < obj:
            obj = obj_try
            alloc = alloc_try
    return alloc

# This option also varies the sizes dealt, in a two-way fashion
def method_random2(n, k, prefs, iters_sizes, iter_pop):
    obj = float('inf')
    for _ in range(iters_sizes):
        sizes = deal_sizes2(n, k, prefs)
        for _ in range(iter_pop):
            alloc_try = np.concatenate([np.full(s, i) for (i, s) in zip(range(k), sizes)])
            np.random.shuffle(alloc_try)
            obj_try = cost(alloc_try, prefs)
            if obj_try < obj:
                obj = obj_try
                alloc = alloc_try
    return alloc
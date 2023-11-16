from depend import np
from depend import cost
from deals import deal_sizes2

def method_random(n, k, prefs, iters):
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

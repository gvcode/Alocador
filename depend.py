import numpy as np

# Cost function (we can define other options):
def cost(alloc, prefs):
    index = np.arange(len(alloc)) * prefs.shape[1] + alloc
    return sum(prefs.flat[index]^2) #sum of squares of preferences

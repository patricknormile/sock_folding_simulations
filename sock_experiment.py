# -*- coding: utf-8 -*-
"""
Created on Thu Jun  4 18:58:50 2020

Suppose you have n pairs of socks (2*n socks total) and each sock has one
and only one match. All your socks are in a random pile in your laundry basket.
You remove a sock at random, and if the sock's pair hasn't been pulled you put
the removed sock into a pile of unmatched socks. If the removed sock's pair is 
already in the pile of unmatched socks, you fold both and put them away.

Using this, we can ask questions about the stochastic process X(t) where X is
size of the pile of unmatched socks at step t.


@author: patno_000
"""

import numpy as np
d = range(10)
n = 10
np_prm = np.random.permutation(2 * n)

ordd = []
ordn = []
for i in range(0, 2 * n, 2) :
    ordd.append(i)
    ordd.append(i+1)
    ordn.append(i+ 1)
    ordn.append(i)
    
print(ordn)

sock_map = [ordd, ordn]
print(sock_map)

def sock_prob(n, out) :
    np_prm = np.random.permutation(2 * n)
    pth = [0]
    pull = []
    for x in range(2 * n):
        state = pth[-1]
        pull = np_prm[0:x]
        pair = ordn.index(np_prm[x])
        if pair in pull:
            state = state - 1
        else:
            state = state + 1
        pth.append(state)
  #  print(pth)
    if out == 0 :
        return pth
    elif out == 1 :
        return max(pth)
    else :
        return 0

sock_prob(10, 0)
res = []
for i in range(400000):
    p = sock_prob(10, 1)
    res.append(p)
np_res = np.array(res)

import matplotlib.pyplot as plt
plt.hist(np_res,9)
plt.show()

leq5 = np_res[np_res <= 5]
np.count_nonzero(leq5)/np.count_nonzero(np_res)
len(np_res)
print(range(10))

#######

den = np.arange(0,len(np_res), 1, float) + 1
type(den) 
num = []
for i in range(len(np_res)):
    x = 0
    if len(num) > 0 :
        
        if np_res[i] <= 5:
            x = 1
        else:
            x = 0 
        num.append(num[-1] + x )
    else :
        if np_res[i] <= 5:
            x = 1
        else:
            x = 0 
        num.append( x )


np_num = np.array(num)

np_cumprob = np.true_divide(np_num, den)
np_cumprob[-1]

plt.plot(np_cumprob[-1900000:-1])

#make sure never goes below 0

i = 0
for j in range(100000):
    x = min(sock_prob(10, 0))
    if x < 0 :
        i += 1

print(i)
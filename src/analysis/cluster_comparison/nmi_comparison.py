#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  5 22:32:00 2020

@author: hamishgibbs
"""

'''
Using existing info map clusters for convenience - will update when possible
'''
import pandas as pd
from igraph.clustering import compare_communities
#%%
argv = ['a', '/Users/hamishgibbs/Documents/Covid-19/unequal_mobility_uk/output/spi_m/infomap/cluster_full.csv']

im = pd.read_csv(argv[1])
im['quadkey'] = [f'{n:012}' for n in im.quadkey.values]
#%%
im = im.groupby('date')    
im = [im.get_group(x) for x in im.groups]
#%%
i0 = im[0][['quadkey', 'cluster']].rename(columns = {'cluster':'cluster_0'})

i1 = im[1][['quadkey', 'cluster']].rename(columns = {'cluster':'cluster_1'})
#%%
ic = pd.merge(i0, i1).dropna(axis = 0, how = 'any')

compare_communities(ic['cluster_0'].tolist(), ic['cluster_1'].tolist(), method = 'nmi')

#%%
nmi = []
for i, df in enumerate(im): 
    if i > 0:
        i0 = im[i - 1][['quadkey', 'cluster']].rename(columns = {'cluster':'cluster_0'})

        i1 = im[i][['quadkey', 'cluster']].rename(columns = {'cluster':'cluster_1'})
        
        ic = pd.merge(i0, i1).dropna(axis = 0, how = 'any')

        nmi.append(compare_communities(ic['cluster_0'].tolist(), ic['cluster_1'].tolist(), method = 'nmi'))

#%%
import matplotlib.pyplot as plt
plt.plot(range(0, len(nmi)), nmi)
#%%

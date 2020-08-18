#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  5 22:52:37 2020

@author: hamishgibbs
"""

i0

i1

#%%
comms = {}
for cluster in im[0]['cluster'].unique():
    
    comms[cluster] = im[0].loc[im[0]['cluster'] == cluster, 'quadkey'].tolist()

#%%
neis = {}

for i in range(0, len(im)):
    
    nei = {}
    
    for qk in im[i]['quadkey']:
        
        community = im[i].loc[im[i]['quadkey'] == qk, 'cluster'].tolist()[0]
            
        neighbors = im[i].loc[im[i]['cluster'] == community, 'quadkey'].tolist()
        
        nei[qk] = list(compress(neighbors, [n != qk for n in neighbors]))
    
    neis[i] = nei
#%%
'''
Do this for every time period - make a map of which tiles are likely to switch modules and which are not 
'''
#%%
'''need a way to emphasize a module shift regardless of module size '''
tile_probabilities = {}
for i in range(0, len(neis)):
    
    if i > 0:
        
        prop_change = {}
        for key in neis[i - 1].keys():
            try:
                
                prev_neighbors = neis[i - 1][key]
            
                current_neighbors = neis[i][key]
                
                #did it change?
                shared_neighbors = [n not in current_neighbors for n in prev_neighbors]
                
                prop_change[key] = sum(shared_neighbors) / len(shared_neighbors)
                
            except:
        
                pass
        
        tile_probabilities[i] = prop_change

#%%
dfs = []
for i in range(1, len(tile_probabilities)):
    
    dfs.append(pd.DataFrame({'quadkey':list(tile_probabilities[i].keys()), 'prop':list(tile_probabilities[i].values())}))

df = pd.concat(dfs)

#%%
df.groupby('quadkey').median().to_csv('/Users/hamishgibbs/Downloads/neighbor_membership.csv')



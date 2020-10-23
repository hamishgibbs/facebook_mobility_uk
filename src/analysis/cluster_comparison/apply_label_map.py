#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Oct 17 15:25:13 2020

@author: hamishgibbs
"""

import sys
import __main__ as main
import pandas as pd
from label_mapping import init_mapping, compute_shared_nodes, map_clusters, arbitrate_duplicate_assignment

#%%
if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/infomap/label_map_test.csv']
else:
    argv = sys.argv
#%%
print('Applying Label Map...')

im = pd.read_csv(argv[1], index_col = 0)
im['quadkey'] = ['{0:012d}'.format(n) for n in im['quadkey']]
a3 = pd.read_csv(argv[2])

im = im.groupby('date')    
im = [im.get_group(x) for x in im.groups]

#%%
'''
apply method to each timestep

'''

for i, date in enumerate(im):        
    
    if i > 0:
        t0 = im[i - 1]
        t1 = im[i]
        
        if i == 1:
            t0 = init_mapping(t0)
        
        t0_cluster_ref = t0.groupby('cluster')['quadkey'].apply(list).to_dict()
        t1_cluster_ref = t1.groupby('cluster')['quadkey'].apply(list).to_dict()
        
            
        #need t0_modules and t1_module variables. Dict with list of nodes
        
        cluster_maps = compute_shared_nodes(t0, t1, t0_cluster_ref, t1_cluster_ref)
        
        cluster_maps = arbitrate_duplicate_assignment(cluster_maps, t0_cluster_ref, t1_cluster_ref)
        
        im[i] = map_clusters(cluster_maps, t1)
        
    
    #if i > 5:
    #    break
    print("{}% Done.".format(round((i / len(im)) * 100, 2)))
        
im_mapped = pd.concat(im)

assert sum([len(str(x)) < 10 for x in im_mapped['cluster']]) == 0

im_mapped.to_csv(argv[-1])

print('Success.')

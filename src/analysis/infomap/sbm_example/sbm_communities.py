#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep 15 14:37:43 2020

@author: hamishgibbs
"""

import sys
import __main__ as main
import pandas as pd
import graph_tool as gt
from graph_tool import inference as gti
import numpy as np
#%%

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/analysis/infomap/sbm_example/data/movement_2020_03_2020.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/src/analysis/infomap/sbm_example/processed/sbm.csv']
else:
    argv = sys.argv

#%%
mob = pd.read_csv(argv[1],  dtype = {'start_quadkey':str, 'end_quadkey':str})
#%%
#trying to drop shetland
mob = mob.loc[[x not in ['Orkney Islands', 'Shetland Islands'] for x in mob['NAME_3']], :]
#%%
def od_df(df):
    
    df = df.loc[:,['start_quadkey', 'end_quadkey', 'n_crisis']].rename(columns = {'start_quadkey':'from', 'end_quadkey':'to', 'n_crisis':'weight'})
    
    return(df)

def extract_nested_blockmodel(state, g, node_id, v_map_i):
    
    levels = state.get_levels()
    
    level_states = {}
      
    for i, level in enumerate(levels):
        
        level_states[i] = []
    
        for v in g.vertices():
            
            tmp = levels[i].get_blocks()[v]
            
            level_states[i].append({'quadkey':v_map_i[node_id[v]], 'cluster':tmp, 'level':i})
        
        level_states[i] = pd.DataFrame(level_states[i])
    
    return(pd.concat(level_states))

def communities_sbm(mob):
    
    #create graph
    g = gt.Graph(directed=True)

    weight = g.new_edge_property('int')
    
    vertices = list(np.unique(list(mob['start_quadkey'].unique()) + list(mob['end_quadkey'].unique())))
    
    #map vertex names to integers
    v_map = dict(zip(vertices, range(0, len(vertices))))
    v_map_i = {v: k for k, v in v_map.items()}
    
    #create an edgelist from dataframe (source, target, weight)
    edgelist = od_df(mob)
    
    #trying to convert weights to  integers
    edgelist['weight'] = [int(x) for x in edgelist['weight']]
    
    edgelist['from'] = [v_map[x] for x in edgelist['from']]
    edgelist['to'] = [v_map[x] for x in edgelist['to']]
    
    edgelist = edgelist.to_numpy()
    
    #add edges to graph
    node_id = g.add_edge_list(edgelist, hashed=True, eprops=[weight])
    
    #minimize nester block model
    state = gti.minimize.minimize_nested_blockmodel_dl(g, deg_corr=False, state_args = {'eweight':weight})
    
    #extract partition for each level
    state = extract_nested_blockmodel(state, g, node_id, v_map_i)
    
    return(state)

#%%
sbm = communities_sbm(mob)

#%%
sbm.to_csv(argv[-1])


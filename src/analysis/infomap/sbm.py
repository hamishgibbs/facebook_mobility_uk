#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 30 15:47:19 2020

@author: hamishgibbs
"""

import sys
import __main__ as main
import pandas as pd
import numpy as np
import graph_tool as gt
from graph_tool import inference as gti
from graph_tool import collection as gtc
from graph_tool import draw as gtd
from community_detection import communities_sbm
#%%

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/leiden_full_norm.csv']
else:
    argv = sys.argv

#%%
mob = pd.read_csv(argv[1], dtype = {'start_quadkey':str, 'end_quadkey':str})

#%%
a3 = pd.read_csv(argv[2], dtype = {'quadkey':str})
#%%
mob = pd.merge(mob, a3, left_on = 'start_quadkey', right_on = 'quadkey')
mob = mob.dropna(axis = 0, subset = ['NAME_1'])
#%%

#%%
communities_sbm(mob[0]).to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/sbm_full_norm.csv')

#%%
#%%
def extract_nested_blockmodel(state):
    
    levels = state.get_levels()

    for s in levels:
      print(s)
    
    level_states = {}
      
    for i, level in enumerate(levels):
        
        level_states[i] = []
    
        for v in g.vertices():
            
            tmp = levels[i].get_blocks()[v]
            
            level_states[i].append({'quadkey':v_map_i[node_id[v]], 'cluster':tmp, 'level':i})
        
        level_states[i] = pd.DataFrame(level_states[i])
    
    return(level_states)
#%%
def extract_blockmodel(state, level = 0):
    
    level_state = []
    
    for v in g.vertices():
        
        tmp = state.get_blocks()[v]
        
        level_state.append({'quadkey':v_map_i[node_id[v]], 'cluster':tmp, 'level':level})
    
    return(pd.DataFrame(level_state))
#%%
extract_blockmodel(state).to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/sbm_full_norm.csv')
#%%
pd.concat(list(extract_nested_blockmodel(state).values())).to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/sbm_full_norm.csv')

#%%


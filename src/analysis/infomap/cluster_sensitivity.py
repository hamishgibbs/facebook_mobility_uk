#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 23 20:59:33 2020

@author: hamishgibbs
"""

# normalize by % users per tile
#remove internal flow
# apply info map and leiden
#label mapping for both methods
#make stacked bar plots of different clusterings w/ label mapping

import sys
import __main__ as main
import pandas as pd
from progress.bar import Bar
#%%
if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/tile_12_oa_pop.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full.csv']
else:
    argv = sys.argv

from utils import info_map
    
#%%
mob = pd.read_csv(argv[1], dtype = {'start_quadkey':str, 'end_quadkey':str})
oa_pop = pd.read_csv(argv[2], dtype = {'quadkey_12':str})

#%%
oa_pop['quadkey_12'] = [f'{int(x):012d}' for x in oa_pop['quadkey_12']]

#%%
mean_pop = mob.loc[mob['start_quadkey'] == mob['end_quadkey'], :].groupby(['start_quadkey']).mean().reset_index()[['start_quadkey', 'n_crisis']].rename(columns = {'start_quadkey':'quadkey_12'})

#%%
oa_pop = pd.merge(mean_pop, oa_pop)
oa_pop['fb_prop'] = oa_pop['n_crisis'] / oa_pop['pop']
oa_pop = oa_pop[['quadkey_12', 'fb_prop']]
#%%
mob = mob.loc[mob['start_quadkey'] != mob['end_quadkey'], :]

#%%
mob = pd.merge(oa_pop, mob.rename(columns = {'start_quadkey':'quadkey_12'})).rename(columns = {'quadkey_12':'start_quadkey', 'fb_prop':'start_fb_prop'})
#%%
mob['n_crisis'] = mob['n_crisis'] * mob['start_fb_prop']
#%%














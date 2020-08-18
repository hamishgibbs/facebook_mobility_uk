#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 10 09:39:32 2020

@author: hamishgibbs

Which matrices are closest to eachother?

Then - cluster the matrices to see what are the dominant patterns in travel network over time?

Date x Date matrix of canberra distance between travel matrices

Travel matrices must all have same dimensions, fill absent nodes with all zeros

Then - cluster this matrix - see https://arxiv.org/pdf/2003.01214.pdf

Need - canberra distance measure between matrices

n x n matrices for each day

May need to account for magnitude of travel, may not

"""

import sys
import __main__ as main
import pandas as pd
import numpy as np
from scipy.spatial.distance import canberra
import itertools
#%%
if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/canberra_distance/c_dist_test.csv']
else:
    argv = sys.argv
#%%
mob = pd.read_csv(argv[1], index_col = 0)
a3 = pd.read_csv(argv[2])
#%%
#remove non UK nodes here
mob = pd.merge(mob, a3, left_on='start_quadkey', right_on = 'quadkey').dropna(subset = ['NAME_2'])
#%%
mob['start_quadkey'] = ['{0:012d}'.format(n) for n in mob['start_quadkey']]
mob['end_quadkey'] = ['{0:012d}'.format(n) for n in mob['end_quadkey']]

nodes = list(np.unique(list(mob['start_quadkey'].unique()) + list(mob['end_quadkey'].unique())))

#%%
#list of all possible journeys
journeys = list(mob['journey'].unique())
#%%
mob = mob.groupby('date')    
mob = [mob.get_group(x) for x in mob.groups]
#%%
mob[0]
#%%
#%%
#need to have the same journeys in all time slices - 0

def create_matrix(date_data):
    
    date = list(date_data['date'].unique())
    
    assert len(date) == 1
    
    date = date[0]
    
    df = date_data[['journey', 'n_crisis']]
    df = pd.concat([df, pd.DataFrame({'journey':list(set(journeys).difference(set(df['journey']))), 'n_crisis':0})])
    df = df.sort_values(by = 'journey', ascending = False) 
    df = pd.concat([df['journey'].str.split('_', n = 1, expand = True).rename(columns = {0:'start_quadkey', 1:'end_quadkey'}), df], axis = 1)
    df = df[['start_quadkey', 'end_quadkey', 'n_crisis']]
    df = np.nan_to_num(df.set_index(['start_quadkey', 'end_quadkey'])['n_crisis'].unstack().values)
    
    return({date:df})
#%%
matrices = [create_matrix(df) for df in mob[0:10]]

#test that all mtrices are the same dims
assert len(np.unique([list(x.values())[0].shape for x in matrices])) == 2

matrices = {k:v for element in matrices for k,v in element.items()}

dates = list(matrices.keys())

'''
canberra distance between all combinations of the matrices - if April 6 vs March 6 == March 6 vs April 6,  combinations not permutations (also for performance)

YES MAKE THIS CHANGE ^

'''

date_combos = list(itertools.combinations(dates, 2)) + [(d, d) for d in dates]

c_dist = []
for combo in date_combos:
    c_dist.append({'date_x':combo[0], 'date_y':combo[1], 'c_dist':canberra(matrices[combo[0]].ravel(), matrices[combo[1]].ravel())})

#%%
df = pd.DataFrame(c_dist)
df = pd.concat([df, df.rename(columns={'date_x':'date_y', 'date_y':'date_x'})]).drop_duplicates()
#%%
df.to_csv(argv[-1])














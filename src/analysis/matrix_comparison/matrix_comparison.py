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

NEEDS TO BE FASTER - spend some time improving performance. 

Scipy sparse matrices?
https://docs.scipy.org/doc/scipy/reference/generated/scipy.sparse.dok_matrix.html#scipy.sparse.dok_matrix

use timing decorator to compare

multiprocess?

"""

import sys
import __main__ as main
import pandas as pd
import numpy as np
from scipy.spatial.distance import canberra
import itertools
import time
from scipy import sparse
from progress.bar import Bar

#%%
if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/canberra_distance/c_dist_test.csv']
else:
    argv = sys.argv
#%%
print('Comparison with Canberra distance...')
mob = pd.read_csv(argv[1], dtype = {'start_quadkey':str, 'end_quadkey':str})
a3 = pd.read_csv(argv[2], dtype = {'quadkey':str})
#%%
#remove non UK nodes here
mob = pd.merge(mob, a3, left_on='start_quadkey', right_on = 'quadkey').dropna(subset = ['NAME_2'])
#%%
nodes = list(np.unique(list(mob['start_quadkey'].unique()) + list(mob['end_quadkey'].unique())))
dates = list(mob['date'].unique())
journeys = list(mob['journey'].unique())

#%%
node_ref = dict(zip(nodes, range(len(nodes))))
#%%
#mob = mob.groupby('date')
#mob = [mob.get_group(x) for x in mob.groups]
#%%
def timeit(method):
    def timed(*args, **kw):
        ts = time.time()
        result = method(*args, **kw)
        te = time.time()
        if 'log_time' in kw:
            name = kw.get('log_name', method.__name__.upper())
            kw['log_time'][name] = int((te - ts) * 1000)
        else:
            print ('%r  %2.2f ms' % \
                  (method.__name__, (te - ts) * 1000))
        return result
    return timed
#%%
#need to have the same journeys in all time slices

#approx 700 ms
@timeit
def create_matrix(date_data, norm = False):

    date = list(date_data['date'].unique())

    assert len(date) == 1

    date = date[0]

    df = date_data[['journey', 'n_crisis']]
    df = pd.concat([df, pd.DataFrame({'journey':list(set(journeys).difference(set(df['journey']))), 'n_crisis':0})])
    df = df.sort_values(by = 'journey', ascending = False)
    df = pd.concat([df['journey'].str.split('_', n = 1, expand = True).rename(columns = {0:'start_quadkey', 1:'end_quadkey'}), df], axis = 1)
    df = df[['start_quadkey', 'end_quadkey', 'n_crisis']]
    df = np.nan_to_num(df.set_index(['start_quadkey', 'end_quadkey'])['n_crisis'].unstack().values)

    if norm:
        df = df / df.sum()

    return({date:df})
#%%

@timeit
def map_qks(mob_date):
    
    mob_date = mob_date.copy()
    
    mob_date['start_quadkey'] = [node_ref[x] for x in mob_date['start_quadkey']]
    mob_date['end_quadkey'] = [node_ref[x] for x in mob_date['end_quadkey']]
    
    return(mob_date)
 
#assign quadkeys an integer reference
mob = map_qks(mob)

#%%
mob = mob[['date', 'start_quadkey', 'end_quadkey', 'n_crisis']]
#%%
#%%
@timeit
def create_matrix_csr(matrix):
    
    M = sparse.csr_matrix((matrix['n_crisis'], (matrix['start_quadkey'], matrix['end_quadkey'])), shape=(len(nodes), len(nodes)))
    
    M = M.toarray().ravel()
    
    return(M)
    
    
#%%
mob = mob.sort_values(by = 'date', ascending = True)
#%%
mob_date = mob.groupby('date')
mob_date = [mob_date.get_group(x) for x in mob_date.groups]
m = [create_matrix_csr(x) for x in mob_date]
m = dict(zip(dates, m))
#%%
#matrices = {k:v for element in matrices for k,v in element.items()}
#%%
from scipy.spatial.distance import cdist
#%%
#dates = list(matrices.keys())
from sklearn.neighbors import DistanceMetric
dist = DistanceMetric.get_metric('canberra')
#%%
date_combos = list(itertools.combinations(dates, 2)) + [(d, d) for d in dates]
#%%
@timeit
def canberra_timed(x, y):
    
    return(canberra(x, y))
#%%
c_dist = []
bar = Bar('Processing', max=len(date_combos))
for i, combo in enumerate(date_combos):
    
    c_dist.append({'date_x':combo[0], 'date_y':combo[1], 'c_dist':canberra_timed(m[combo[0]], m[combo[1]])})
    
    bar.next()
bar.finish()
    
df = pd.DataFrame(c_dist)
df.to_csv(argv[-1])
#%%
'''
@timeit
def timed_dist(m):
    
    return(dist.pairwise(m))
#%%
print('Computing pairwise Canberra distance...')
res = timed_dist(m)

dates = mob['date'].unique()

res = pd.DataFrame(res)

res.columns = dates

res.index = dates

res = res.stack().reset_index()
res.columns = ['date_x', 'date_y', 'c_dist']

canberra distance between all combinations of the matrices - if April 6 vs March 6 == March 6 vs April 6,  combinations not permutations (also for performance)

YES MAKE THIS CHANGE ^


date_combos = list(itertools.combinations(dates, 2)) + [(d, d) for d in dates]
#%%
print('Computing canberra distance...')
c_dist = []
for i, combo in enumerate(date_combos):
    
    matrix_a = mob.loc[mob['date'] == combo[0], :]
    matrix_b = mob.loc[mob['date'] == combo[1], :]
    
    matrix_a = create_matrix_csr(matrix_a).toarray()
    matrix_b = create_matrix_csr(matrix_b).toarray()
    
    c_dist.append({'date_x':combo[0], 'date_y':combo[1], 'c_dist':canberra(matrix_a, matrix_b)})

    print(i / len(date_combos))


df = pd.DataFrame(c_dist)
df = pd.concat([df, df.rename(columns={'date_x':'date_y', 'date_y':'date_x'})]).drop_duplicates()

res.to_csv(argv[-1])
print('Success.')
#%%
#df.to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/canberra_distance/c_dist_norm.csv')
'''

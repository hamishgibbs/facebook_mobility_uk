#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 30 14:11:14 2020

@author: hamishgibbs
"""

import sys
import __main__ as main
import pandas as pd
from progress.bar import Bar
import networkx as nx
from networkx.algorithms.community.kclique import k_clique_communities
import igraph as ig
import leidenalg
import numpy as np

#%%

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/leiden_full_norm.csv']
else:
    argv = sys.argv

def od_df(df):
    
    df = df.loc[:,['start_quadkey', 'end_quadkey', 'n_crisis']].rename(columns = {'start_quadkey':'from', 'end_quadkey':'to', 'n_crisis':'weight'})
    
    return(df)


def od_igraph(mob_date):
    g = ig.Graph.TupleList(od_df(mob_date).itertuples(index=False), directed=True, weights=True, edge_attrs=None)
    
    g.vs['id'] = g.vs['name']
    
    return(g)


dates = ['2020-03-19', '2020-04-02', '2020-07-23']
mob = pd.read_csv(argv[1])
mob = mob.loc[mob['start_quadkey'] != mob['end_quadkey'], :]
mob['date'] = pd.to_datetime(mob['date'])
#%%
mob['month'] = [x.month for x in mob['date']]
months = list(np.unique([x.month for x in mob['date']]))
#%%
mob = mob.groupby(['month', 'journey', 'start_quadkey', 'end_quadkey']).sum()['n_crisis'].reset_index()
#%%
#mob = mob.loc[[x in dates for x in mob['month']], :]
mob = mob.groupby('month')    
mob = [mob.get_group(x) for x in mob.groups]
assert len(months) == len(mob)
#%%
mob
#%%
hierarchy = {}
for i, month in enumerate(months):
    
    optimiser = leidenalg.Optimiser()

    G = od_igraph(mob[i])

    rp = optimiser.resolution_profile(G, leidenalg.CPMVertexPartition, min_diff_resolution = 0.01, resolution_range=(0,1), weights='weight')
    
    hierarchy[month] = rp
    
#%%
assert len(months) == len(hierarchy)
hierarchy
#%%
#redo this - one at a time - then abstract 
#in functions not loop list compression is faster?

def collapse_clusters(partition, G):
    
    r = []
    for x in [(i, x) for i, x in enumerate(partition)]:
        r.append([{x[0]: int(a)} for a in x[1]])
    
    cluster_df = pd.concat([pd.DataFrame.from_dict(x, orient = 'index') for x in [item for sublist in r for item in sublist]])
    cluster_df = cluster_df.reset_index().rename(columns = {'index':'cluster', 0:'vertex_index'})
    
    for i in cluster_df['vertex_index']:
        G.vs()[i]
        
    cluster_df['quadkey'] = [G.vs()[int(i)]['name'] for i in cluster_df['vertex_index']]
    cluster_df = cluster_df[['cluster', 'quadkey']]
    
    return(cluster_df)
#%%
all_date_res = []
for a, month in enumerate(months):
    date_res = []
    for i, h in enumerate(hierarchy[month]):
        print(i)
        date_res.append(collapse_clusters(h, od_igraph(mob[a])))
        date_res[i]['resolution'] = h.resolution_parameter
    date_res = pd.concat(date_res)
    date_res['month'] = month
    all_date_res.append(date_res)
#%%
all_date_res = pd.concat(all_date_res)
#%%


#%%
all_date_res.to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/leiden/leiden_hierarchy_months.csv')

















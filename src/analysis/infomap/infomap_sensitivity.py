#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 18 12:06:53 2020

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
#%%
def od_df(df):
    
    df = df.loc[:,['start_quadkey', 'end_quadkey', 'n_crisis']].rename(columns = {'start_quadkey':'from', 'end_quadkey':'to', 'n_crisis':'weight'})
    
    return(df)

def od_graph(df):
    
    df = od_df(df)
    g = nx.from_pandas_edgelist(df, 'from', 'to', ['weight']).to_directed()    
    
    return(g)

def od_igraph(mob_date):
    g = ig.Graph.TupleList(od_df(mob_date).itertuples(index=False), directed=True, weights=True, edge_attrs=None)
    
    g.vs['id'] = g.vs['name']
    
    return(g)

mob = pd.read_csv(argv[1])
mob = mob.loc[mob['start_quadkey'] != mob['end_quadkey'], :]
#mob['date'] = pd.to_datetime(mob['date'])
#%%
#mob['month'] = [x.month for x in mob['date']]
#months = list(np.unique([x.month for x in mob['date']]))
#%%
#mob = mob.groupby(['month', 'journey', 'start_quadkey', 'end_quadkey']).sum()['n_crisis'].reset_index()
#%%
#mob = mob.loc[[x in dates for x in mob['month']], :]
#mob = mob.groupby('month')    
#mob = [mob.get_group(x) for x in mob.groups]
#assert len(months) == len(mob)

#%%
def communities_lei(mob):
    G = od_igraph(m)
    partition = leidenalg.find_partition(G,leidenalg.ModularityVertexPartition, n_iterations = 2, weights='weight')
    cluster = []
    for i, part in enumerate(partition):
        df = pd.DataFrame({'quadkey':G.vs()[part]['name']})
        df['cluster'] = i
        cluster.append(df)
    return(pd.concat(cluster))
    
#%%

mob = mob.groupby('date')    
mob = [mob.get_group(x) for x in mob.groups]

#%%
#%%
clust = []
for m in mob:
    G = od_igraph(m)
    partition = leidenalg.find_partition(G,leidenalg.ModularityVertexPartition, n_iterations = 2, weights='weight')
    cluster = []
    for i, part in enumerate(partition):
        df = pd.DataFrame({'quadkey':G.vs()[part]['name']})
        df['cluster'] = i
        cluster.append(df)
    leiden_test = pd.concat(cluster)
    date = m['date'].unique()[0]
    leiden_test['date'] = date
    clust.append(leiden_test)
    
#%%
pd.concat(clust).to_csv(argv[-1])

#%%
leiden_test.to_csv('/Users/hamishgibbs/Downloads/leiden_test_months.csv')
#%%
list(partition)





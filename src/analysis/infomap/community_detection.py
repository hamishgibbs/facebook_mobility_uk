#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 10 13:11:27 2020

@author: hamishgibbs
"""

import graph_tool as gt
import numpy as np
import pandas as pd
from graph_tool import inference as gti
import igraph as ig
import leidenalg
from infomap import Infomap


def od_df(df):
    
    df = df.loc[:,['start_quadkey', 'end_quadkey', 'n_crisis']].rename(columns = {'start_quadkey':'from', 'end_quadkey':'to', 'n_crisis':'weight'})
    
    return(df)

def od_igraph(mob_date):
    g = ig.Graph.TupleList(od_df(mob_date).itertuples(index=False), directed=True, weights=True, edge_attrs=None)
    
    g.vs['id'] = g.vs['name']
    
    return(g)

def extract_blockmodel(state, g, node_id, v_map_i, level = 0):
    
    level_state = []
    
    for v in g.vertices():
        
        tmp = state.get_blocks()[v]
        
        level_state.append({'quadkey':v_map_i[node_id[v]], 'cluster':tmp, 'level':level, 'method':'sbm'})
    
    return(pd.DataFrame(level_state))

#try to get the working on a subgraph (Scotland)
def communities_sbm(mob):
    
    g = gt.Graph(directed=True)

    weight = g.new_edge_property('int')
    
    vertices = list(np.unique(list(mob['start_quadkey'].unique()) + list(mob['end_quadkey'].unique())))
    
    v_map = dict(zip(vertices, range(0, len(vertices))))
    v_map_i = {v: k for k, v in v_map.items()}
    
    edgelist = od_df(mob)
    edgelist['from'] = [v_map[x] for x in edgelist['from']]
    edgelist['to'] = [v_map[x] for x in edgelist['to']]
    
    edgelist = edgelist.to_numpy()
    
    node_id = g.add_edge_list(edgelist, hashed=True, eprops=[weight])
    
    state = gti.minimize.minimize_blockmodel_dl(g, deg_corr=False)
    
    return(extract_blockmodel(state, g, node_id, v_map_i))

def communities_im(mob, silent = True):
    
    mob = od_df(mob).reset_index(drop = True)
            
    #quadkeys exceed C max values - map nodes to an int value
    unique_qks = np.unique(mob['from'].astype('int').tolist() + mob['to'].astype('int').tolist())
    
    qk_ref = dict(zip(unique_qks, range(0, len(unique_qks))))
        
    qk_ref_i = {v: k for k, v in qk_ref.items()}
        
    im_str = "--two-level --directed --seed 1000"
    
    if silent:
        im_str = im_str + " --silent"
    
        
    im = Infomap(im_str)

    for i in range(0, len(mob['to'])):
        row = mob.loc[i, :]
        
        im.addLink(qk_ref[int(row['from'])], qk_ref[int(row['to'])], row['weight'])
        
    im.run()

    clusters = []
    for node in im.tree:
        if node.is_leaf:
            clusters.append({'quadkey':qk_ref_i[node.node_id], 'cluster':node.module_id, 'flow':node.flow})
            
    return(pd.DataFrame(clusters))

def communities_lei(mob):
    
    G = od_igraph(mob)
    
    partition = leidenalg.find_partition(G,leidenalg.ModularityVertexPartition, n_iterations = 2, weights='weight')
    
    cluster = []
    for i, part in enumerate(partition):
        df = pd.DataFrame({'quadkey':G.vs()[part]['name']})
        df['cluster'] = i
        cluster.append(df)
    return(pd.concat(cluster))

def extract_communities_date(mob, communities, silent = True):
    
    res_l = []
    
    for i, m in enumerate(mob):
        res = communities(m)
        res['date'] = list(m['date'].unique())[0]
        res_l.append(res)
        
        if not silent:
            print(i)
    
    return(pd.concat(res_l))

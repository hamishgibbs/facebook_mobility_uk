#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 13 15:14:52 2020

@author: hamishgibbs
"""

import backbone_network
import networkx as nx
from networkx.algorithms.cluster import average_clustering

def od_df(df):
    
    df = df.loc[:,['start_quadkey', 'end_quadkey', 'n_crisis']].rename(columns = {'start_quadkey':'from', 'end_quadkey':'to', 'n_crisis':'weight'})
    
    return(df)

def od_graph(df):
    
    df = od_df(df)
    g = nx.from_pandas_edgelist(df, 'from', 'to', ['weight']).to_directed()    
    
    return(g)

def get_backbone(df, alpha = 0.8):

    g = od_graph(df)
    
    b = backbone_network.get_graph_backbone(g, alpha_t = alpha)
    
    return(b)

def backbone_summary(bb_network, timepoint_mob, alpha):
    
    og_network = od_graph(timepoint_mob)
    
    bb_journeys = ['_'.join(t) for t in bb_network.edges()] 
    og_journeys = ['_'.join(t) for t in og_network.edges()] 
    
    n_bb_edges = len(bb_journeys)
    n_og_edges = len(og_journeys)
    
    og_flow = timepoint_mob['n_crisis'].sum()
    bb_flow = timepoint_mob.loc[[x in bb_journeys for x in timepoint_mob['journey']], 'n_crisis'].sum()
        
    return({'alpha':alpha, 'n_og_edges':n_og_edges, 'n_bb_edges':n_bb_edges, 'edge_ratio':n_bb_edges / n_og_edges,
            'og_flow':og_flow, 'bb_flow':bb_flow, 'flow_ratio':bb_flow / og_flow, 'date':timepoint_mob['date'].unique()[0],
            'clustering_coefficient':average_clustering(bb_network), 'degree_sequence':sorted([d for n, d in bb_network.degree()], reverse=True)})
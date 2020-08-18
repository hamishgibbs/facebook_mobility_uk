#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug  9 12:07:32 2020

@author: hamishgibbs
"""

import numpy as np
import pandas as pd
from infomap import Infomap

def od_df(df):
    
    df = df.loc[:,['start_quadkey', 'end_quadkey', 'n_crisis']].rename(columns = {'start_quadkey':'from', 'end_quadkey':'to', 'n_crisis':'weight'})
    
    return(df)

def od_df_baseline(df):
    
    df = df.loc[:,['start_quadkey', 'end_quadkey', 'n_baseline']].rename(columns = {'start_quadkey':'from', 'end_quadkey':'to', 'n_baseline':'weight'})
    
    return(df)

def info_map(mob_date, od_df = od_df, include_internal =  False, silent = True):
    
    date = mob_date['date'].unique()[0]
    
    mob_date = od_df(mob_date).reset_index(drop = True)
        
    if not include_internal:
        mob_date = mob_date.loc[mob_date['from'] != mob_date['to'], :]
        mob_date = mob_date.reset_index(drop = True)
            
    #quadkeys exceed C max values - map nodes to an int value
    unique_qks = np.unique(mob_date['from'].astype('int').tolist() + mob_date['to'].astype('int').tolist())
    qk_ref = {}
    for i, qk in enumerate(unique_qks):
        qk_ref[qk] = i
    qk_ref_inv = {v: k for k, v in qk_ref.items()}
    
    if silent:
        im_str = "--two-level --directed --seed 1000 --silent"
    else:
        im_str = "--two-level --directed --seed 1000"
        
    im = Infomap(im_str)

    for i in range(0, len(mob_date['to'])):
        row = mob_date.loc[i, :]
        
        im.addLink(qk_ref[int(row['from'])], qk_ref[int(row['to'])], row['weight'])
        
    im.run()

    clusters = []
    for node in im.tree:
        if node.is_leaf:
            clusters.append({'date':date, 'quadkey':qk_ref_inv[node.node_id], 'cluster':node.module_id, 'flow':node.flow})
            
    return(pd.DataFrame(clusters))

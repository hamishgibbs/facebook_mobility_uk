#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 10 13:52:34 2020

@author: hamishgibbs
"""

#all communities in one file for a given time period
import sys
import __main__ as main
import pandas as pd
import numpy as np
from community_detection import communities_sbm, communities_lei, communities_im, extract_communities_date
from utils import read_mob_data, read_a3_data, select_uk_records, date_agg_split
from itertools import compress

#%%

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv']
else:
    argv = sys.argv

#%%
mob = read_mob_data(argv[1])
a3 = read_a3_data(argv[2])
#%%
mob = select_uk_records(mob, a3)

#%%
#define custom date values here
#mob['date'] = [x.week for x in pd.to_datetime(mob['date'])]
#%%
def chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]

def get_biweek_ref():
    biweeks = list(chunks(list(mob['date'].unique()), 2))
    r = []
    for i, w in enumerate(biweeks):
        for x in w:
            r.append({x: i})
    r = dict((key,d[key]) for d in r for key in d)

    return(r)

#biweek_ref = get_biweek_ref()
#%%
#mob['date'] = [biweek_ref[x] for x in mob['date']]

#%%
interval = 'daily'
#%%
mob = date_agg_split(mob)
#%%
mob
#%%
sbm = extract_communities_date(mob, communities_sbm, False)
#%%
lei = extract_communities_date(mob, communities_lei, False)
#%%
im = extract_communities_date(mob, communities_im, False)
#%%
sbm.to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_biweek/communities_sbm.csv')
lei.to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/communities_biweek/communities_lei.csv')
#%%
im.to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full.csv')
#%%



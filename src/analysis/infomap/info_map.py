#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug  6 07:01:41 2020

@author: hamishgibbs
impo"""
import sys
import __main__ as main
import pandas as pd
from progress.bar import Bar
#%%
if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full_norm_months.csv']
else:
    argv = sys.argv

from utils import info_map
    
#%%
mob = pd.read_csv(argv[1])

#aggregate by month
mob['date'] = pd.to_datetime(mob['date'])
mob['date'] = [x.month for x in mob['date']]
months = list(mob['date'].unique())
#%%
mob = mob.groupby(['date', 'journey', 'start_quadkey', 'end_quadkey']).sum()['n_crisis'].reset_index()
#%%
#mob = mob.loc[[x in dates for x in mob['month']], :]
#mob = mob.groupby('date')    
#mob = [mob.get_group(x) for x in mob.groups]
#assert len(months) == len(mob)
#%%
dates = mob['date'].unique()

bar = Bar('Running Infomap', max=len(dates))

partitions = []

for i, date in enumerate(dates):
            
    mob_date = mob.loc[mob['date'] == date, :]
    
    partitions.append(info_map(mob_date))
    
    bar.next()
    
    print(i / len(dates))

partitions = pd.concat(partitions)
partitions.to_csv(argv[-1])

















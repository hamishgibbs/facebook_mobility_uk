#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug  6 07:01:41 2020

@author: hamishgibbs
"""
import sys
import __main__ as main
import pandas as pd
from progress.bar import Bar
#%%
if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full.csv']
else:
    argv = sys.argv

from utils import info_map
    
#%%
mob = pd.read_csv(argv[1])

#%%
dates = mob['date'].unique()

bar = Bar('Running Infomap', max=len(dates))

partitions = []

for i, date in enumerate(dates):
            
    mob_date = mob.loc[mob['date'] == date, :]
    
    partitions.append(info_map(mob_date))
    
    bar.next()

partitions = pd.concat(partitions)
partitions.to_csv(argv[-1])

















#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 10 22:31:01 2020

@author: hamishgibbs
"""

import __main__ as main
import sys
import numpy as np
import pandas as pd
import glob
#%%

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/Britain_AdminMovement',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/admin_mobility_hours.csv']
else:
    argv = sys.argv

#%%
fns = glob.glob(argv[1] + '/*.csv')

def preprocess_mobility(fn):
    
    mob = pd.read_csv(fn)
    
    mob = mob.loc[mob['country'] == 'GB', :]
    
    mob = mob[['geometry', 'date_time', 'start_polygon_id', 'start_polygon_name', 'end_polygon_id', 'end_polygon_name', 'n_crisis', 'n_baseline']]
    
    mob['journey'] = mob['start_polygon_id'].astype(str) + '_' + mob['end_polygon_id'].astype(str)
    
    return(mob)
    
    
#%%
print('Processing AdminMovement...')
pd.concat([preprocess_mobility(fn) for fn in fns]).to_csv(argv[-1])
print('Done.')
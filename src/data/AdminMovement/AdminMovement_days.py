#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 10 22:47:36 2020

@author: hamishgibbs
"""

import sys
import pandas as pd

#%%
argv = sys.argv

mob = pd.read_csv(argv[1])
#%%
print('Combining AdminMovement...')
mob['date'] = [dt[0:10] for dt in mob['date_time'].astype(str)]
mob['date'] = pd.to_datetime(mob['date'])
#%%
mob = mob[['journey', 'date', 'start_polygon_id', 'end_polygon_id', 'n_crisis', 'n_baseline']]
#%%
mob = mob.groupby(['journey', 'date', 'start_polygon_id', 'end_polygon_id']).sum().reset_index()
#%%
mob.to_csv(argv[2])
print('Done.')
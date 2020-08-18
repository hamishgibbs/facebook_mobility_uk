#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 13:43:31 2020

@author: hamishgibbs
"""

import sys
import pandas as pd
from utils import preprocess_mobility

#%%
argv = sys.argv

mob = preprocess_mobility(argv[1])
#%%
mob['journey'] = mob['start_quadkey'].astype(str) + '_' + mob['end_quadkey'].astype(str)
#%%
mob['date'] = [dt[0:10] for dt in mob['date_time'].astype(str)]
mob['date'] = pd.to_datetime(mob['date'])
#%%
mob = mob[['journey', 'date', 'start_quadkey', 'end_quadkey', 'n_crisis', 'n_baseline']]
#%%
mob = mob.groupby(['journey', 'date', 'start_quadkey', 'end_quadkey']).sum().reset_index()
#%%
mob.to_csv(argv[2])
    
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

#%%
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
#old_mob = pd.read_csv(argv[2])
#old_mob = old_mob[['journey', 'date', 'start_quadkey', 'end_quadkey', 'n_crisis', 'n_baseline']]
#%%
#combine with old mobility data
#old_mob['date'] = pd.to_datetime(old_mob['date'])

#old_mob = old_mob.loc[old_mob['date'] < mob['date'].min(), :]
#%%
#mob = pd.concat([old_mob, mob])
#%%
mob.to_csv(argv[-1])
    
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 14:01:47 2020

@author: hamishgibbs
"""


import sys
import pandas as pd
from utils import preprocess_population

#%%
argv = sys.argv
#%%
pop = preprocess_population(argv[1])
#%%
pop['date'] = [dt[0:10] for dt in pop['date_time'].astype(str)]
pop['date'] = pd.to_datetime(pop['date'])
#%%
pop = pop[['date', 'quadkey', 'n_crisis', 'n_baseline']]
#%%
pop = pop.groupby(['date', 'quadkey']).median().reset_index()
#%%
pop.to_csv(argv[2])
    
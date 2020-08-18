#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 11:44:24 2020

@author: hamishgibbs
"""


import __main__ as main
import os
import sys
import pickle

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/params/date_range.pickle',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv']
else:
    argv = sys.argv
    sys.path.append('/'.join(os.path.dirname(main.__file__).split('/')[:-2]))
    from utils.movement_utils import preprocess_mobility


date_range = pickle.load(open(argv[1], 'rb'))

mob = preprocess_mobility(argv[2], parse_dates = ['date'], index_col = 0)

#%%
mob_internal = mob.loc[mob['start_quadkey'] == mob['end_quadkey'], :]

#%%
for i, date in enumerate(date_range):
    if i > 0:
        mob.loc[(mob['date'] >= date_range[i - 1]) & (mob['date'] <= date_range[i]), 'period'] = int(i)
#%%
mob.groupby(['journey', 'period']).sum().reset_index().to_csv(argv[-1], index = False)

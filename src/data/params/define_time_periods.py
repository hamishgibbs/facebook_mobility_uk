#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 11:45:37 2020

@author: hamishgibbs

Defining a simple division of four equal time periods here
"""


import __main__ as main
import sys
import pandas as pd
import pickle

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv']
else:
    argv = sys.argv
    
mob = pd.read_csv(argv[1], parse_dates = ['date'])

#%%
dates = mob['date'].unique()

date_range = pd.date_range(min(dates), max(dates), 5)

pickle.dump(date_range, open(argv[-1], 'wb'))
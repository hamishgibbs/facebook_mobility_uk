#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Oct 17 14:42:34 2020

@author: hamishgibbs

Script to extract communities from Facebook movement using InfoMap algorithm.

Identifies daily communities. 

"""

import sys
import __main__ as main
from community_detection import communities_im, extract_communities_date
from utils import read_mob_data, read_a3_data, select_uk_records, date_agg_split

#%%

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/infomap/infomap_full.csv']
else:
    argv = sys.argv
    

# Read mob and admin level 3 data
mob = read_mob_data(argv[1])
a3 = read_a3_data(argv[2])
#%%
# Drop international records
mob = select_uk_records(mob, a3)
#%%

# Group by date and return a list of dates
mob = date_agg_split(mob)
#%%
# Extract communities with InfoMap algorithm
im = extract_communities_date(mob, communities_im, silent = False)

#%%
im.to_csv(argv[-1])
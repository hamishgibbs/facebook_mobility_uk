#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 14:23:30 2020

@author: hamishgibbs
"""


import sys
import numpy as np
import geopandas as gpd
from utils import preprocess_population, tile_polygon

#%%
argv = sys.argv
#%%
pop = preprocess_population(argv[1])
#%%
quadkeys = list(np.unique(pop['quadkey'])) 
#%%
qk_ref = gpd.GeoDataFrame({'quadkey':quadkeys}, geometry = [tile_polygon(qk) for qk in quadkeys])

qk_ref.to_file(argv[2])
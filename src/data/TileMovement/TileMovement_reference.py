#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 14:27:01 2020

@author: hamishgibbs

Create a shapefile of all movement tiles
"""


import sys
import numpy as np
import geopandas as gpd
from utils import preprocess_mobility, tile_polygon
#%%
argv = sys.argv
#%%
mob = preprocess_mobility(argv[1])
#%%
quadkeys = list(np.unique(list(np.unique(mob['start_quadkey'])) + list(np.unique(mob['end_quadkey']))))
#%%
qk_ref = gpd.GeoDataFrame({'quadkey':quadkeys}, geometry = [tile_polygon(qk) for qk in quadkeys])
#%%
qk_ref.to_file(argv[2])
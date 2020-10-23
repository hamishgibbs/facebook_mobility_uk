#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 17:08:25 2020

@author: hamishgibbs
"""

import __main__ as main
import sys
import geopandas as gpd

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/gadm36_GBR_shp/gadm36_GBR_3.shp',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/tile_reference/tiles_zoom_13.shp',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/oa_reference/oa_tile_reference_13.csv']
else:
    argv = sys.argv
#%%
a3 = gpd.read_file(argv[1])
a3 = a3.to_crs("EPSG:27700")

tiles = gpd.read_file(argv[2])
tiles.crs = 4326
#%%
tiles['geometry'] = [pt.centroid for pt in tiles['geometry']]
tiles = tiles.to_crs("EPSG:27700")

#%%
print('Extracting admin level 3 to tiles')
intersect = gpd.overlay(tiles, a3, how='intersection')
intersect = intersect[['quadkey', 'GID_0', 'NAME_0', 'GID_1', 'NAME_1', 'GID_2', 'NAME_2', 'GID_3', 'NAME_3']]

#%%
intersect.to_csv(argv[-1])

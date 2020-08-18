#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 10 22:31:01 2020

@author: hamishgibbs

This geographytrick sort of works. 

Could also try to work with the namestack to get it to a known collection of admin units

"""

import __main__ as main
import sys
import numpy as np
import pandas as pd
import geopandas as gpd
import glob
from shapely.geometry import Polygon
#%%

if not hasattr(main, '__file__'):
    argv = ['code', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/admin_mobility_hours.csv',
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/raw/gadm36_GBR_shp/gadm36_GBR_3.shp',
            '/Users/hamishgibbs/Downloads/Parish__December_2017__Boundaries_in_England_and_Wales-shp/Parish__December_2017__Boundaries_in_England_and_Wales.shp']
else:
    argv = sys.argv

#%%
mob = pd.read_csv(argv[1])
#%%
a3 = gpd.read_file(argv[2])
parish = gpd.read_file(argv[3])
#%%
len(set(mob['start_polygon_name'].tolist()).intersection(set(parish['par17nm'])))
#%%
unique_geo = mob[['journey', 'geometry', 'n_crisis']].groupby(['journey', 'geometry']).count().reset_index()
#%%
unique_geo.loc[unique_geo['n_crisis'] > 1, ]
#%%
mob.groupby('journey')['geometry'].nunique()
#%%
#points are modal location, not centroid
#%%
len(a3['NAME_3'].unique())
#%%
len(mob['start_polygon_name'].unique())

#%%
mob_geom = mob['geometry'].str.split(' ', expand=True)
#%%
mob_geom['start_polygon_id'] = mob['start_polygon_id']
mob_geom['end_polygon_id'] = mob['end_polygon_id']
#%%
mob_geom = mob_geom.rename(columns = {1:'start_lon', 2:'start_lat', 3:'end_lon', 4:'end_lat'})[['start_polygon_id', 'end_polygon_id', 'start_lon', 'start_lat', 'end_lon', 'end_lat']]
#%%
mob_geom['start_lon'] = remove_chars(mob_geom['start_lon'])
mob_geom['start_lat'] = remove_chars(mob_geom['start_lat'])
mob_geom['end_lon'] = remove_chars(mob_geom['end_lon'])
mob_geom['end_lat'] = remove_chars(mob_geom['end_lat'])
#%%
mob_geom = pd.concat([mob_geom[['start_polygon_id', 'start_lon', 'start_lat']].rename(columns={'start_polygon_id':'polygon_id', 'start_lon':'lon', 'start_lat':'lat'}),
           mob_geom[['end_polygon_id', 'end_lon', 'end_lat']].rename(columns={'end_polygon_id':'polygon_id', 'end_lon':'lon', 'end_lat':'lat'})])
#%%
min_df = mob_geom.groupby(['polygon_id']).min().reset_index().rename(columns = {'lon':'min_x', 'lat':'min_y'})
max_df = mob_geom.groupby(['polygon_id']).max().reset_index().rename(columns = {'lon':'max_x', 'lat':'max_y'})
geom_df = pd.merge(min_df, max_df)
#%%
row = geom_df.loc[0, :]

def create_row_poly(row):

    return(Polygon([[row['min_x'], row['min_y']], [row['min_x'], row['max_y']], [row['max_x'], row['max_y']], [row['max_x'], row['min_y']]]))
#%%
geom = []
for index, row in geom_df.iterrows():
    geom.append({'polygon_id':row['polygon_id'], 'geometry':create_row_poly(row)})
#%%
g_df = gpd.GeoDataFrame(pd.DataFrame(geom))
g_df.crs = 4326
g_df.to_file('/Users/hamishgibbs/Downloads/admin_geography_test.shp')

qgis
 #%%
def remove_chars(s):
    
    s = s.str.replace('(', '')
    s = s.str.replace(',', '')
    s = s.str.replace(')', '')
    
    return(pd.to_numeric(s))
#%%
Maximum longitude per polygon
Minimum longitude per polygon
etc.
Make this into a box
Union
#%%
#This is why this data is not used. 
#Points are modal locations, not centroids
#admin reference polygons are a combination of different admin levels + some that don't exist
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 23 06:50:19 2020

@author: hamishgibbs
"""

import os
import glob
from dotenv import load_dotenv
from sqlalchemy import create_engine
import pandas as pd
import mysql.connector
from pyquadkey2 import quadkey as qk
import folium
from src.utils.mob_utils import tile_polygon
#%%
load_dotenv()
db = create_engine(os.getenv("DATABASE_URL"))    
#%%
mob = pd.read_sql('select distinct quadkey from fb_population order by length(quadkey);', con=db)
#%%
pd.read_sql('select TABLE_SCHEMA from fb_population;', con=db)
#%%
#%%
mob = pd.read_sql('select distinct start_quadkey from mobility where date_time = ( SELECT MIN(date_time) FROM mobility );', con=db)
#%%
db.execute('drop table fb_mobility')
#%%which dates have the incorrect 12
#%%
mob_fn = glob.glob('/Users/hamishgibbs/Documents/Covid-19/covid_facebook_mobility/data/Facebook_Data/Britain_population/*.csv')
table_name = 'fb_population'
#%%
for i in range(len(mob_fn)):
    df = preprocess_population(mob_fn[i])
    try:
    
        frame = df.to_sql(table_name, db, if_exists='append');
    
    except ValueError as vx:
    
        print(vx)
    
    except Exception as ex:   
    
        print(ex)
    
    else:
    
        print("Table {} {} of {} created successfully.".format(table_name, i + 1, len(mob_fn))); 
#%%
#preprocess mobility 
def preprocess_mobility(fn):
    df = pd.read_csv(fn)
    df['start_quadkey'] = df['start_quadkey'].apply(lambda x: '{0:0>12}'.format(x))
    df['end_quadkey'] = df['end_quadkey'].apply(lambda x: '{0:0>12}'.format(x))    
    return(df)
def preprocess_population(fn):
    df = pd.read_csv(fn)
    df['quadkey'] = df['quadkey'].apply(lambda x: '{0:0>13}'.format(x))    
    return(df)    
#%%
#create la lookup, OA pop lookup, and country lookup, geodata for quadkeys
#quadkeys change zoom level?
#these could be quadkeys with 0 at start that have been removed
mob['start_quadkey_len'] = mob['start_quadkey'].astype(str).apply(len)
mob['end_quadkey_len'] = mob['end_quadkey'].astype(str).apply(len)
#%%
pd.testing.assert_series_equal(mob['start_quadkey_len'], mob['end_quadkey_len'])
#%%
mob.loc[mob['start_quadkey_len'] < 12, 'date_time'].unique()
#%%
a = mob.groupby(['date_time', 'start_quadkey_len']).count()
#%%
#%%what proportion of quadkeys are len 11
len(mob.loc[mob['start_quadkey_len'] == 11, 'date_time']) / len(mob['start_quadkey_len'])
#%%
geo = []
for q in mob['quadkey']:
        
    try:
        geo.append({'quadkey':q, 'geometry':tile_polygon(qk.from_str(q)).__geo_interface__})
    except:
        continue
geo
#%%

#check if length 11 qks are in the uk or if they need a 0 before
#%%
m = folium.Map(
    location=[54.127186, -1.992395],
    zoom_start=6,
    tiles='Stamen Terrain'
)

for json in geo:
    folium.GeoJson(
        json['geometry'],
        tooltip = json['quadkey'],
        name='geojson'
    ).add_to(m)

m.save('/Users/hamishgibbs/Documents/Covid-19/unequal_mobility_uk/output/tmp/index.html')
#%%

'''
need ref table for 13 quadkey to 12
need ref tabel for is centroid in uk or not
need ref table for centorid country
neef ref table for la of centroid
'''

'''
Actually sit down and lay out the analysis - then write the code
use notes from the meetings and roz messages to get an idea of what is wanted
layout analysis 
then write code to fit it

'''




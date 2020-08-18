#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 12:32:17 2020

@author: hamishgibbs
"""

import pandas as pd
from pyquadkey2 import quadkey
from shapely.geometry import Polygon

def preprocess_population(fn):
    df = pd.read_csv(fn)
    df['quadkey'] = df['quadkey'].apply(lambda x: '{0:0>13}'.format(x))    
    df['n_crisis'] = pd.to_numeric(df['n_crisis'], errors = 'coerce')
    df['n_baseline'] = pd.to_numeric(df['n_baseline'], errors = 'coerce')
    
    return(df)  

def tile_polygon(qk):
    
    qk = quadkey.QuadKey(str(qk))
    
    a1 = qk.to_geo(anchor = 1)
    a2 = qk.to_geo(anchor = 2)
    a3 = qk.to_geo(anchor = 3)
    a4 = qk.to_geo(anchor = 5)    
    
    bottom_l = [a1[1], a1[0]]
    bottom_r = [a4[1], a4[0]]
    top_l = [a3[1], a3[0]]
    top_r = [a2[1], a2[0]]
    
    return(Polygon([bottom_l, bottom_r, top_r, top_l]))


def tile_centroid(qk):
    
    p = tile_polygon(qk).centroid
    
    return(p)
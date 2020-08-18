#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 12:32:01 2020

@author: hamishgibbs
"""

import sys
import glob
import pandas as pd
from progress.bar import Bar

from utils import preprocess_population
#%%
argv = sys.argv

fns = glob.glob(argv[1] + '/*.csv')

pop = []
bar = Bar('Preprocessing TilePopulation', max=len(fns))
for fn in fns:
    pop.append(preprocess_population(fn))
    bar.next()
    
pd.concat(pop).to_csv(argv[2])
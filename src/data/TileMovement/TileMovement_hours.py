#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Aug  3 11:57:17 2020

@author: hamishgibbs

"""

import sys
import glob
import pandas as pd
from progress.bar import Bar

from utils import preprocess_mobility
#%%
argv = sys.argv

fns = glob.glob(argv[1] + '/*.csv')

mob = []
bar = Bar('Preprocessing TileMovement', max=len(fns))
for fn in fns:
    mob.append(preprocess_mobility(fn))
    bar.next()

    
mob = pd.concat(mob)

mob.to_csv(argv[2])
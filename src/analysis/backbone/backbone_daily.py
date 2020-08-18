#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 13 15:05:25 2020

@author: hamishgibbs
"""

import __main__ as main
import sys
import pandas as pd
from utils import get_backbone, backbone_summary
import numpy as np
import time

if(not hasattr(main, '__file__')):
    argv = ['none', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/interim/mobility_days.csv', 
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/backbone_summary.csv']
else:
    argv = sys.argv

#%%
mob = pd.read_csv(argv[1], dtype={'start_quadkey':str, 'end_quadkey':str})
mob = mob.loc[mob['start_quadkey'] != mob['end_quadkey']]
#%%
mob = mob.groupby('date')    
mob = [mob.get_group(x) for x in mob.groups]
#%%

def timeit(method):
    def timed(*args, **kw):
        ts = time.time()
        result = method(*args, **kw)
        te = time.time()
        if 'log_time' in kw:
            name = kw.get('log_name', method.__name__.upper())
            kw['log_time'][name] = int((te - ts) * 1000)
        else:
            print('%r  %2.2f ms' % (method.__name__, (te - ts) * 1000))
        return result
    return timed

@timeit
def alpha_sensitivity(mob, increment = 0.1):
    
    summary_data = []

    alphas = np.arange(0, 1 + increment, increment).tolist()
    summary_data = []
    for alpha in alphas:
        b = get_backbone(mob, alpha)
        summary_data.append(backbone_summary(b, mob, alpha))
        print('Alpha {} done'.format(alpha))
    
    return(summary_data)
    
#%%

summaries = [alpha_sensitivity(x, 0.1) for x in mob[0:1]]
#%%
pd.concat([pd.DataFrame(x) for x in summaries]).to_csv(argv[-1])
#%%
print('alpha = 0.1, Expected execution: {} minutes.'.format(round(len(mob) * (65391.46 / 1000 / 60), 2)))
print('alpha = 0.05, Expected execution: {} minutes.'.format(round(len(mob) * (130302.41 / 1000 / 60), 2)))
print('alpha = 0.01, Expected execution: {} minutes.'.format(round(len(mob) * (667901.12 / 1000 / 60), 2)))






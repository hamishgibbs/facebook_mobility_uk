#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 13 15:05:25 2020

@author: hamishgibbs
"""

import __main__ as main
import sys
import pandas as pd
from backbone_utils import get_backbone, backbone_summary
import numpy as np
import time
from utils import read_mob_data, read_a3_data, select_uk_records, date_agg_split

if(not hasattr(main, '__file__')):
    argv = ['none', '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/mobility_days_norm.csv', 
            '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/la_reference/a3_tile_reference.csv',
                        '/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/backbone_summary.csv']
else:
    argv = sys.argv

#%%
mob = pd.read_csv(argv[1], dtype={'start_quadkey':str, 'end_quadkey':str})
mob = mob.loc[mob['start_quadkey'] != mob['end_quadkey']]
#%%
dates = ['2020-03-19', '2020-04-12']
mob = mob.loc[[x in dates for x in mob['date']]]
#%%
mob = mob.groupby('date')    
mob = [mob.get_group(x) for x in mob.groups]
#%%
mob = read_mob_data(argv[1])
a3 = read_a3_data(argv[2])
#%%
mob = select_uk_records(mob, a3)
#%%

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
mob = [mob[0], mob[3], mob[6], mob[9]]
#%%
summaries = [alpha_sensitivity(x, 0.01) for x in mob]
#%%
pd.concat([pd.DataFrame(x) for x in summaries]).to_csv(argv[-1])
#%%
print('alpha = 0.1, Expected execution: {} minutes.'.format(round(len(mob) * (65391.46 / 1000 / 60), 2)))
print('alpha = 0.05, Expected execution: {} minutes.'.format(round(len(mob) * (130302.41 / 1000 / 60), 2)))
print('alpha = 0.01, Expected execution: {} minutes.'.format(round(len(mob) * (667901.12 / 1000 / 60), 2)))


#%%
alpha_focus = [
    {'alpha':0.23, 'mob':mob[0]},
    {'alpha':0.27, 'mob':mob[2]},
    {'alpha':0.24, 'mob':mob[1]},
    ]
#%%
#%%
bb = []
bb_df = []
for i in range(0, len(alpha_focus)):
    bb.append(get_backbone(alpha_focus[i]['mob'], alpha_focus[i]['alpha']))
    bb_df.append(backbone_df(bb[i], dates[i]))
#%%
pd.concat(bb_df).to_csv('/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/processed/backbone/backbone_date_focus.csv')
#%%
bb_journeys = ['_'.join(t) for t in bb[0].edges()] 

def backbone_df(bb, date):
    df = pd.DataFrame({'journey':['_'.join(t) for t in bb.edges()]})
    df['date'] = date
    return(df)
#%%

backbone_df(bb[0], dates[0])
#%%

#%%
get_backbone(alpha_focus[0]['mob'], alpha_focus[0]['alpha'])



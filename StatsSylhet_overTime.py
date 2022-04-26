import pandas as pd
import os
from Collators import Panelist, Flooder, Statistics, Visualiser
import matplotlib.pyplot as plt
import numpy as np
import warnings
import plotly.express as px
warnings.simplefilter(action='ignore', category=FutureWarning)
import json

def get_json(path):
    """ Function for loading JSON files """
    with open(path, "r") as f:
        itn = json.load(f)
    return itn

def get_flood(file_names):
    flood_df = pd.DataFrame(columns=['dov', 'floodedAreakm2', 'sylhet_areakm2'])

    for i in file_names:
        prop = pd.DataFrame(columns=['dov', 'floodedAreakm2', 'sylhet_areakm2'])
        x = get_json(i)
        prop['dov'] = x['features'][0]['properties']['dates']
        prop['floodedAreakm2'] = x['features'][0]['properties']['floodedAreakm2']
        prop['sylhet_areakm2'] = x['features'][0]['properties']['sylhet_area']

        flood_df = flood_df.append(prop, ignore_index=True)

    return flood_df

os.chdir('C:/Users/offne/Documents/GitHub/FAARM_Analysis/Data/ClusterBound_tuned')

surv = ['P2.geojson', 'P3.geojson', 'P4.geojson', 'P5.geojson',
        'P6.geojson', 'P7.geojson', 'P8.geojson', 'P9.geojson']

flood_df = get_flood(surv)
flood_df['dov'] = pd.to_datetime(flood_df['dov'], errors='coerce')
flood_df = Panelist(flood_df).get_dd_mm_yyyy()

# Get percent coverage
flood_df['perc_flooded'] = flood_df['floodedAreakm2']/flood_df['sylhet_areakm2']

# Check Years
ystat_r = Statistics(flood_df).stats_by_time('perc_flooded', 'year', range(2016, 2020, 1))

# Check Months
mstat_r = Statistics(flood_df).stats_by_time('perc_flooded', 'month', range(1, 13, 1))

# Compare months
flood_df16 = flood_df[flood_df['year'] == 2016]
flood_df17 = flood_df[flood_df['year'] == 2017]
flood_df18 = flood_df[flood_df['year'] == 2018]
flood_df19 = flood_df[flood_df['year'] == 2019]
mstat_r16 = Statistics(flood_df16).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
mstat_r17 = Statistics(flood_df17).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
mstat_r18 = Statistics(flood_df18).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
mstat_r19 = Statistics(flood_df19).stats_by_time('perc_flooded', 'month', range(1, 13, 1))


# Calc perc increase of max - annual // print(0.47 - 0.38)

# Calculate change scores for maximums to identify seasonal trends
x = mstat_r19 - mstat_r16
x['month'] = mstat_r16['month']

# Between 2016-2019, the maximum percent of flood coverage in Sylhet increased by 9%
# There are noticable differences for Jan/Feb and May/Junebetween 2016 and 2019
# - January & Feb saw a respective 13% and 9% decrease in flooding on average.
# - May & June saw a respective 12% and 9% decrease in flooding on average.

# By month, data refelcted known dry seasons (Feb-April) and monsoon nseasons (June-Sept)
# - Spike in Jan2016 indicated 29% coverage compared to mean of 0.17
# - Spike in April2017 indicated 14% coverage compared to 0.05 average
# - 2018 stays true to trend
# - Extended dry period of ~0.08% between Feb2019 and June2019 (normally ~4%)



# ymstat = pd.DataFrame(columns=['year', 'month', 'max', 'sd', 'mean', 'kurt', 'skew', 'count', 'num_img'])
# mstat_r16['year'] = 2016
# ymstat = ymstat.append(mstat_r16, ignore_index=True)
# mstat_r17['year'] = 2017
# ymstat = ymstat.append(mstat_r17, ignore_index=True)
# mstat_r18['year'] = 2018
# ymstat = ymstat.append(mstat_r18, ignore_index=True)
# mstat_r19['year'] = 2019
# ymstat = ymstat.append(mstat_r19, ignore_index=True)
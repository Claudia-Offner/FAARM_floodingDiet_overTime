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

os.chdir(r'C:\Users\offne\OneDrive - London School of Hygiene and Tropical Medicine\2. Research\C. FAARM\- DD-Flooding TimeSeries - CO\4. Data\20220223_SylhetDiv_250mScale_AreaKm2_tuned')

surv = ['P2.geojson', 'P3.geojson', 'P4.geojson', 'P5.geojson',
        'P6.geojson', 'P7.geojson', 'P8.geojson', 'P9.geojson']

flood_df = get_flood(surv)

flood_df['dov'] = pd.to_datetime(flood_df['dov'], errors='coerce')
flood_df = Panelist(flood_df).get_dd_mm_yyyy()

# Get percent coverage
flood_df['perc_flooded'] = flood_df['floodedAreakm2']/flood_df['sylhet_areakm2']

# Create year_month col
flood_df['month'] = flood_df['month'].astype(int).apply(lambda x: '{0:0>2}'.format(x))
flood_df['year_month'] = flood_df['year'].astype(str) + '-' + flood_df['month'].astype(str)
flood_df.drop('day', axis=1, inplace=True)
flood_df['month'] = flood_df['month'].astype(int)

# Create seasons cols
season_DD = {1:'Jan/Feb', 2:'Jan/Feb', 3:'Mar/Apr', 4: 'Mar/Apr', 5:'May/Jun', 6:'May/Jun',
                7:'Jul/Aug', 8:'Jul/Aug', 9:'Sept/Oct', 10:'Sept/Oct', 11:'Nov/Dec', 12:'Nov/Dec'}
season_nums = {'Jan/Feb':1 , 'Mar/Apr':2, 'May/Jun':3, 'Jul/Aug':4, 'Sept/Oct':5, 'Nov/Dec':6}
season_names = {1:'Jan/Feb', 2:'Mar/Apr',3:'May/Jun', 4:'Jul/Aug', 5:'Sept/Oct', 6:'Nov/Dec'}
flood_df['season_DD'] = flood_df['month']
flood_df['season_DD'] = flood_df['season_DD'].replace(season_DD)
flood_df['season'] = flood_df['season_DD']
flood_df['season'] = flood_df['season'].replace(season_nums)
flood_df['year_season'] = flood_df['year'].astype(str) + '-' + flood_df['season'].astype(str)


# Check Years
ystat_r = Statistics(flood_df).stats_by_time('perc_flooded', 'year', range(2016, 2020, 1))

# Check Months
mstat_r = Statistics(flood_df).stats_by_time('perc_flooded', 'month', range(1, 13, 1)).add_prefix('avMonthly_').rename(columns = {'avMonthly_month':'month'})

# Check Seasons
sstat_r = Statistics(flood_df).stats_by_time('perc_flooded', 'season', range(1, 7, 1)).add_prefix('avSeason_').rename(columns = {'avSeason_season':'season'})

# Group by year-month
# flood_df = flood_df.groupby(['year_month'], as_index=False).mean()  #panel
# df = pd.merge(flood_df, mstat_r, how='left', on=['month']).drop(['avMonthly_count'], axis=1)
# df['diff'] = df['perc_flooded']-df['avMonthly_mean']

# Group by year-season
flood_df = flood_df.groupby(['year_season'], as_index=False).mean()  #panel
df = pd.merge(flood_df, sstat_r, how='left', on=['season']).drop(['avSeason_count'], axis=1)
df['diff'] = df['perc_flooded']-df['avSeason_mean']


# VISUALISE
# df.plot('year_month',y='diff')
# plt.axhline(y = 0, color = 'r', linestyle = '-')
# plt.axhline(y = 0.05, color = 'r', linestyle = 'dashed')
# plt.axhline(y = -0.05, color = 'r', linestyle = 'dashed')
# plt.show()
#
# df.plot('year_month', y=['perc_flooded', 'avMonthly_mean'])
# plt.show()

df.to_csv('poster_season.csv', index=False)
ystat_r.to_csv('poster_year.csv', index=False)









#%%

#### OTHER #####

# # Compare months
# flood_df16 = flood_df[flood_df['year'] == 2016]
# flood_df17 = flood_df[flood_df['year'] == 2017]
# flood_df18 = flood_df[flood_df['year'] == 2018]
# flood_df19 = flood_df[flood_df['year'] == 2019]
# mstat_r16 = Statistics(flood_df16).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
# mstat_r17 = Statistics(flood_df17).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
# mstat_r18 = Statistics(flood_df18).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
# mstat_r19 = Statistics(flood_df19).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
#
#
# # Calc perc increase of max - annual // print(0.47 - 0.38)
#
# # Calculate change scores for maximums to identify seasonal trends
# x = mstat_r19 - mstat_r16
# x['month'] = mstat_r16['month']

# Add weights
# for i in [1, 2, 3, 4]:
#     x = df.loc[df['month'] == i, 'perc_flooded'] * 1.5
#     df.loc[df['month'] == i, 'perc_flooded'] = x

# Between 2016-2019, the maximum percent of flood coverage in Sylhet increased by 9%
# There are noticable differences for Jan/Feb and May/Junebetween 2016 and 2019
# - January & Feb saw a respective 13% and 9% decrease in flooding on average.
# - May & June saw a respective 12% and 9% decrease in flooding on average.

# By month, data refelcted known dry seasons (Feb-April) and monsoon nseasons (June-Sept)
# - Spike in Jan2016 indicated 29% coverage compared to mean of 0.17
# - Spike in April2017 indicated 14% coverage compared to 0.05 average
# - 2018 stays true to trend
# - Extended dry period of ~0.08% between Feb2019 and June2019 (normally ~4%)



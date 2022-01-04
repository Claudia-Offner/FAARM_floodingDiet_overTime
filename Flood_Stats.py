import pandas as pd
import os
from Collators import Panelist, Flooder, Statistics, Visualiser
import matplotlib.pyplot as plt
import numpy as np
import warnings
import plotly.express as px
warnings.simplefilter(action='ignore', category=FutureWarning)

def get_flood(file_names):

    flood_df = pd.DataFrame(columns=['c_code', 'panel', 'dov', 'c_shape_area', 'c_flood_diff', 'c_max', 'c_mean', 'c_min',
                                     'c_sd', 'r_flood_diff', 'r_max', 'r_min', 'r_mean', 'r_sd'])

    for i in file_names:
        result = Flooder(i).json_to_df()
        flood_df = flood_df.append(result, ignore_index=True)

    flood_df['panel'] = flood_df['panel'].str.replace('P9', 'end')

    return flood_df

#%%
# ====================================================================================
# GET GEE FLOODING DATA
# ====================================================================================
# Load satellite image data from JSON file to dataframe, clean images and export to csv
os.chdir('C:/Users/offne/Documents/FAARM/Data/GEE/Flooding 2/')

surv = ['P1.geojson', 'P2.geojson', 'P3.geojson', 'P4.geojson', 'P5.geojson',
        'P6.geojson', 'P7.geojson', 'P8.geojson', 'P9.geojson', 'end.geojson']

flood_df = get_flood(surv)
flood_df['dov'] = pd.to_datetime(flood_df['dov'], errors='coerce')
flood_df = Panelist(flood_df).get_dd_mm_yyyy()

# Transformations
flood_df['c_flood_diff'] = flood_df['c_flood_diff']+1
flood_df['r_flood_diff'] = flood_df['r_flood_diff']+1
flood_df['c_flood_trans'] = np.log(flood_df['c_flood_diff'])
flood_df['r_flood_trans'] = np.log(flood_df['r_flood_diff'])

# Save data
os.chdir('C:/Users/offne/Documents/FAARM/')
flood_df.to_csv('Data/flood_df.csv', index=False)


#%%
# ====================================================================================
# STATS: Flood differences at regional and cluster level
# ====================================================================================

# Check descriptive stats (whole dataset)
stats = Statistics(flood_df).general_stats()
print('Regional Flooding \n', stats['r_flood_diff'])
print('Cluster Flooding \n', stats['c_flood_diff'])

# Check Years
ystat_r = Statistics(flood_df).stats_by_time('r_flood_diff', 'year', range(2015, 2021, 1))
print('Region Flooding by Year\n', ystat_r)
ystat_c = Statistics(flood_df).stats_by_time('c_flood_diff', 'year', range(2015, 2021, 1))
print('Cluster Flooding by Year\n', ystat_c)

# Check Months
mstat_r = Statistics(flood_df).stats_by_time('r_flood_diff', 'month', range(1, 13, 1))
print('Region Flooding by Month\n', mstat_r)
mstat_c = Statistics(flood_df).stats_by_time('c_flood_diff', 'month', range(1, 13, 1))
print('Cluster Flooding by Month\n', mstat_c)

# Expect flooding to be worse in cluster 44 & 43
x = flood_df.loc[flood_df['c_flood_diff'] > 10]

# ====================================================================================
# VISUALS
# ====================================================================================
# Compare histograms between regions
flood_df['c_flood_diff'].hist()
plt.show(block=True)
flood_df['r_flood_diff'].hist()
plt.show(block=True)

# Compare fluctuations over MONTHS between clusters and region
Visualiser(mstat_r, mstat_c).plotter('month', 'max', range(0, 12), [0, 11], 'Maximum by Region & Cluster \n (over months)')
Visualiser(mstat_r, mstat_c).plotter('month', 'sd', range(0, 12), [0, 11], 'StnDev by Region & Cluster \n (over months)')
Visualiser(mstat_r, mstat_c).plotter('month', 'mean', range(0, 12), [0, 11], 'Mean by Region & Cluster \n (over months)')

# Compare fluctuations over YEARS between clusters and region
Visualiser(ystat_r, ystat_c).plotter('year', 'max', range(0, 6), [0, 4], 'Maximum by Region & Cluster \n (over years)')
Visualiser(ystat_r, ystat_c).plotter('year', 'sd', range(0, 6), [0, 4], 'StnDev by Region & Cluster \n (over years)')
Visualiser(ystat_r, ystat_c).plotter('year', 'mean', range(0, 6), [0, 4], 'Mean by Region & Cluster \n (over years)')

# SATELLITE IMAGES (by year and month)
ystat_r['num_img'].sum()  # 190 images
mstat_r['num_img'].sum()  # 190 images
x = flood_df.drop_duplicates(subset='dov')  # 190 images
ym_check = x.groupby(['year', 'month'], as_index=False).count()
# Weird counting for cluster stat columns - Check december (12) 2015 & June (6) 2017 & May (5) 2019
ym_check = ym_check[['year', 'month', 'panel']]  # panel reps the number of satellite images collected for time frame


#%%
# Examine correlations - corplots
corrmat = flood_df.corr()
fig = px.imshow(corrmat, zmin=-1, zmax=1, color_continuous_scale= 'rdbu')
fig.update_layout(title = {'text': '<b>Covariate Correlation Matrix</b>',
                   'font': {'size':20, 'family':'Arial', 'color':'black'},
                   'xanchor':'center',
                   'x': 0.5,
                   'y': 0.95 },
                  font_family= 'Arial',
                  font_color='black',
                  margin = dict(t=70,r=70,b=70,l=70),
                  showlegend = True,
                  width = 700, height = 600,
                  autosize = False )
fig.show(renderer='browser')


#%%
# Initial Observations:
# - Seasonal changes (monthly) are clearly reflected at both REGIONAL and CLUSTER level.
# - Reasonable annual changes captured at a REGIONAL level, but trends are not reflected at CLUSTER level.
# =  This method may not be capturing flood severity/extents at the desired spatial resolution.


#%%
# ====================================================================================
# # Select non-seasonal flooding
# idx = flood_df.columns.get_loc('dov')
# flood_df.insert(loc=idx+1, column='month', value=pd.to_datetime(flood_df['dov']).dt.month)
# flood_df['month'] = flood_df['month'].apply(pd.to_numeric, downcast='integer', errors='coerce')
# flood_df = flood_df[flood_df.month.isin([11, 12, 1, 2, 3, 4, 5, 6])]
# flood_df = flood_df.reset_index().drop(['index', 'month'], axis=1)

# Address any NA values in WCODE, CLUSTER_CO, PANEL, DOV
# FLOOD.isnull().sum()  # no NAs

# # Aggregate Flooding Data by c_code and panel
# FLOOD = pd.DataFrame(columns=['c_code', 'panel', 'Shape_Area', 'c_flood_sum', 'c_flood_mean', 'r_flood_sum', 'r_flood_mean', 'r_max', 'r_min'])
# sum = flood_df.groupby(['c_code', 'panel']).sum().reset_index()
# mean = flood_df.groupby(['c_code', 'panel']).mean().reset_index()
# min = flood_df.groupby(['c_code', 'panel']).min().reset_index()
# max = flood_df.groupby(['c_code', 'panel']).max().reset_index()
# FLOOD['c_flood_sum'] = sum['Cluster_FloodDiff']
# FLOOD['r_flood_sum'] = sum['Region_FloodDiff']
# FLOOD['c_code'] = sum['c_code']
# FLOOD['panel'] = sum['panel']
# FLOOD['c_flood_mean'] = mean['Cluster_FloodDiff']
# FLOOD['r_flood_mean'] = mean['Region_FloodDiff']
# FLOOD['Shape_Area'] = mean['Shape_Area']
# FLOOD['r_min'] = min['Minimum']
# FLOOD['r_min'] = min['Minimum']
# FLOOD['r_max'] = max['Maximum']

# os.chdir('C:/Users/offne/Documents/FAARM/')



#%%
# ====================================================================================
# STATS: Compare to Temperature & other environmental variables
# ====================================================================================
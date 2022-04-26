import geopandas as gpd
import pandas as pd
import os

os.chdir('C:/Users/offne/Documents/GitHub/FAARM_Analysis/Data/Flooding Events/')

# Load 1000 buffer
shape1000 = gpd.read_file('flood_zonalstats_1000m.shp')
shape1000.columns = ['c_code', 'area_km2', 'cluster_pixel_count', 'Flood_20160719_sum', 'Flood_20160719_mean',
                     'Flood_20170328_sum', 'Flood_20170328_mean', 'Flood_20180810_sum', 'Flood_20180810_mean',
                     'Flood_20190620_sum', 'Flood_20190620_mean', 'geometry']
# Load 500 buffer
shape500 = gpd.read_file('flood_zonalstats_500m.shp')
shape500.columns = ['c_code', 'area_km2', 'cluster_pixel_count', 'Flood_20160719_sum', 'Flood_20160719_mean',
                     'Flood_20170328_sum', 'Flood_20170328_mean', 'Flood_20180810_sum', 'Flood_20180810_mean',
                     'Flood_20190620_sum', 'Flood_20190620_mean', 'geometry']

# Average over time
shape500['floodMean_overtime'] = shape500[['Flood_20160719_mean', 'Flood_20170328_mean', 'Flood_20180810_mean',
                                           'Flood_20190620_mean']].mean(axis=1)
shape1000['floodMean_overtime'] = shape1000[['Flood_20160719_mean', 'Flood_20170328_mean', 'Flood_20180810_mean',
                                           'Flood_20190620_mean']].mean(axis=1)

# Merge with FAARM data (get wcodes)
faarm = pd.read_csv('FSN-MH_compiled_211027.csv', low_memory=False)
faarm = faarm[['wcode', 'c_code']]
res1000 = pd.merge(faarm, shape1000, how='left', on=['c_code'])
res500 = pd.merge(faarm, shape500, how='left', on=['c_code'])

res1000.to_csv('FSN-MH-flood1000_compiled_220210.csv', index=False)
res500.to_csv('FSN-MH-flood500_compiled_220210.csv', index=False)



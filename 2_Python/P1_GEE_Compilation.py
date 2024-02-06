# DESCRIPTION

# IMPORT
import os

from Collators import Extractor, Panelist, SpatialProcessor
from os import listdir
import pandas as pd
import inspect
import warnings

warnings.simplefilter(action='ignore', category=FutureWarning)

# IMPORTANT - set file path to data folder location
data_path = 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/' \
            '- DD-Flooding Interaction - CO/4. Data'

# FUNCTIONS

def retrieve_name(var):
    """
    This function turns a variable into a string name
    """
    callers_local_vars = inspect.currentframe().f_back.f_locals.items()
    return [var_name for var_name, var_val in callers_local_vars if var_val is var]


# ========================================================================================
# GET BOUNDING BOX (for GEE)
# ========================================================================================
# # Create Bounding Box from FAARM 1km Buffer file (for G1_compReference.js)
# os.chdir(data_path)
#
# # Reproject to a projected coordinate system in meters (32646), for precise 1km buffer
# x = SpatialProcessor('FAARM/enum_cCode_buffer1000.shp').create_bbox(crs=32646, buffer=1000, plot=False)
# y = SpatialProcessor(x).transform_df(crs=32646)  # Transform back to WGS_1984_UTM_Zone_46N
# y.to_file('FAARM/FAARM1km_bbox_CO.shp')  # Export


# ========================================================================================
# GET REFERENCE STATS (for GEE)
# ========================================================================================
# # Convert Composite Reference Statistics to CSV for selection (from G1_compReference.js)
# os.chdir(data_path + '/GEE_Flooding_Clusters')
#
# main_prop = ['Date', 'Maximum', 'Minimum', 'Mean', 'Stdev']
# y = Extractor('ReferenceStats.geojson').json_to_df(main_props=main_prop, out_param='id')
# y.reset_index(inplace=True)
# y.to_csv('ReferenceStats.csv', index=False)


# ====================================================================================
# GEE FLOODING EXPOSURE (for FAARM)
# ====================================================================================
# Load satellite image data from JSON file to dataframe, clean images and export to csv
os.chdir(data_path + '/GEE_Flooding_Clusters/10m_1.68threshold_8months/New folder/') #20240131_Fix

file_names = os.listdir()

#  Create df with properties
flood_df = pd.DataFrame(columns=['c_code', 'panel', 'dov', 'c_Areakm2', 'c_floodedAreakm2', 'r_floodedAreakm2',
                                 'r_area', 'r_max', 'r_min', 'r_mean', 'r_sd'])

props = ['date', 'panel', 'r_floodedAreakm2', 'r_area', 'r_max', 'r_min', 'r_mean', 'r_sd']
nprops = ['enum_c_cod', 'c_Areakm2', 'c_floodedAreakm2']
nfeat = 'clusters'

# Append each geojson to empty df
for f in file_names:
    result = Extractor(f).nested_json_to_df(main_props=props, nested_feature=nfeat, nested_props=nprops)
    flood_df = flood_df.append(result, ignore_index=True)

# Organise dataframe
# flood_df['panel'] = flood_df['panel'].str.replace('P9', 'end')
flood_df['dov'] = pd.to_datetime(flood_df['dov'], errors='coerce')
flood_df = Panelist(flood_df).get_dd_mm_yyyy()

# Remove values based on satellite coverage (r_area)
# NB: areas <100 only cover clusters 1 fully (2 & 3 only partially)
# NB: areas >990 cover every cluster
flood_df = flood_df[(flood_df['r_area'] > 900)]

# Calculate Flood Percentage by Cluster
flood_df['perc_flooded_c'] = flood_df['c_floodedAreakm2'] / flood_df['c_area']
flood_df['perc_flooded_r'] = flood_df['r_floodedAreakm2'] / flood_df['r_area']

# Get dates of satellite images extracted (pre-aggregation)
os.chdir(data_path + '/GEE_Flooding_Clusters/')
flood_dates = flood_df[['panel', 'dov', 'day', 'month', 'year', 'r_floodedAreakm2', 'r_area', 'perc_flooded_r']]
flood_dates = flood_dates.drop_duplicates().reset_index().drop('index', axis=1).sort_values(by=['dov'])
# Save data
flood_dates.to_csv('gee_flood_dates.csv', index=False)

# Flood mean grouped by wcode, year, month
flood_df['month2'] = flood_df['month'].apply(lambda m: '{0:0>2}'.format(m))
flood_df['year_month'] = flood_df['year'].astype(str) + '-' + flood_df['month2'].astype(str)
flood_df = flood_df.groupby(['c_code', 'year_month'], as_index=False).mean()  # panel
flood_df = flood_df.sort_values(by=['c_code', 'year_month']).reset_index().drop('index', axis=1)  # important for lag
flood_df.drop('day', axis=1, inplace=True)
# Save data
flood_df.to_csv('gee_flood_df.csv', index=False)

#%%
# ====================================================================================
# GEE ENVIRONMENT CONTROLS (for FAARM)
# ====================================================================================
# os.chdir(data_path + 'GEE_Environment_Clusters/')
#
# # Get data
# # NOTE: Run each data frame line by line (struggles to process at once)
# props = ['date']
# nprops = ['enum_c_cod', 'dov', 'mean', 'min', 'max']
# nfeat = 'clusters'
#
# elev = Extractor('elev_res_30m.geojson').json_to_df(main_props=['enum_c_cod', 'elev'])
# temp = Extractor('temp_res_250m.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
#                                                                                 nested_props=nprops)
# evap = Extractor('evap_res_250m.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
#                                                                                 nested_props=nprops)
# ndvi = Extractor('ndvi_res_250m.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
#                                                                                 nested_props=nprops)
# prec = Extractor('prec_res_250m.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
#                                                                                 nested_props=nprops)
#
# # Save to csv
# elev.to_csv('gee_elev_df.csv', index=False)
# temp.to_csv('gee_temp_df.csv', index=False)
# evap.to_csv('gee_evap_df.csv', index=False)
# ndvi.to_csv('gee_ndvi_df.csv', index=False)
# prec.to_csv('gee_prec_df.csv', index=False)


#
# ====================================================================================
# DATA CLEANING & FORMATTING (for FAARM)
# ====================================================================================
os.chdir(data_path)

elev = pd.read_csv('GEE_Environment_Clusters/gee_elev_df.csv', low_memory=False)
temp = pd.read_csv('GEE_Environment_Clusters/gee_temp_df.csv', low_memory=False)
evap = pd.read_csv('GEE_Environment_Clusters/gee_evap_df.csv', low_memory=False)
ndvi = pd.read_csv('GEE_Environment_Clusters/gee_ndvi_df.csv', low_memory=False)
prec = pd.read_csv('GEE_Environment_Clusters/gee_prec_df.csv', low_memory=False)
flood_df = pd.read_csv('GEE_Flooding_Clusters/gee_flood_df.csv', low_memory=False)

# Convert temperature data from kelvin (scale 0.02) to celcius
for i in ['mean', 'min', 'max']:
    temp[i] = temp[i] * 0.02 - 273.15

# Re-format all datasets
df_list = [temp, evap, ndvi, prec]
res = pd.DataFrame()
for i in df_list:
    # Get year/month distinction
    i['dov'] = pd.to_datetime(i['dov'])
    i = Panelist(i).get_dd_mm_yyyy()
    # Rename mean column
    mean_name = retrieve_name(i)[0] + '_mean'
    min_name = retrieve_name(i)[0] + '_min'
    max_name = retrieve_name(i)[0] + '_max'
    i.rename(columns={'mean': mean_name, 'min': min_name, 'max': max_name}, inplace=True)
    # Make single digits double (i.e. 01, 02, etc.)
    i['month'] = i['month'].astype(int).apply(lambda m: '{0:0>2}'.format(m))
    # year_month column
    i['year_month'] = i['year'].astype(str) + '-' + i['month'].astype(str)
    i.drop('day', axis=1, inplace=True)
    i['month'] = i['month'].astype(int)

# Aggregate to be monthly/seasonal estimates
temp = temp.groupby(['c_code', 'year_month', 'year', 'month'], as_index=False).mean()
evap = evap.groupby(['c_code', 'year_month', 'year', 'month'], as_index=False).mean()
ndvi = ndvi.groupby(['c_code', 'year_month', 'year', 'month'], as_index=False).mean()
prec = prec.groupby(['c_code', 'year_month', 'year', 'month'], as_index=False).mean()

# Merge datasets
res0 = pd.merge(temp, evap, how='left', on=['c_code', 'year_month', 'year', 'month'])
res1 = pd.merge(res0, ndvi, how='left', on=['c_code', 'year_month', 'year', 'month'])
res2 = pd.merge(res1, prec, how='left', on=['c_code', 'year_month', 'year', 'month'])
res3 = pd.merge(res2, elev, how='left', on=['c_code'])
res4 = pd.merge(res3, flood_df, how='left', on=['c_code', 'year_month', 'year', 'month'])

# Check missing values: 268 temperature & 297 evapotranspiration
res4.isna().sum()

# Merge with flood data and export
res4.to_csv('1_GEE_df.csv', index=False)

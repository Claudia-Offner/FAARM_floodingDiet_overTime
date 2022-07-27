# DESCRIPTION

# IMPORT
from Collators import Extractor, Panelist, SpatialProcessor
import os
import pandas as pd
import inspect
import warnings


warnings.simplefilter(action='ignore', category=FutureWarning)

# IMPORTANT - set file path to data folder location
data_path = 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/' \
            '- DD-Flooding TimeSeries - CO/4. Data/Final'


# FUNCTIONS


def retrieve_name(var):
    """
    This function turns a variable into a string name
    """
    callers_local_vars = inspect.currentframe().f_back.f_locals.items()
    return [var_name for var_name, var_val in callers_local_vars if var_val is var]


#%%
# ========================================================================================
# GET BOUNDING BOX (for GEE)
# ========================================================================================
# Create Bounding Box from FAARM 1km Buffer file (for G1_compReference.js)
os.chdir(data_path)

# Reproject to a projected coordinate system in meters (32646), for precise 1km buffer
x = SpatialProcessor('FAARM/enum_cCode_buffer1000.shp').create_bbox(crs=32646, buffer=1000, plot=False)
y = SpatialProcessor(x).transform_df(crs=4326)  # Transform back to WSG84
y.to_file('FAARM/FAARM1km_bbox_CO.shp')  # Export

#%%
# ========================================================================================
# GET REFERENCE STATS (for GEE)
# ========================================================================================
# Convert Composite Reference Statistics to CSV for selection (from G1_compReference.js)

main_prop = ['Date', 'Maximum', 'Minimum', 'Mean', 'Stdev']
y = Extractor('ReferenceSelection.geojson').json_to_df(main_props=main_prop)
y.to_csv('ReferenceSelection.csv', index=False)

#%%
# ====================================================================================
# GEE FLOODING EXPOSURE (for FAARM)
# ====================================================================================
# Load satellite image data from JSON file to dataframe, clean images and export to csv
os.chdir(data_path + '/GEE_Flooding_Clusters')

file_names = ['P1.geojson', 'P2.geojson', 'P3.geojson', 'P4.geojson', 'P5.geojson',
              'P6.geojson', 'P7.geojson', 'P8.geojson', 'P9.geojson', 'end.geojson']

flood_df = pd.DataFrame(columns=['c_code', 'panel', 'dov', 'c_Areakm2', 'c_floodedAreakm2', 'r_floodedAreakm2', 'r_max',
                                 'r_min', 'r_mean', 'r_sd'])

props = ['date', 'panel', 'r_floodedAreakm2', 'r_max', 'r_min', 'r_mean', 'r_sd']
nprops = ['enum_c_cod', 'c_Areakm2', 'c_floodedAreakm2']
nfeat = 'clusters'

for f in file_names:
    result = Extractor(f).nested_json_to_df(main_props=props, nested_feature=nfeat, nested_props=nprops)
    flood_df = flood_df.append(result, ignore_index=True)

flood_df['panel'] = flood_df['panel'].str.replace('P9', 'end')

flood_df['dov'] = pd.to_datetime(flood_df['dov'], errors='coerce')
flood_df = Panelist(flood_df).get_dd_mm_yyyy()

# Calculate Flood Percentage by Cluster
flood_df['perc_flooded'] = flood_df['c_floodedAreakm2'] / flood_df['c_Areakm2']

# Flood mean grouped by wcode, year, month
flood_df['month2'] = flood_df['month'].apply(lambda m: '{0:0>2}'.format(m))
flood_df['year_month'] = flood_df['year'].astype(str) + '-' + flood_df['month2'].astype(str)
flood_df = flood_df.groupby(['c_code', 'year_month'], as_index=False).mean()  # panel
flood_df = flood_df.sort_values(by=['c_code', 'year_month']).reset_index().drop('index', axis=1)  # important for lag
flood_df.drop('day', axis=1, inplace=True)

# Save data
os.chdir(data_path)
flood_df.to_csv('gee_flood_df.csv', index=False)

#%%
# ====================================================================================
# GEE ENVIRONMENT CONTROLS (for FAARM)
# ====================================================================================
os.chdir(data_path)

# Get data
# NOTE: Run each data frame line by line (struggles to process at once)
props = ['date']
nprops = ['enum_c_cod', 'dov', 'mean', 'min', 'max']
nfeat = 'clusters'

elev = Extractor('GEE_Environment_Clusters/elev_res.geojson').json_to_df(main_props=['enum_c_cod', 'elev'])
temp = Extractor('GEE_Environment_Clusters/temp_res.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
                                                                                nested_props=nprops)
evap = Extractor('GEE_Environment_Clusters/evap_res.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
                                                                                nested_props=nprops)
ndvi = Extractor('GEE_Environment_Clusters/ndvi_res.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
                                                                                nested_props=nprops)
prec = Extractor('GEE_Environment_Clusters/prec_res.geojson').nested_json_to_df(main_props=props, nested_feature=nfeat,
                                                                                nested_props=nprops)
# Save to csv
elev.to_csv('gee_elev_df.csv', index=False)
temp.to_csv('gee_temp_df.csv', index=False)
evap.to_csv('gee_evap_df.csv', index=False)
ndvi.to_csv('gee_ndvi_df.csv', index=False)
prec.to_csv('gee_prec_df.csv', index=False)


# ====================================================================================
# DATA CLEANING & FORMATTING (for FAARM)
# ====================================================================================
os.chdir(data_path)

elev = pd.read_csv('gee_elev_df.csv', low_memory=False)
temp = pd.read_csv('gee_temp_df.csv', low_memory=False)
evap = pd.read_csv('gee_evap_df.csv', low_memory=False)
ndvi = pd.read_csv('gee_ndvi_df.csv', low_memory=False)
prec = pd.read_csv('gee_prec_df.csv', low_memory=False)
flood_df = pd.read_csv('gee_flood_df.csv', low_memory=False)

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

# Handle missing values: 268 temperature & 297 evapotranspiration
res4.isna().sum()

# Merge with flood data and export
res4.to_csv('1_GEE_df.csv', index=False)

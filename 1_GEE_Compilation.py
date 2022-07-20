# DESCRIPTION

# IMPORT
from Collators import Flooder, Panelist
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


def nested_json_to_df(json):
    """
    This function iterates over an image collection and extracts cluster specific data for each image
    (i.e. 2 dimensions). Pay attention to the column names, they should be specific to data at hand.
    """
    r1 = json
    # create output data frame
    df = pd.DataFrame(columns=['enum_c_cod', 'dov', 'INSIDE_X', 'INSIDE_Y', 'area_km2', 'mean', 'min', 'max'])
    images = len(r1['features'])
    for img in range(images):  # for every image in geojson
        image = r1['features'][img]['properties']['clusters']
        clusters = len(image)
        for c in range(clusters):  # for every cluster in Clusters
            # Get cluster stats
            prop = image[c]['properties']
            # Get regional stats
            prop['dov'] = r1['features'][img]['properties']['date']
            df = df.append(prop, ignore_index=True)

    df.rename(columns={'enum_c_cod': 'c_code'}, inplace=True)
    df = df.drop(['INSIDE_X', 'INSIDE_Y', 'area_km2'], axis=1)

    return df


def json_to_df(json):
    """
    This function extracts cluster specific data for 1 image (i.e. 1 dimension). Pay attention to the column names,
    they should be specific to data at hand.
    """
    r1 = json
    # create output data frame
    df = pd.DataFrame(columns=['enum_c_cod', 'INSIDE_X', 'INSIDE_Y', 'area_km2', 'elev'])
    clusters = len(r1['features'])
    for clus in range(clusters):  # for every image in geojson
        prop = r1['features'][clus]['properties']
        df = df.append(prop, ignore_index=True)

    df.rename(columns={'enum_c_cod': 'c_code'}, inplace=True)
    df = df.drop(['INSIDE_X', 'INSIDE_Y', 'area_km2'], axis=1)

    return df


def get_flood(file_names):
    df = pd.DataFrame(columns=['c_code', 'panel', 'dov', 'c_Areakm2', 'c_floodedAreakm2', 'r_floodedAreakm2', 'r_max',
                               'r_min', 'r_mean', 'r_sd'])

    for f in file_names:
        result = Flooder(f).json_to_df()
        df = df.append(result, ignore_index=True)

    df['panel'] = df['panel'].str.replace('P9', 'end')

    return df


# ====================================================================================
# GEE FLOODING EXPOSURE
# ====================================================================================
# Load satellite image data from JSON file to dataframe, clean images and export to csv
# os.chdir(data_path + '/GEE_Flooding_Clusters')
#
# surv = ['P1.geojson', 'P2.geojson', 'P3.geojson', 'P4.geojson', 'P5.geojson',
#         'P6.geojson', 'P7.geojson', 'P8.geojson', 'P9.geojson', 'end.geojson']
#
# flood_df = get_flood(surv)
# flood_df['dov'] = pd.to_datetime(flood_df['dov'], errors='coerce')
# flood_df = Panelist(flood_df).get_dd_mm_yyyy()
#
# # Calculate Flood Percentage by Cluster
# flood_df['perc_flooded'] = flood_df['c_floodedAreakm2'] / flood_df['c_Areakm2']
#
# # Flood mean grouped by wcode, year, month
# flood_df['month2'] = flood_df['month'].apply(lambda x: '{0:0>2}'.format(x))
# flood_df['year_month'] = flood_df['year'].astype(str) + '-' + flood_df['month2'].astype(str)
# flood_df = flood_df.groupby(['c_code', 'year_month'], as_index=False).mean()  # panel
# flood_df = flood_df.sort_values(by=['c_code', 'year_month']).reset_index().drop('index', axis=1)  # important for lag
# flood_df.drop('day', axis=1, inplace=True)
#
# # Save data
# os.chdir(data_path)
# flood_df.to_csv('gee_flood_df.csv', index=False)


# ====================================================================================
# GEE ENVIRONMENT CONTROLS
# ====================================================================================
# os.chdir(data_path)

# Get data
# NOTE: Run each data frame line by line (struggles to process at once)
# elev = json_to_df(Flooder('GEE_Environment_Clusters/elev_res.geojson').get_json())
# temp = nested_json_to_df(Flooder('GEE_Environment_Clusters/temp_res.geojson').get_json())
# evap = nested_json_to_df(Flooder('GEE_Environment_Clusters/evap_res.geojson').get_json())
# ndvi = nested_json_to_df(Flooder('GEE_Environment_Clusters/ndvi_res.geojson').get_json())
# prec = nested_json_to_df(Flooder('GEE_Environment_Clusters/prec_res.geojson').get_json())
# Save to csv
# elev.to_csv('gee_elev_df.csv', index=False)
# temp.to_csv('gee_temp_df.csv', index=False)
# evap.to_csv('gee_evap_df.csv', index=False)
# ndvi.to_csv('gee_ndvi_df.csv', index=False)
# prec.to_csv('gee_prec_df.csv', index=False)

# %%
# ====================================================================================
# DATA CLEANING & FORMATTING
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
    # Make single digits double
    i['month'] = i['month'].astype(int).apply(lambda x: '{0:0>2}'.format(x))
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

from Collators import Flooder, Panelist
import os
import pandas as pd
import inspect
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

def retrieve_name(var):
    '''
    This function turns a variable into a string name
    '''
    callers_local_vars = inspect.currentframe().f_back.f_locals.items()
    return [var_name for var_name, var_val in callers_local_vars if var_val is var]

def json_to_df_2D(json):
    '''
    This function iterates over an image collection and extracts cluster specific data for each image
    (i.e. 2 dimensions). Pay attention to the column names, they should be specific to data at hand.
    '''
    r1 = json
    # create output data frame
    df = pd.DataFrame(columns=['enum_c_cod', 'dov', 'INSIDE_X', 'INSIDE_Y', 'area_km2', 'mean', 'min', 'max'])
    images = len(r1['features'])
    for i in range(images):  # for every image in geojson
        image = r1['features'][i]['properties']['clusters']
        clusters = len(image)
        for c in range(clusters):  # for every cluster in Clusters
            # Get cluster stats
            prop = image[c]['properties']
            # Get regional stats
            prop['dov'] = r1['features'][i]['properties']['date']
            df = df.append(prop, ignore_index=True)

    df.rename(columns={'enum_c_cod': 'c_code'}, inplace=True)
    df = df.drop(['INSIDE_X', 'INSIDE_Y', 'area_km2'], axis=1)

    return df

def json_to_df_1D(json):
    '''
    This function extracts cluster specific data for 1 image (i.e. 1 dimension). Pay attention to the column names,
    they should be specific to data at hand.
    '''
    r1 = json
    # create output data frame
    df = pd.DataFrame(columns=['enum_c_cod', 'INSIDE_X', 'INSIDE_Y', 'area_km2', 'elev'])
    clusters = len(r1['features'])
    for i in range(clusters):  # for every image in geojson
        prop = r1['features'][i]['properties']
        df = df.append(prop, ignore_index=True)

    df.rename(columns={'enum_c_cod': 'c_code'}, inplace=True)
    df = df.drop(['INSIDE_X', 'INSIDE_Y', 'area_km2'], axis=1)

    return df

def get_flood(file_names):

    flood_df = pd.DataFrame(columns=['c_code', 'panel', 'dov', 'c_Areakm2', 'c_floodedAreakm2', 'r_floodedAreakm2', 'r_max',
                                     'r_min', 'r_mean', 'r_sd'])

    for i in file_names:
        result = Flooder(i).json_to_df()
        flood_df = flood_df.append(result, ignore_index=True)

    flood_df['panel'] = flood_df['panel'].str.replace('P9', 'end')

    return flood_df

# ====================================================================================
# GEE FLOODING EXPOSURE
# ====================================================================================
# Load satellite image data from JSON file to dataframe, clean images and export to csv
# os.chdir(r'G:\My Drive\GEE\Cluster_100mbuff_10mres_tuned')
#
# surv = ['P1.geojson', 'P2.geojson', 'P3.geojson', 'P4.geojson', 'P5.geojson',
#         'P6.geojson', 'P7.geojson', 'P8.geojson', 'P9.geojson', 'end.geojson']
#
# flood_df = get_flood(surv)
# flood_df['dov'] = pd.to_datetime(flood_df['dov'], errors='coerce')
# flood_df = Panelist(flood_df).get_dd_mm_yyyy()
#
# # Calculate Flood Percentage by Cluster
# flood_df['perc_flooded'] = flood_df['c_floodedAreakm2']/flood_df['c_Areakm2']
#
# # Flood mean grouped by wcode, year, month
# flood_df['month2'] = flood_df['month'].apply(lambda x: '{0:0>2}'.format(x))
# flood_df['year_month'] = flood_df['year'].astype(str) + '-' + flood_df['month2'].astype(str)
# flood_df = flood_df.groupby(['c_code', 'year_month'], as_index=False).mean()  #panel
# flood_df = flood_df.sort_values(by=['c_code', 'year_month']).reset_index().drop('index', axis=1)  # IMPORTANT FOR LAG sort values
# flood_df.drop('day', axis=1, inplace=True)
#
# # Save data
os.chdir(r'G:\My Drive\GEE')
# flood_df.to_csv('flood_df.csv', index=False)


# ====================================================================================
# GEE ENVIRONMENT CONTROLS
# ====================================================================================

# Get data
# elev = json_to_df_1D(Flooder('Environ/elev_res.geojson').get_json())
# temp = json_to_df_2D(Flooder('Environ/temp_res.geojson').get_json())
# evap = json_to_df_2D(Flooder('Environ/evap_res.geojson').get_json())
# ndvi = json_to_df_2D(Flooder('Environ/ndvi_res.geojson').get_json())
# prec = json_to_df_2D(Flooder('Environ/prec_res.geojson').get_json())
# Save to csv
# elev.to_csv('elev_df.csv', index=False)
# temp.to_csv('temp_df.csv', index=False)
# evap.to_csv('evap_df.csv', index=False)
# ndvi.to_csv('ndvi_df.csv', index=False)
# prec.to_csv('prec_df.csv', index=False)

elev = pd.read_csv('elev_df.csv', low_memory=False)
temp = pd.read_csv('temp_df.csv', low_memory=False)
evap = pd.read_csv('evap_df.csv', low_memory=False)
ndvi = pd.read_csv('ndvi_df.csv', low_memory=False)
prec = pd.read_csv('prec_df.csv', low_memory=False)
flood_df = pd.read_csv('flood_df.csv', low_memory=False)


# DATA CLEANING & FORMATTING

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
    i.rename(columns={'mean': mean_name, 'min':min_name, 'max':max_name}, inplace=True)
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
res = pd.merge(temp, evap, how='left', on=['c_code', 'year_month', 'year', 'month'])
res = pd.merge(res, ndvi, how='left', on=['c_code', 'year_month', 'year', 'month'])
res = pd.merge(res, prec, how='left', on=['c_code', 'year_month', 'year', 'month'])
res = pd.merge(res, elev, how='left', on=['c_code'])
res = pd.merge(res, flood_df, how='left', on=['c_code', 'year_month', 'year', 'month'])

# Handle missing values: 268 temperature & 297 evapotranspiration
res.isna().sum()

# Merge with flood data and export
res.to_csv('1_Flooding_environ_df.csv', index=False)


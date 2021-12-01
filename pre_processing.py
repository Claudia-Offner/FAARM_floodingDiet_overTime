import pandas as pd
from pre_processors import Organiser, Panelist, Flooder
import warnings
import os
warnings.simplefilter(action='ignore', category=FutureWarning)


def dem_end_to_base(baseline, endline, end_HH):
    """
    Format dataframe
    - INPUT: data frame
    - OUTPUT: data frame
    """
    # Make coherent with baseline variables
    # get 'wi_water1', 'wi_6' from baseline & drop 'wi_pond',
    endline['wi_water1'] = baseline['wi_water1']
    endline['wi_6'] = baseline['wi_6']
    # combine 'wi_cb', 'wi_gsp', 'wi_cdg', 'wi_rp' into 'wi_livestock'
    endline['wi_livestock'] = end_HH['wi_cb'] + end_HH['wi_gsp'] + end_HH['wi_cdg'] + end_HH['wi_rp']
    # add variables that will assume baseline values
    cols = ['religion', 'education', 'age', 'hhs_111', 'hhs_113', 'hhs_103', 'hhs_103b', 'hhs_101', 'hhs_102', 'hhs_104', 'hhs_104ot']
    for i in cols:
        endline[i] = ''
    endline = endline.loc[:, tuple(baseline.columns)]

    return endline

def get_flood(file_names):

    flood_df = pd.DataFrame(columns=['cluster_co', 'panel', 'dov', 'Shape_Area', 'Cluster_Diff', 'Region_Diff',
                                     'Maximum', 'Minimum', 'Mean', 'Stdev'])

    for i in file_names:
        result = Flooder(i).json_to_df()
        flood_df = flood_df.append(result, ignore_index=True)

    flood_df['panel'] = flood_df['panel'].str.replace('P9', 'end')
    flood_df.rename(columns={'cluster_co': 'c_code'}, inplace=True)

    return flood_df


# ====================================================================================
# LOAD DATA
# ====================================================================================
# columns have mixed dtypes that need to be addressed
bi = pd.read_csv('Data/Bimonthly Women.csv', low_memory=False)
baseW = pd.read_csv('Data/Baseline Women.csv', low_memory=False)
endW = pd.read_csv('Data/Endline Women.csv', low_memory=False)
baseHH = pd.read_csv('Data/Baseline HH.csv', low_memory=False)
endHH = pd.read_csv('Data/Endline HH.csv', low_memory=False)
epds_wdds = pd.read_csv('Data/Bi/Women_EPDS_WDDS.csv', low_memory=False)

# DATA CLEANING TOOLS
# l = set(list(wealth_end.columns))
# x = set(list(DEM_BASE.columns))
# print('Names in FC df but not in shape gdf', set(list(l - x))) #FC data set
# print('Names in shape gdf but not in FC df', set(list(x - l))) #shape dataset

# CHECK NAVALUES
# DEM.isnull().sum()
# x = hfias_bi_org[hfias_bi_org['dov'].isnull()]
# EPDS_WDDS.loc[EPDS_WDDS['wcode'] == 661]

# ====================================================================================
# GET EPDS & WDDS & PREGNANCY
# ====================================================================================

epds_wdds_sub = epds_wdds.loc[:, ('wcode', 'c_code', 'dov', 'new_women',
                              'md_1', 'md_2', 'md_3', 'md_4', 'md_5', 'md_6', 'md_7', 'md_8', 'md_9', 'md_10', 'md_score', 'md_scale', 'rd',
                              'dd10r_starch', 'dd10r_legume', 'dd10r_nuts', 'dd10r_dairy', 'dd10r_flesh', 'dd10r_eggs', 'dd10r_dglv', 'dd10r_vita', 'dd10r_othf', 'dd10r_othv', 'dd10r_score_m', 'dd10r_min_m', 'dd10r_score', 'dd10r_min',
                              'preg', 'enum_dcocode', 'actually_preg', 'months', 'monthsdt', 'monthsest', 'y_child', 'preg_week', 'pregmuac', 'pregmuac_cat', 'prior_preg', 'months_preg', 'ldov_check', 'still_preg', 'contpreg_mon', 'dayspreg', 'pull_dovi', 'pull_preg_stat', 'pull_months', 'epds', 'md_scale_d', 'dco_m', 'dov_m', 'preg_w', 'prior_preg_b')]

epds_wdds_org = Organiser(epds_wdds_sub[:]).format()  # wcodes already unnested
EPDS_WDDS = Panelist(epds_wdds_org[:]).get_panels()

# Address any NA values in WCODE, CLUSTER_CO, PANEL, DOV
# EPDS_WDDS.isnull().sum()  # 2 c_codes missing ; 93 panels missing (rounds after endline)
# x = EPDS_WDDS[EPDS_WDDS['c_code'].isnull()]  # wcode 1718, 3822 - assign to clusters 17 and 38
EPDS_WDDS.loc[EPDS_WDDS['wcode'].eq(1718) & EPDS_WDDS['c_code'].isnull(), 'c_code'] = 17
EPDS_WDDS.loc[EPDS_WDDS['wcode'].eq(3822) & EPDS_WDDS['c_code'].isnull(), 'c_code'] = 38
EPDS_WDDS.loc[:, 'c_code'] = EPDS_WDDS.loc[:, 'c_code'].astype('int64')  # Make c_code integer
EPDS_WDDS = EPDS_WDDS.dropna(subset=['panel'])  # drop instances from rounds post endline
EPDS_WDDS.rename(columns={'dov':'dov_epds_wdds'}, inplace = True)


# ====================================================================================
# GET HFIAS
# ====================================================================================

hfias_base = baseW[['wcode', 'c_code', 'today_w', 'treatment', 'hfias1', 'hfias2', 'hfias3', 'hfias4', 'hfias5', 'hfias6', 'hfias7', 'hfias8', 'hfias9', 'hfias_score', 'hfias', 'hfias_cat', 'hfias_d']]
hfias_bi = bi[['wcode', 'c_code', 'dov', 'treatment', 'hfias1', 'hfias2', 'hfias3', 'hfias4', 'hfias5', 'hfias6', 'hfias7', 'hfias8', 'hfias9', 'hfias_score', 'hfias', 'hfias_cat', 'hfias_d']]
hfias_end = endHH[['wcode1', 'wcode2', 'wcode3', 'c_code', 'today_h', 'treatment', 'hfias1', 'hfias2', 'hfias3', 'hfias4', 'hfias5', 'hfias6', 'hfias7', 'hfias8', 'hfias9', 'hfias_score', 'hfias', 'hfias_cat', 'hfias_d']]

hfias_base_org = Organiser(hfias_base[:]).format('2015-06-30')
hfias_bi_org = Organiser(hfias_bi[:]).format().dropna(axis=0)  # drop rows with na values
hfias_end_org = Organiser(hfias_end[:]).unnest_wcodes('2020-02-29')

# Merge datasets
hfias = hfias_base_org.append(hfias_bi_org).append(hfias_end_org)
hfias = Organiser(hfias[:]).format()
HFIAS = Panelist(hfias).get_panels()

# Address any NA values in WCODE, CLUSTER_CO, PANEL, DOV
# HFIAS.isnull().sum()  # 180 panels missing (rounds after endline)
HFIAS.loc[:, 'c_code'] = HFIAS.loc[:, 'c_code'].astype('int64')  # Make c_code integer
HFIAS = HFIAS.dropna(subset=['panel'])  # drop instances from rounds post endline
HFIAS.rename(columns={'dov':'dov_hfias'}, inplace = True)


# ====================================================================================
# GET DEMOGRAPHIC DATA
# ====================================================================================

# BASELINE:
# WEALTH (hh): Assets, remittances (HHS), Land holding (wi)
wealth_base = baseHH.loc[:, ('wcode', 'today_hh',
                             'hhs_111', 'hhs_113', 'hhs_103', 'hhs_103b', 'hhs_101', 'hhs_102', 'hhs_104', 'hhs_104ot',
                             'hhs_1', 'hhs_1ot', 'hhs_2', 'hhs_2ot', 'hhs_3', 'hhs_3ot', 'hhs_4', 'hhs_5', 'hhs_6', 'hhs_7', 'hhs_7ot', 'hhs_91', 'hhs_92', 'hhs_93', 'hhs_94', 'hhs_95', 'hhs_96', 'hhs_97', 'hhs_98', 'hhs_99', 'hhs_910', 'hhs_911', 'hhs_912', 'hhs_913', 'hhs_914', 'hhs_915', 'hhs_916', 'hhs_917', 'hhs_918', 'hhs_919', 'hhs_920', 'hhs_921', 'hhs_922',
                             'wi_floor', 'wi_wall1', 'wi_wall2', 'wi_roof', 'wi_hl', 'wi_al', 'wi_land', 'wi_elec', 'wi_single', 'wi_finhouse', 'wi_rooms', 'wi_fuel', 'wi_water1', 'wi_water2', 'wi_water3', 'wi_latrine1', 'wi_latrine2', 'wi_latrine4', 'wi_latrine5', 'wi_latrine6', 'wi_latrine7', 'wi_latrine8', 'wi_latrine9', 'wi_latrine10', 'wi_latrine11', 'wi_livestock', 'wi_2', 'wi_3', 'wi_4', 'wi_5', 'wi_6', 'wi_7', 'wi_8', 'wi_9', 'wi_10', 'wi_11', 'wi_12', 'wi_13', 'wi_14', 'wi_15', 'wi_17', 'wi_18', 'wi_19', 'wi_20', 'wi_22',
                             'wealth', 'dec', 'quint', 'terc', 'wealth2', 'dec2', 'quint2', 'terc2')]

wealth_base = Organiser(wealth_base[:]).format('2015-06-30')  # wcodes already unnested

# RELIGION & EDUCATION & AGE (w)
other_base = baseW.loc[:, ('wcode', 'c_code', 'today_w', 'g_2w', 'woman_edu_cat_', 'woman_q1_c')]
other_base.rename(columns={'g_2w':'religion', 'woman_edu_cat_':'education', 'woman_q1_c':'age'}, inplace = True)

other_base = Organiser(other_base[:]).format('2015-06-30')
dem_base = pd.merge(other_base, wealth_base, how='left', on=['wcode', 'dov'])

# ENDLINE
# WEALTH (hh): Assets, remittances (HHS), Land holding (wi)
wealth_end = endHH.loc[:, ('wcode1', 'wcode2', 'wcode3', 'c_code', 'today_h',
                           'hhs_1', 'hhs_1ot', 'hhs_2', 'hhs_2ot', 'hhs_3', 'hhs_3ot', 'hhs_4', 'hhs_5', 'hhs_6', 'hhs_7', 'hhs_7ot', 'hhs_91', 'hhs_92', 'hhs_93', 'hhs_94', 'hhs_95', 'hhs_96', 'hhs_97', 'hhs_98', 'hhs_99', 'hhs_910', 'hhs_911', 'hhs_912', 'hhs_913', 'hhs_914', 'hhs_915', 'hhs_916', 'hhs_917', 'hhs_918', 'hhs_919', 'hhs_920', 'hhs_921', 'hhs_922',
                           'wi_floor', 'wi_wall1', 'wi_wall2', 'wi_roof', 'wi_hl', 'wi_al', 'wi_land', 'wi_elec', 'wi_single', 'wi_finhouse', 'wi_rooms', 'wi_fuel', 'wi_water2', 'wi_water3', 'wi_latrine1', 'wi_latrine2', 'wi_latrine4', 'wi_latrine5', 'wi_latrine6', 'wi_latrine7', 'wi_latrine8', 'wi_latrine9', 'wi_latrine10', 'wi_latrine11', 'wi_2', 'wi_3', 'wi_4', 'wi_5', 'wi_7', 'wi_8', 'wi_9', 'wi_10', 'wi_11', 'wi_12', 'wi_13', 'wi_14', 'wi_15', 'wi_17', 'wi_18', 'wi_19', 'wi_20', 'wi_22',
                           'wealth', 'dec', 'quint', 'terc', 'wealth2', 'dec2', 'quint2', 'terc2')]

wealth_end = Organiser(wealth_end[:]).unnest_wcodes('2020-02-29')
dem_end = dem_end_to_base(dem_base, wealth_end, endHH)

# MERGE DATASETS
DEM = dem_base.append(dem_end)
DEM = Organiser(DEM[:]).format()
DEM = Panelist(DEM[:]).get_panels()  # there are 180 missing panels from measure taken after Endline

# Address any NA values in WCODE, CLUSTER_CO, PANEL, DOV
# DEM.isnull().sum()  # 180 panels missing (rounds after endline)
DEM.loc[:, 'c_code'] = DEM.loc[:, 'c_code'].astype('int64')  # Make c_code integer
DEM = DEM.dropna(subset=['panel'])  # drop instances from rounds post endline
DEM.rename(columns={'dov':'dov_dem'}, inplace = True)

# ====================================================================================
# GET GEE FLOODING DATA
# ====================================================================================

# Load satellite image data from JSON file to dataframe, clean images and export to csv
os.chdir('C:/Users/offne/Documents/FAARM/Data/GEE/Flooding/')

surv = ['P1.geojson', 'P2.geojson', 'P3.geojson', 'P4.geojson', 'P5.geojson',
        'P6.geojson', 'P7.geojson', 'P8.geojson', 'P9.geojson', 'end.geojson']

FLOOD = get_flood(surv)

os.chdir('C:/Users/offne/Documents/FAARM/')

# Address any NA values in WCODE, CLUSTER_CO, PANEL, DOV
# FLOOD.isnull().sum()  # no NAs
FLOOD.rename(columns={'dov':'dov_flood'}, inplace = True)

# Aggregate Flooding Data by c_code and panel
# - NOTE remember to take max of maximum and min of minimum!!!!
FLOOD = FLOOD.groupby(['c_code', 'panel']).mean().reset_index()

# ====================================================================================
# GET AGRICULTURE PRODUCTION DATA
# GET HOUSEHOLD SIZE OVER TIME
# ====================================================================================

#%%
# ====================================================================================
# MERGE ALL DATASETS
# ====================================================================================

# Create empty data frame with all wcodes, c_codes, panels
df1 = pd.DataFrame()
df = pd.DataFrame()
wcodes = EPDS_WDDS[['wcode', 'c_code']].drop_duplicates(subset=['wcode']).reset_index().drop(['index'], axis=1)
panels = ['base', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'end']
for i in range(len(wcodes)):
    df1 = df1.append(panels)
for i in range(len(panels)):
    df = df.append(wcodes)
df = df.sort_values(by=['wcode']).reset_index().drop(['index'], axis=1)  # sort values
df1 = df1.reset_index().drop(['index'], axis=1)  # sort values
df['panel'] = df1

# Merge data - NOTE some wcodes in EPDS_WDDS have multiple instances for one panel
x = pd.merge(df, FLOOD, how='left', on=['c_code', 'panel'])
x = pd.merge(x, EPDS_WDDS, how='left', on=['wcode', 'c_code', 'panel'])
x = pd.merge(x, HFIAS, how='left', on=['wcode', 'c_code', 'panel'])
result = pd.merge(x, DEM, how='left', on=['wcode', 'c_code', 'panel'])


#%%
# ====================================================================================
# Save to file
# ====================================================================================

result.to_csv('Data/subset_result.csv', index=False)


import pandas as pd
import os
from Collators import Panelist, Flooder, Organiser
from pandas.api.types import is_numeric_dtype
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

# def get_flood(file_names):
#
#     flood_df = pd.DataFrame(columns=['c_code', 'panel', 'dov', 'c_Areakm2', 'c_floodedAreakm2', 'r_floodedAreakm2', 'r_max',
#                                      'r_min', 'r_mean', 'r_sd'])
#
#     for i in file_names:
#         result = Flooder(i).json_to_df()
#         flood_df = flood_df.append(result, ignore_index=True)
#
#     flood_df['panel'] = flood_df['panel'].str.replace('P9', 'end')
#
#     return flood_df
#
# # ====================================================================================
# # GET GEE FLOODING DATA
# # ====================================================================================
# # Load satellite image data from JSON file to dataframe, clean images and export to csv
# os.chdir('C:/Users/offne/Documents/GitHub/FAARM_Analysis/Data/Cluster_100mbuff_10mres_tuned/')
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
# flood_df = flood_df.groupby(['c_code', 'year_month', 'panel'], as_index=False).mean()
# flood_df = flood_df.sort_values(by=['c_code', 'year_month']).reset_index().drop('index', axis=1)  # IMPORTANT FOR LAG sort values
#
# # Save data
# os.chdir('C:/Users/offne/Documents/GitHub/FAARM_Analysis/')
# flood_df.to_csv('Data/Cluster100_10mflood_df.csv', index=False)


# # Get means of Major Flood Event Months !!! ADAPT TO YEAR_MONTH VARIABLE INSTEAD !!!
# flood_201607 = flood_df.loc[flood_df['year'].eq(2016) & flood_df['month'].eq(7)].groupby('c_code').mean().reset_index()
# flood_201704 = flood_df.loc[flood_df['year'].eq(2017) & flood_df['month'].eq(4)].groupby('c_code').mean().reset_index()
# flood_201808 = flood_df.loc[flood_df['year'].eq(2018) & flood_df['month'].eq(7)].groupby('c_code').mean().reset_index()
# flood_events = pd.DataFrame(columns=['c_code', 'perc_flood_201607', 'perc_flood_201704', 'perc_flood_201808'])
# flood_events['perc_flood_201607'] = flood_201607['perc_flooded']
# flood_events['perc_flood_201704'] = flood_201704['perc_flooded']
# flood_events['perc_flood_201808'] = flood_201808['perc_flooded']
# flood_events['c_code'] = flood_201607['c_code']
#
# # Save data
# flood_events.to_csv('Data/Cluster100_10mflood_events_df.csv', index=False)

# Read Flood Data from CSV
flood_df = pd.read_csv('Data/Cluster100_10mflood_df.csv', low_memory=False)


# ====================================================================================
# GET WDDS DATA
# ====================================================================================

# Read diet data from csv
bi = pd.read_csv('Data/womens dd - long.csv', low_memory=False)
bi_sub = bi.loc[:, ('wcode', 'c_code', 'dov', 'treatment', 'dd10r_score', 'dd10r_min', 'season')]

bi_org = Organiser(bi_sub[:]).format()  # wcodes already unnested
bi_Diet = Panelist(bi_org[:]).get_panels()

# Address any NA values in WCODE, CLUSTER_CO, PANEL, DOV
# bi_Diet.isnull().sum()  # 2 c_codes missing ; 93 panels missing (rounds after endline)
# x = bi_Diet[bi_Diet['c_code'].isnull()]  # wcode 1718, 3822 - assign to clusters 17 and 38
bi_Diet.loc[bi_Diet['wcode'].eq(1718) & bi_Diet['c_code'].isnull(), 'c_code'] = 17
bi_Diet.loc[bi_Diet['wcode'].eq(3822) & bi_Diet['c_code'].isnull(), 'c_code'] = 38
bi_Diet.loc[bi_Diet['wcode'].eq(5416) & bi_Diet['c_code'].isnull(), 'c_code'] = 54
bi_Diet.loc[:, 'c_code'] = bi_Diet.loc[:, 'c_code'].astype('int64')  # Make c_code integer
bi_Diet = bi_Diet.dropna(subset=['panel'])  # drop instances from rounds post endline
# Create year-month columns
bi_Diet = Panelist(bi_Diet).get_dd_mm_yyyy()
bi_Diet['month'] = bi_Diet['month'].apply(lambda x: '{0:0>2}'.format(x))
bi_Diet['year_month'] = bi_Diet['year'].astype(str) + '-' + bi_Diet['month'].astype(str)
bi_Diet.rename(columns={'dov':'dov_bi_Diet'}, inplace = True)
bi_Diet['season'] = bi_Diet['season'].fillna(method='ffill', axis=0)


# ====================================================================================
# MERGE DATA
# ====================================================================================

# Get all year-month combinations for data
bi_Diet = bi_Diet.sort_values(by=['wcode', 'panel'])
bi_Diet.loc[bi_Diet['panel'] == 'base', 'year_month'] = '0-0'  # set baseline to 0-0 for diet
ym = list(set(list(bi_Diet['year_month']) + list(flood_df['year_month'])))
ym.sort()

# Create empty data frame with all wcodes, c_codes, panels
df1 = pd.DataFrame()
df = pd.DataFrame()
wcodes = bi_Diet[['wcode', 'c_code']].drop_duplicates(subset=['wcode']).reset_index().drop(['index'], axis=1)
for i in range(len(wcodes)):
    df1 = df1.append(ym)
for i in range(len(ym)):
    df = df.append(wcodes)

df = df.sort_values(by=['wcode']).reset_index().drop(['index'], axis=1)  # sort values
df1 = df1.reset_index().drop(['index'], axis=1)  # sort values
df['year_month'] = df1

# Merge data - NOTE some wcodes in DIET have multiple instances for one panel
x = pd.merge(df, flood_df, how='left', on=['c_code','year_month'])
RESULT = pd.merge(x, bi_Diet, how='left', on=['wcode', 'c_code', 'year_month'])

# Clean up columns
RESULT = RESULT.drop(['day_x', 'day_y', 'month_y', 'year_y', 'panel_y', 'dov_bi_Diet'], axis=1)
RESULT.rename(columns={'panel_x':'panel', 'year_x': 'year', 'month_x':'month'}, inplace = True)


# ====================================================================================
# DATA CLEANING
# ====================================================================================
# Fill na for year
RESULT['year'] = RESULT['year'].fillna(2015)
RESULT['month'] = RESULT['month'].fillna(9)

# Fill in missing Seasons (months): 1(Sept-Oct);2(Nov-Dec);3(Jan-Feb);4(Mar-Apr);5(May-June);6(Jul-Aug)
season_DD = {1:'Jan/Feb', 2:'Jan/Feb', 3:'Mar/Apr', 4: 'Mar/Apr', 5:'May/Jun', 6:'May/Jun',
                7:'Jul/Aug', 8:'Jul/Aug', 9:'Sept/Oct', 10:'Sept/Oct', 11:'Nov/Dec', 12:'Nov/Dec'}
# season_nums = {'Jan/Feb':3 , 'Mar/Apr':4, 'May/Jun':5, 'Jul/Aug':6, 'Sept/Oct':1, 'Nov/Dec':2}
# season_names = {3:'Jan/Feb', 4:'Mar/Apr',5:'May/Jun', 6:'Jul/Aug', 1:'Sept/Oct', 2:'Nov/Dec'}
season_nums = {'Jan/Feb':1 , 'Mar/Apr':2, 'May/Jun':3, 'Jul/Aug':4, 'Sept/Oct':5, 'Nov/Dec':6}
season_names = {1:'Jan/Feb', 2:'Mar/Apr',3:'May/Jun', 4:'Jul/Aug', 5:'Sept/Oct', 6:'Nov/Dec'}
RESULT['season_DD'] = RESULT['month']
RESULT['season_DD'] = RESULT['season_DD'].replace(season_DD)
RESULT['season'] = RESULT['season_DD']
RESULT['season'] = RESULT['season'].replace(season_nums)

# Dummy code binary DD
RESULT['dd10r_min'] = RESULT['dd10r_min'].map({'Inadequate diet': 0,'Diet diverse': 1})  # 1 = has inadquate diet

# Drop string columns
RESULT.drop(['year_month', 'panel'], axis=1)

# Aggregate dataset so mean is taken for numeric values and mode is taken for all else
RESULT = RESULT.groupby(['c_code', 'wcode', 'year', 'season'], as_index=False).mean()
RESULT['month'] = RESULT['month'] + 0.01
RESULT['month'] = round(RESULT['month'])

# 1 season time lag for flooding (score)
groups = RESULT.groupby('wcode')
RESULT['Flood_1Lag'] = ''
for admin, group in groups:
    # Flood
    group['timelag1'] = group['perc_flooded'].shift(1)
    mask = group['timelag1'].index
    RESULT.loc[mask, 'Flood_1Lag'] = group['timelag1']
    # Season
    group['timelag1'] = group['perc_flooded'].shift(1)
    mask = group['timelag1'].index
    RESULT.loc[mask, 'Flood_1Lag'] = group['timelag1']

RESULT['Flood_1Lag'] = pd.to_numeric(RESULT['Flood_1Lag']) # set to numeric

# Drop data from 2020
RESULT = RESULT[RESULT['year'] != 2020]

# Add season names again
RESULT['season_DD'] = RESULT['season']
RESULT['season_DD'] = RESULT['season_DD'].replace(season_names)

# Reorganise data
RESULT['month'] = RESULT['month'].astype(int).apply(lambda x: '{0:0>2}'.format(x))
RESULT['year_month'] = RESULT['year'].astype(int).astype(str) + '-' + RESULT['month'].astype(str)
RESULT['season'] = RESULT['season'].astype(int).apply(lambda x: '{0:0>2}'.format(x))
RESULT['year_season'] = RESULT['year'].astype(int).astype(str) + '-' + RESULT['season'].astype(int).astype(str)
RESULT = RESULT.sort_values(by=['wcode', 'year_month']).reset_index().drop(['index'], axis=1)  # sort values

# Check missing data
# RESULT.isnull().sum()
# RESULT['year_season'].value_counts()
# len(pd.unique(RESULT['year_season']))


# ====================================================================================
# GET BASELINE CHARACTERISTICS
# ====================================================================================

base = pd.read_csv('Data/FSN-MH data test 2_220314.csv', low_memory=False)
base_sub = base.loc[:, ('wcode', 'c_code', 'g_2h_BL', 'fam_type_BL', 'wi_hl_BL', 'wi_al_BL', 'wi_land_BL', 'num_crops_BL',
                        'woman_edu_cat__BL', 'hfias_BL', 'hfias_score_BL', 'hfias_cat_BL', 'hfias_d_BL',
                        'dd10r_score_m_BL', 'dd10r_min_m_BL', 'dd10r_score_BL',
                        'dd10r_min_BL', 'mobility_BL', 'support_BL', 'communication_BL', 'decision_BL', 'md_score_BL',
                        'md_scale_BL', 'age_3_BL', 'pb_621_BL', 'know_score_BL', 'w_monbirth_BL', 'wbmi_BL',
                        'wbmi_cat_BL', 'ced_BL', 'whb_BL', 'treatment', 'dd10r_score_EL', 'dd10r_min_EL',
                        'gravidity_EL', 'md_score_EL', 'md_scale_EL', 'treat', 'dep_ratio', 'dd10r_score_m_EL',
                        'dd10r_min_m_EL', 'dd10r_min_EL', 'dd10r_score_m_y1', 'dd10r_min_m_y1', 'dd10r_score_m_y2',
                        'dd10r_min_m_y2', 'dd10r_score_m_y3', 'dd10r_min_m_y3', 'dd10r_score_m_sur', 'dd10r_min_m_sur',
                        'dd10r_score_m_el', 'dd10r_min_m_el', 'avg_hfias_sandEL', 'num_crops_havg', 'num_crops_wavg',
                        'num_crops_ravg', 'lit_cat', 'anemic_all_BL', 'impgarden_BL', 'DD_score_avg_sandEL',
                        'num_crops_avg_cat', 'num_crops_avg_bin', 'avg_hfias_cat', 'avg_hfias_bin',
                        'avg_hfias_cat_sandEL', 'avg_hfias_bin_sandEL', 'avg_hfias_bin_rev', 'avg_hfias_bin_sandEL_rev',
                        'avg_hfias_cat_rev', 'avg_hfias_cat_sandEL_rev', 'avg_hfias_rev', 'avg_hfias_sandEL_rev',
                        'md_d13_EL_rev', 'num_crops_avg', 'avg_hfias', 'DD_min_sandEl', 'md_d13_BL', 'md_d13_EL',
                        'md_d13_BL_rev', 'md_d13_EL_rev', 'md_d12_BL', 'md_d12_EL', 'md_d12_BL_rev', 'md_d12_EL_rev',
                        'md_d11_BL', 'md_d11_EL', 'md_d11_BL_rev', 'md_d11_EL_rev', 'wealth_BL', 'dec_BL', 'quint_BL',
                        'terc_BL', 'wealth2_BL', 'dec2_BL', 'quint2_BL', 'terc2_BL')]
# Merge datasets
RESULT = pd.merge(RESULT, base_sub, how='left', on=['wcode', 'c_code', 'treatment'])

cols = ['treatment', 'g_2h_BL', 'fam_type_BL', 'wi_hl_BL', 'wi_al_BL', 'wi_land_BL',
        'num_crops_BL', 'woman_edu_cat__BL', 'hfias_BL', 'hfias_score_BL',
        'hfias_cat_BL', 'hfias_d_BL', 'dd10r_score_m_BL', 'dd10r_min_m_BL',
        'dd10r_score_BL', 'dd10r_min_BL', 'mobility_BL', 'support_BL',
        'communication_BL', 'decision_BL', 'md_score_BL', 'md_scale_BL',
        'age_3_BL', 'pb_621_BL', 'know_score_BL', 'w_monbirth_BL', 'wbmi_BL',
        'wbmi_cat_BL', 'ced_BL', 'whb_BL', 'dd10r_score_EL', 'dd10r_min_EL',
        'gravidity_EL', 'md_score_EL', 'md_scale_EL', 'treat', 'dep_ratio',
        'dd10r_score_m_EL', 'dd10r_min_m_EL', 'dd10r_min_EL',
        'dd10r_score_m_y1', 'dd10r_min_m_y1', 'dd10r_score_m_y2',
        'dd10r_min_m_y2', 'dd10r_score_m_y3', 'dd10r_min_m_y3',
        'dd10r_score_m_sur', 'dd10r_min_m_sur', 'dd10r_score_m_el',
        'dd10r_min_m_el', 'avg_hfias_sandEL', 'num_crops_havg',
        'num_crops_wavg', 'num_crops_ravg', 'lit_cat', 'anemic_all_BL',
        'impgarden_BL', 'DD_score_avg_sandEL', 'num_crops_avg_cat',
        'num_crops_avg_bin', 'avg_hfias_cat', 'avg_hfias_bin',
        'avg_hfias_cat_sandEL', 'avg_hfias_bin_sandEL', 'avg_hfias_bin_rev',
        'avg_hfias_bin_sandEL_rev', 'avg_hfias_cat_rev',
        'avg_hfias_cat_sandEL_rev', 'avg_hfias_rev', 'avg_hfias_sandEL_rev',
        'md_d13_EL_rev', 'num_crops_avg', 'avg_hfias', 'DD_min_sandEl',
        'md_d13_BL', 'md_d13_EL', 'md_d13_BL_rev', 'md_d13_EL_rev', 'md_d12_BL',
        'md_d12_EL', 'md_d12_BL_rev', 'md_d12_EL_rev', 'md_d11_BL', 'md_d11_EL',
        'md_d11_BL_rev', 'md_d11_EL_rev', 'wealth_BL', 'dec_BL', 'quint_BL',
        'terc_BL', 'wealth2_BL', 'dec2_BL', 'quint2_BL', 'terc2_BL']

for c in cols:
    RESULT[c] = RESULT.groupby('wcode')[c].ffill().bfill()

# Check NA values
RESULT.isnull().sum()
RESULT.columns[RESULT.isnull().any()]
#%%

# Save to CSV
RESULT.to_csv('Data/Cluster100_10mflood_diet_df.csv', index=False)


#%%%

# # Address NA values (Assume that dietary score holds constant until next sample)
# RESULT['dd10r_score'] = RESULT['dd10r_score'].fillna(method='ffill', axis=0)
# RESULT['dd10r_min'] = RESULT['dd10r_min'].fillna(method='ffill', axis=0)
# RESULT['treatment'] = RESULT['treatment'].fillna(method='ffill', axis=0)
# RESULT.drop(RESULT.index[RESULT['year_month'] == '2015-09'], inplace=True)  # for missing satellite data
# RESULT = RESULT.fillna(0)  # for baseline flooding

# 1 month time lag for flooding (score & bin)
# groups = RESULT.groupby('wcode')
# RESULT['DDLag_score'] = ''
# RESULT['DDLag_bin'] = ''
# for admin, group in groups:
#     # Score
#     group['timelag1'] = group['dd10r_score'].shift(-1)
#     mask = group['timelag1'].index
#     RESULT.loc[mask, 'DDLag_score'] = group['timelag1']
#     # Binary
#     group['timelag2'] = group['dd10r_min'].shift(-1)
#     mask = group['timelag2'].index
#     RESULT.loc[mask, 'DDLag_bin'] = group['timelag2']


# Set select columns to integer
# cols = ['year', 'month', 'treatment', 'dd10r_score', 'DDLag_score', 'DDLag_bin']
# for c in cols:
#     RESULT[c] = RESULT[c].astype(int)

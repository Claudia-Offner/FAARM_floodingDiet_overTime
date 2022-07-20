# DESCRIPTION

# IMPORT
import pandas as pd
import os
from Collators import Panelist, Organiser
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

# IMPORTANT - set file path to data folder location
data_path = 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/' \
            '- DD-Flooding TimeSeries - CO/4. Data/Final'
os.chdir(data_path)


# ====================================================================================
# GET GEE FLOODING DATA
# ====================================================================================

# Read Flood/ Environmental Data from CSV
gee_df = pd.read_csv('1_GEE_df.csv', low_memory=False)

# ====================================================================================
# GET WDDS DATA
# ====================================================================================

# Read diet data from original csv (DF 1)
# bi = pd.read_csv('Data/FAARM/JW_womensDD_long.csv', low_memory=False) # !!!!!!
# bi_sub = bi.loc[:, ('wcode', 'c_code', 'dov', 'treatment', 'dd10r_score', 'dd10r_min', 'dd10r_score_m', 'dd10r_min_m',
#                     'ramadan', 'preg')]
# df_name = 'Cluster100_10mflood_diet_df'

# Read diet data from new csv (DF 2) - w/ weights
bi = pd.read_csv('FAARM/JW_CompiledDD_Apr22.csv', low_memory=False)
bi_sub = bi.loc[:, ('wcode', 'c_code', 'dov', 'treatment', 'dd10r_score', 'dd10r_min', 'dd10r_score_m', 'dd10r_min_m',
                    'ramadan', 'preg', 'wdiet_wt')]
df_name = '2_FAARM_GEE_df.csv'

# Data organising & cleaning
bi_org = Organiser(bi_sub[:]).format()  # wcodes already unnested
bi_Diet = Panelist(bi_org[:]).get_panels()
# bi_Diet.isnull().sum()  # 2 c_codes missing ; 93 panels missing (rounds after endline)
bi_Diet['panel'] = bi_Diet['panel'].fillna('end')

# Add time variables
bi_Diet = Panelist(bi_Diet).get_dd_mm_yyyy()
bi_Diet['month'] = bi_Diet['month'].apply(lambda f: '{0:0>2}'.format(f))
bi_Diet['year_month'] = bi_Diet['year'].astype(str) + '-' + bi_Diet['month'].astype(str)
bi_Diet.rename(columns={'dov': 'dov_bi_Diet'}, inplace=True)
# bi_Diet['season'] = bi_Diet['season'].fillna(method='ffill', axis=0)

 
# ====================================================================================
# MERGE DATA
# ====================================================================================

# Get all year-month combinations for data
bi_Diet = bi_Diet.sort_values(by=['wcode', 'panel'])
# bi_Diet.loc[bi_Diet['panel'] == 'base', 'year_month'] = '0-0'  # set baseline to 0-0 for diet
ym = list(set(list(bi_Diet['year_month']) + list(gee_df['year_month'])))
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
x = pd.merge(df, gee_df, how='left', on=['c_code', 'year_month'])
RESULT = pd.merge(x, bi_Diet, how='left', on=['wcode', 'c_code', 'year_month'])

# Clean up columns
RESULT = RESULT.drop(['month_y', 'year_y', 'dov_bi_Diet'], axis=1)
RESULT.rename(columns={'panel_x': 'panel', 'year_x': 'year', 'month_x': 'month'}, inplace=True)

 
# ====================================================================================
# DATA CLEANING
# ====================================================================================

# Fill na for years & months
year = ['2015', '2016', '2017', '2018', '2019', '2020']
for y in year:
    RESULT.loc[RESULT['year_month'].str.contains(y, regex=False), 'year'] = float(y)
month = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12']
for m in month:
    RESULT.loc[RESULT['year_month'].str.contains(m, regex=False), 'month'] = float(m)

 
# Fill in missing Seasons (months): 1(Sept-Oct);2(Nov-Dec);3(Jan-Feb);4(Mar-Apr);5(May-June);6(Jul-Aug)
season_DD = {1: 'Jan/Feb', 2: 'Jan/Feb', 3: 'Mar/Apr', 4: 'Mar/Apr', 5: 'May/Jun', 6: 'May/Jun',
             7: 'Jul/Aug', 8: 'Jul/Aug', 9: 'Sept/Oct', 10: 'Sept/Oct', 11: 'Nov/Dec', 12: 'Nov/Dec'}
season_nums = {'Jan/Feb': 1, 'Mar/Apr': 2, 'May/Jun': 3, 'Jul/Aug': 4, 'Sept/Oct': 5, 'Nov/Dec': 6}
season_names = {1: 'Jan/Feb', 2: 'Mar/Apr', 3: 'May/Jun', 4: 'Jul/Aug', 5: 'Sept/Oct', 6: 'Nov/Dec'}
RESULT['season_DD'] = RESULT['month']
RESULT['season_DD'] = RESULT['season_DD'].replace(season_DD)
RESULT['season'] = RESULT['season_DD']
RESULT['season'] = RESULT['season'].replace(season_nums)

# Dummy code binary DD
RESULT['dd10r_min_m'] = RESULT['dd10r_min_m'].map({'Inadequate diet': 0, 'Diet diverse': 1})
RESULT['dd10r_min'] = RESULT['dd10r_min'].map({'Inadequate diet': 0, 'Diet diverse': 1})
RESULT['preg'] = RESULT['preg'].map({'0)no': 0, '1)yes': 1})

# Drop string columns
RESULT.drop(['year_month', 'panel'], axis=1)

# Aggregate dataset so mean is taken for numeric values and mode is taken for all else
RESULT = RESULT.groupby(['c_code', 'wcode', 'year', 'season'], as_index=False).mean()
col = ['month', 'treatment', 'dd10r_min', 'dd10r_min_m', 'ramadan', 'preg']
for c in col:
    RESULT[c] = RESULT[c] + 0.01
    RESULT[c] = round(RESULT[c])

# 1 season time lag for flooding (score)
groups = RESULT.groupby('wcode')
RESULT['Flood_1Lag'] = ''
RESULT['season_flood'] = ''
for woman, group in groups:
    # Flood
    group['timelag1'] = group['perc_flooded'].shift(1)
    mask = group['timelag1'].index
    RESULT.loc[mask, 'Flood_1Lag'] = group['timelag1']
    # Season
    group['timelag2'] = group['season'].shift(1)
    mask = group['timelag2'].index
    RESULT.loc[mask, 'season_flood'] = group['timelag2']

 
RESULT['Flood_1Lag'] = pd.to_numeric(RESULT['Flood_1Lag'])  # set to numeric

# Add season names again
RESULT['season_DD'] = RESULT['season']
RESULT['season_DD'] = RESULT['season_DD'].replace(season_names)
RESULT['season_flood'] = RESULT['season_flood'].replace(season_names)
RESULT['season_flood'] = RESULT['season_flood'].fillna('Nov/Dec')  # Fill season flood NA with Jan/Feb

 
# Reorganise data
RESULT['month'] = RESULT['month'].astype(int).apply(lambda f: '{0:0>2}'.format(f))
RESULT['year_month'] = RESULT['year'].astype(int).astype(str) + '-' + RESULT['month'].astype(str)
RESULT['season'] = RESULT['season'].astype(int).apply(lambda f: '{0:0>2}'.format(f))
RESULT['year_season'] = RESULT['year'].astype(int).astype(str) + '-' + RESULT['season'].astype(int).astype(str)
RESULT = RESULT.sort_values(by=['wcode', 'year_month']).reset_index().drop(['index'], axis=1)  # sort values

# Check missing data
# RESULT.isnull().sum()
# RESULT['year_season'].value_counts()
# len(pd.unique(RESULT['year_season']))
 
# ====================================================================================
# GET BASELINE CHARACTERISTICS
# ====================================================================================

base = pd.read_csv('FAARM/TS_FSN-MH-Data_220314.csv', low_memory=False)
base_sub = base.loc[:, ('wcode', 'c_code', 'g_2h_BL', 'fam_type_BL', 'wi_hl_BL', 'wi_al_BL', 'wi_land_BL',
                        'num_crops_BL', 'woman_edu_cat__BL', 'hfias_BL', 'hfias_score_BL', 'hfias_cat_BL', 'hfias_d_BL',
                        'mobility_BL', 'support_BL', 'communication_BL', 'decision_BL', 'md_score_BL',
                        'md_scale_BL', 'age_3_BL', 'pb_621_BL', 'know_score_BL', 'w_monbirth_BL', 'wbmi_BL',
                        'wbmi_cat_BL', 'ced_BL', 'whb_BL', 'treatment',
                        'dd10r_score_m_BL', 'dd10r_min_m_BL', 'dd10r_score_BL', 'dd10r_min_BL',
                        'dd10r_score_m_EL', 'dd10r_min_m_EL', 'dd10r_score_EL', 'dd10r_min_EL',
                        'gravidity_EL', 'md_score_EL', 'md_scale_EL', 'treat', 'dep_ratio',
                        'avg_hfias_sandEL', 'num_crops_havg', 'num_crops_wavg',
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
        'hfias_cat_BL', 'hfias_d_BL', 'mobility_BL', 'support_BL',
        'dd10r_score_m_BL', 'dd10r_min_m_BL', 'dd10r_score_BL', 'dd10r_min_BL',
        'dd10r_score_m_EL', 'dd10r_min_m_EL', 'dd10r_score_EL', 'dd10r_min_EL',
        'communication_BL', 'decision_BL', 'md_score_BL', 'md_scale_BL',
        'age_3_BL', 'pb_621_BL', 'know_score_BL', 'w_monbirth_BL', 'wbmi_BL',
        'wbmi_cat_BL', 'ced_BL', 'whb_BL', 'gravidity_EL', 'md_score_EL',
        'md_scale_EL', 'treat', 'dep_ratio', 'avg_hfias_sandEL', 'num_crops_havg',
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
# RESULT.columns[RESULT.isnull().any()]


# Save to CSV
# path = df_name + '.csv'
RESULT.to_csv(df_name, index=False)

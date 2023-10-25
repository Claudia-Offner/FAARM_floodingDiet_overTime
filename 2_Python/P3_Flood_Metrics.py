# DESCRIPTION

# IMPORT
import os
import pandas as pd
from Collators import Statistics
import matplotlib.pyplot as plt
import numpy as np

# FUNCTION

def lagger(cols, df, lag):
    groups = df.groupby('wcode')
    for c in cols:
        df[c + '_lag'] = ''
        for woman, group in groups:
            group['timelag1'] = group[c].shift(lag)
            mask = group['timelag1'].index
            df.loc[mask, c + '_lag'] = group['timelag1']

        df[c + '_lag'] = pd.to_numeric(df[c + '_lag'])  # set to numeric

    return df


# IMPORTANT - set file path to data folder location
data_path = 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/' \
            '- DD-Flooding TimeSeries - CO/4. Data/Final'
os.chdir(data_path)


# FUNCTIONS
def visual(x, var, title):

    # Plot raw data
    x.plot('year_season', y=[var, 'avSeason_mean'])
    plt.title(title)
    plt.show()

    # Plot differenced data
    x.plot('year_season', y='diff')
    plt.axhline(y=0, color='r', linestyle='-')
    # plt.axhline(y=0.05, color='r', linestyle='dashed')
    # plt.axhline(y=-0.05, color='r', linestyle='dashed')
    plt.title(title)
    plt.show()


def seasonal_weight(d, var):

    d['weight'] = 1
    d.loc[d['season'] == 2, 'weight'] = 1.5
    # d.loc[df['season'] == 3, 'weight'] = 0.75
    d['flooded_weight'] = d['weight'] * d[var]

    return d

# ====================================================================================
# CODE
# ====================================================================================


df = pd.read_csv('2_FAARM_GEE_df2.csv', low_memory=False)
# df = pd.read_csv('1_GEE_df.csv', low_memory=False)


# FOCUS on clusters & regional flooding (do not need wcode data)
# df['r_Areakm2'] = 12298  # add sylhet division total km2
# df['r_code'] = 'Sylhet'
sub = df.drop(['wcode'], axis=1).groupby(['c_code', 'year_season', 'year', 'season', 'month'], as_index=False).mean()
sub = sub[['c_code', 'year_season', 'year', 'season', 'month', 'c_Areakm2', 'c_floodedAreakm2',
           'perc_flooded', 'r_floodedAreakm2']]
# sub['r_perc_flooded'] = sub['r_floodedAreakm2'] / sub['r_Areakm2']

# Statistics over time (for all c_codes)
ystat_c = Statistics(sub).stats_by_time('perc_flooded', 'year', range(2015, 2020, 1))
ystat_c = ystat_c.add_prefix('avYearly_').rename(columns={'avYearly_year': 'year'})
sstat_c = Statistics(sub).stats_by_time('perc_flooded', 'season', range(1, 7, 1))
sstat_c = sstat_c.add_prefix('avSeason_').rename(columns={'avSeason_season': 'season'})

# Statistics over time (for Sylhet division)
# ystat_r = Statistics(sub).stats_by_time('r_perc_flooded', 'year', range(2015, 2020, 1))
# ystat_r = ystat_r.add_prefix('avYearly_').rename(columns={'avYearly_year': 'year'})
# sstat_r = Statistics(sub).stats_by_time('r_perc_flooded', 'season', range(1, 7, 1))
# sstat_r = sstat_r.add_prefix('avSeason_').rename(columns={'avSeason_season': 'season'})

# NOTE: Gaps in months is due to seasonal aggregation - not useful to examine
# mstat = Statistics(sub).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
# mstat = mstat.add_prefix('avMonthly_').rename(columns={'avMonthly_month': 'month'})
# stats_c = Statistics(df).general_stats().drop(['count'], axis=0)


# ====================================================================================
# TEST: Weighted Metric & Anomaly Metric
# ====================================================================================

# Division Average
# div = sub.drop(['c_code', 'c_Areakm2', 'c_floodedAreakm2'], axis=1).groupby(['r_code', 'year_season'],
#                                                                                             as_index=False).mean()
# div = pd.merge(div, sstat_r[['season', 'avSeason_mean']], how='left', on=['season'])
# div = seasonal_weight(div, 'r_perc_flooded')  # Weight
# div['diff'] = div['perc_flooded']-div['avSeason_mean']  # Anomaly
# # Visualise
# visual(div, 'perc_flooded', 'Division Level')

# All Cluster Average
all_clust = sub.drop(['c_code'], axis=1).groupby(['year_season'], as_index=False).mean()
all_clust = pd.merge(all_clust, sstat_c[['season', 'avSeason_mean']], how='left', on=['season'])
all_clust = seasonal_weight(all_clust, 'perc_flooded')  # Weight
all_clust['diff'] = all_clust['perc_flooded']-all_clust['avSeason_mean']  # Anomaly
# Visualise
visual(all_clust, 'perc_flooded', 'Cluster Level (All)')

# Individual Cluster Average
c = sub.groupby(['c_code', 'season'], as_index=False).mean()
# Average by cluster and season
c = c[['c_code', 'season', 'perc_flooded']].rename(columns={'perc_flooded': 'avSeason_mean'})
# Merge back with wcode data and extract difference between cluster seasonal average and time point
sing_clust = pd.merge(sub, c, how='left', on=['c_code', 'season'])
sing_clust = seasonal_weight(sing_clust, 'perc_flooded')  # Weight
sing_clust['diff'] = sing_clust['perc_flooded']-sing_clust['avSeason_mean']  # Anomaly
# Visualse individual cluster
sing_clust = sing_clust[sing_clust['c_code'] == 43]
visual(sing_clust, 'perc_flooded', 'Single Cluster Level')


# ====================================================================================
# EXTRACT DATA
# ====================================================================================

# Calculate difference between flood instance by cluster value
y = df[['c_code', 'year_season', 'perc_flooded']].drop_duplicates(subset=['c_code', 'year_season'], keep='last').reset_index(drop=True)
groups = y.groupby('c_code')
y['flooded_diff'] = ''
for cluster, group in groups:
    # Flood weight
    group['diff'] = np.insert(np.diff(group['perc_flooded']), 0, 0)
    mask = group['diff'].index
    y.loc[mask, 'flooded_diff'] = group['diff']

y.loc[y['year_season'] == '2015-1', 'flooded_diff'] = 'NA'
y = y.replace('NA', np.NaN)

# Take the overall cluster seasonal average and subtract from wcode data
RESULT = pd.merge(df, sstat_c[['season', 'avSeason_mean']], how='left', on=['season'])
RESULT = pd.merge(RESULT, y[['c_code', 'year_season', 'flooded_diff']], how='left', on=['c_code', 'year_season'])  # Diff
RESULT = seasonal_weight(RESULT, 'perc_flooded')  # Weight
RESULT['flooded_anom'] = RESULT['perc_flooded']-RESULT['avSeason_mean']  # Anomaly
RESULT['flooded_anom_w'] = RESULT['flooded_weight']-RESULT['avSeason_mean']  # Weighted Anomaly

# Create 1 seasonal time lag for every metric
RESULT = lagger(['flooded_weight', 'flooded_anom', 'flooded_anom_w'], RESULT, 1)

#%%
# Save
RESULT.to_csv('3_FloodMetrics2.csv', index=False)



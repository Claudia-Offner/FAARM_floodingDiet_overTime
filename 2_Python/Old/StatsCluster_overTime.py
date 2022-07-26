import pandas as pd
from Collators import Panelist, Statistics, Organiser
import numpy as np
import warnings
from scipy.stats.stats import pearsonr
import matplotlib.pyplot as plt
import plotly.express as px
warnings.simplefilter(action='ignore', category=FutureWarning)

# flood_df = pd.read_csv('Data/Cluster100_10mflood_df.csv', low_memory=False)
RESULT = pd.read_csv('../Data/Cluster100_10mflood_diet_df.csv', low_memory=False)
# RESULT = RESULT[RESULT['panel'] != 'base']

#%%
# ====================================================================================
# GET STATISTICS - There are temporal trends for flooding (not for diet)
# ====================================================================================

# Get percent coverage
df16 = RESULT[RESULT['year'] == 2016]
df17 = RESULT[RESULT['year'] == 2017]
df18 = RESULT[RESULT['year'] == 2018]
df19 = RESULT[RESULT['year'] == 2019]

# Check Years
ystat_Flood = Statistics(RESULT).stats_by_time('perc_flooded', 'year', range(2016, 2020, 1))
ystat_DD = Statistics(RESULT).stats_by_time('dd10r_score', 'year', range(2016, 2020, 1))

# Check Months
mstat_Flood = Statistics(RESULT).stats_by_time('perc_flooded', 'year_month', range(1, 13, 1))
mstat_DD = Statistics(RESULT).stats_by_time('dd10r_score', 'month', range(1, 13, 1))

# Compare months over years
mstat_Flood16 = Statistics(df16).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
mstat_Flood17 = Statistics(df17).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
mstat_Flood18 = Statistics(df18).stats_by_time('perc_flooded', 'month', range(1, 13, 1))
mstat_Flood19 = Statistics(df19).stats_by_time('perc_flooded', 'month', range(1, 13, 1))

# Check Months
mstat_DD16 = Statistics(df16).stats_by_time('dd10r_score', 'month', range(1, 13, 1))
mstat_DD17 = Statistics(df17).stats_by_time('dd10r_score', 'month', range(1, 13, 1))
mstat_DD18 = Statistics(df18).stats_by_time('dd10r_score', 'month', range(1, 13, 1))
mstat_DD19 = Statistics(df19).stats_by_time('dd10r_score', 'month', range(1, 13, 1))

#%%
# ====================================================================================
# TEST ASSOCIATIONS - Continuous variables
# ====================================================================================

# Test Preliminary Association (No aggregation): DD & Flood% (strong evience, small effect size)
print('Continuous Raw Association', pearsonr(RESULT['perc_flooded'], RESULT['dd10r_score']))

# Test Association with av year-month
RESULT['year_month'] = RESULT['year'].astype(int).astype(str) + '-' + RESULT['month'].astype(int).astype(str)
year_month = RESULT.groupby(['wcode', 'year_month'], as_index=False).mean()
test = year_month[year_month['year_month'] != '0-0']
print('Continuous Year-month Association', pearsonr(test['perc_flooded'], test['dd10r_score']))

# Test Association with av year-month & DD lag
groups = year_month.groupby('wcode')
timelag = {}
for admin, group in groups:
    group['timelag'] = group['dd10r_score'].shift(-1)
    timelag[admin] = tuple(group['timelag'])

timelag1 = pd.DataFrame.from_dict(timelag)
timelag1 = timelag1.T
timelag1.columns = set(year_month['year_month'].tolist())
timelag1 = timelag1.stack(dropna = False).reset_index()
timelag1.columns = ['admin_name','year_month','DDtimelag']
year_month['DDtimelag'] = timelag1['DDtimelag']
test = year_month.dropna()
test = test[test['year_month'] != '0-0']
print('Continuous Year-month Association w/ -1 month DD lag', pearsonr(test['perc_flooded'], test['DDtimelag']))

# Test association with the deviation from the overall monthly averages
test = year_month[year_month['year_month'] != '0-0'].dropna()  #drop baseline & na values
test = pd.merge(test, mstat_Flood[['month', 'mean']], how='left', on=['month'])  # Add monthly average
test.rename(columns={'mean': 'floodMonthlyMean'}, inplace=True)
test['floodMeanAnomaly'] = test['perc_flooded'] - test['floodMonthlyMean']  # Take the difference
print('Continuous Year-month Association w/ -1 month DD lag & overall flood deviation', pearsonr(test['floodMeanAnomaly'], test['DDtimelag']))

# Test association with the deviation from the cluster code monthly averages
test = year_month[year_month['year_month'] != '0-0'].dropna()  #drop baseline & na values
x = test.groupby(['c_code', 'month'], as_index=False).mean()
y = pd.merge(test, x[['c_code', 'month', 'perc_flooded']], how='left', on=['c_code', 'month'])  # Add monthly average
y.rename(columns={'perc_flooded_x': 'perc_flooded', 'perc_flooded_y': 'percFlood_monthcode'}, inplace=True)
y['floodMeanAnomaly'] = y['perc_flooded'] - y['percFlood_monthcode']  # Take the difference
print('Continuous Year-month Association w/ -1 month DD lag & cluster-month flood deviation', pearsonr(y['floodMeanAnomaly'], y['DDtimelag']))


#%%
# TTest for treatment and flood perc
from scipy import stats
import matplotlib.pyplot as plt
treat = year_month[(year_month['treatment'] == 0)]
notreat = year_month[(year_month['treatment'] == 1)]
stats.levene(treat['perc_flooded'], notreat['perc_flooded'])
stats.levene(treat['dd10r_score'], notreat['dd10r_score'])

#%%
















#%%
# COMBINE DATASETS FOR COMPARISON OVER TIME (by months)
# Make overall monthly time series stats
ym_FloodAv = pd.DataFrame(columns=['month', 'min', 'max', 'sd', 'mean', 'kurt', 'skew', 'count', 'num_img', 'month2'])
mstat_Flood16['month2'] = '2016-' + mstat_Flood16['month'].astype(int).astype(str)
mstat_Flood17['month2'] = '2017-' + mstat_Flood17['month'].astype(int).astype(str)
mstat_Flood18['month2'] = '2018-' + mstat_Flood18['month'].astype(int).astype(str)
mstat_Flood19['month2'] = '2019-' + mstat_Flood19['month'].astype(int).astype(str)
ym_FloodAv = ym_FloodAv.append(mstat_Flood16, ignore_index=True)
ym_FloodAv = ym_FloodAv.append(mstat_Flood17, ignore_index=True)
ym_FloodAv = ym_FloodAv.append(mstat_Flood18, ignore_index=True)
ym_FloodAv = ym_FloodAv.append(mstat_Flood19, ignore_index=True)

# Add monthly average
x = pd.merge(ym_FloodAv, mstat_Flood[['month', 'mean']], how='left', on=['month'])
x.rename(columns={'mean_y': 'monthMean', 'mean_x': 'actualMean'}, inplace=True)

# Take the difference
x['floodMeanAnomaly'] = x['actualMean'] - x['monthMean']
check = x[['month2', 'actualMean', 'monthMean', 'floodMeanAnomaly']]
#%%

# VISUALS FOR DEVIATIONS FROM MEAN
# Compare actual flood mean and monthly flood mean
plt.plot(x['month2'], x['actualMean'], label="Actual Flooded")
plt.plot(x['month2'], x['monthMean'], label="Average Flooded")
plt.xticks(rotation=45)
plt.legend()
plt.show()

# Check flood mean anomaly (difference)
plt.plot(x['month2'], x['floodMeanAnomaly'], label="Flooding difference from mean")
plt.axhline(y=0, color='r', linestyle='-.')
plt.xticks(rotation=45)
plt.legend()
plt.show()

# import plotly.express as px
# fig = px.line(x, x="month2", y="FloodAnomaly", title="Flooding difference from mean")
# fig.show()
# flood_df['perc_flooded'].describe()

#%%
# Add weights:
# *0.5 - for floods within the flooding season and deviation within +-0.05
# *1.5 - for floods within dry season and deviation within +- 0.2





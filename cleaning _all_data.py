import pandas as pd
import numpy as np
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)


df = pd.read_csv('Data/subset_result.csv', low_memory=False)

# Subset the df

df = df[['wcode', 'c_code', 'panel', 'treatment', 'new_women', 'Shape_Area',
         'c_flood_sum', 'c_flood_mean', 'r_flood_sum', 'r_flood_mean', 'r_max', 'r_min',
         'md_score', 'md_scale', 'dd10r_score', 'dd10r_min', 'hfias_score', 'hfias', 'hfias_cat', 'hfias_d',
         'preg', 'religion', 'education', 'age', 'wealth', 'dec', 'quint', 'terc']]
         # 'Cluster_Mean_ndvi', 'Region_Mean_ndvi', 'Cluster_Mean_prec', 'Region_Mean_prec'


# ====================================================================================
# DATA CLEANING
# ====================================================================================

# Set categorical variables
# g = df.columns.to_series().groupby(df.dtypes).groups
cols = ['panel', 'treatment', 'md_scale', 'dd10r_min', 'hfias', 'hfias_cat', 'hfias_d', 'preg', 'religion', 'education',
        'dec', 'quint', 'terc']
for c in cols:
    df[c] = pd.Categorical(df[c])

# Flood variables
df['Shape_Area'] = df['Shape_Area'].fillna(method='bfill', axis=0)
cols = ['c_flood_sum', 'c_flood_mean', 'r_flood_sum', 'r_flood_mean', 'r_max', 'r_min', ]
for i in cols:
    df[i] = df[i].fillna(0)

# Treatment
df['treatment'] = df['treatment'].fillna(method='ffill', axis=0)

# ====================================================================================
# CUMULATIVE VARIABLES
# ====================================================================================

# ISSUE: Multiple panel records for various wcodes
# Get round values from epds
groups = df.groupby('wcode')
for name, group in groups:
    if group['panel'].duplicated().any() == True:
        d = group.duplicated(subset=['panel'])
        d_index = list(d[d].index)
        for i in d_index:
            df.loc[i, 'c_flood_sum'] = np.nan
            df.loc[i, 'r_flood_sum'] = np.nan
            # df.loc[i, 'Cluster_Mean_ndvi'] = np.nan
            # df.loc[i, 'Region_Mean_ndvi'] = np.nan
            # df.loc[i, 'Cluster_Mean_prec'] = np.nan
            # df.loc[i, 'Region_Mean_prec'] = np.nan


# CUMULATIVE ENVIRONMENT VARIABLE
# df['c_ndvi_cum'] = df[['wcode', 'Cluster_Mean_ndvi']].groupby('wcode').cumsum()
# df['r_ndvi_cum'] = df[['wcode', 'Region_Mean_ndvi']].groupby('wcode').cumsum()
# df['c_prec_cum'] = df[['wcode', 'Cluster_Mean_prec']].groupby('wcode').cumsum()
# df['r_prec_cum'] = df[['wcode', 'Region_Mean_prec']].groupby('wcode').cumsum()
# CUMULATIVE FLOODING VARIABLES
df['c_flood_cum'] = df[['wcode', 'c_flood_sum']].groupby(['wcode']).cumsum()
df['r_flood_cum'] = df[['wcode', 'r_flood_sum']].groupby(['wcode']).cumsum()

# ====================================================================================
# FLOODING VARIABLES
# ====================================================================================

# 3 categorical variable based on IQR (CLUSTER)
df['c_flood_cum'].describe()
iqr = 7.823529 - 0.000000
max = df['c_flood_cum'].max()
df['c_flood_cat'] = pd.cut(df.c_flood_cum, bins=[-1, 0, iqr, max],
                           labels=['no flooding', 'some flooding', 'extensive flooding'])
# Get dichotomous variable (CLUSTER)
df['c_flood_d'] = pd.cut(df.c_flood_cum, bins=[-1, 0, max], labels=['no flooding', 'flooding'])


# 3 categorical variable based on IQR (REGION)
df['r_flood_cum'].describe()
iqr = 3.828923e+06 - 5.431288e+05
max = df['r_flood_cum'].max()
df['r_flood_cat'] = pd.cut(df.r_flood_cum, bins=[-1, 0, iqr, max],
                           labels=['no flooding', 'some flooding', 'extensive flooding'])
# Get dichotomous variable (REGION)
df['r_flood_d'] = pd.cut(df.r_flood_cum, bins=[-1, 0, max], labels=['no flooding', 'flooding'])

# PERCENTAGE FLOODED
# Would be interesting to calculate the percentage of flooded pixels relative to cluster pixel area?


# ====================================================================================
# DEPRESSION VARIABLES
# ====================================================================================
# '0)Low probability of depression'
# '1)Most likely just periodic sadness'
# '2)possibility of PPD'
# '3)High probability of clinical depression'

x = df.loc[df['md_scale'] == '2)possibility of PPD']
x = x[['md_score', 'md_scale']]
x['md_score'].min()
df['md_scale_d'] = pd.cut(df.md_score, bins=[0, 13, 30], labels=['no depression', 'depression'])


# ====================================================================================
# SUBSET FOR PRELIMINARY ANALYSIS
# ====================================================================================

data = df[['wcode', 'c_code', 'panel', 'treatment',
           'c_flood_sum', 'c_flood_cum', 'c_flood_cat', 'c_flood_d',
           'r_flood_sum', 'r_flood_cum', 'r_flood_cat', 'r_flood_d',
           'md_score', 'md_scale', 'md_scale_d',
           'dd10r_score', 'dd10r_min',
           'hfias_score', 'hfias', 'hfias_cat', 'hfias_d',
           'preg', 'religion', 'education', 'age', 'terc']]
           # 'Cluster_Mean_ndvi', 'Region_Mean_ndvi', 'Cluster_Mean_prec', 'Region_Mean_prec',
           # 'c_ndvi_cum', 'r_ndvi_cum', 'c_prec_cum', 'r_prec_cum'


# Separate BL, P8, EL
keep_same = {'wcode', 'c_code', 'panel', 'treatment'}

bl = data.loc[data['panel'] == 'base']
bl.columns = ['{}{}'.format(c, '' if c in keep_same else '_bl') for c in bl.columns]
bl = bl.drop(['panel'], axis=1)

P8 = data.loc[data['panel'] == 'P8']
P8.columns = ['{}{}'.format(c, '' if c in keep_same else '_p8') for c in P8.columns]
P8 = P8.drop(['panel'], axis=1)


el = data.loc[data['panel'] == 'end']
el.columns = ['{}{}'.format(c, '' if c in keep_same else '_el') for c in el.columns]
el = el.drop(['panel'], axis=1)

# Merge EL and BL again
x = pd.merge(bl, P8, how='right', on=['wcode', 'c_code', 'treatment'])
full_trial = pd.merge(x, el, how='right', on=['wcode', 'c_code', 'treatment'])

# # Separate Trial data
# n_intervention = full_trial.loc[full_trial['treatment'] == 0]
# y_intervention = full_trial.loc[full_trial['treatment'] == 1]

full_trial.columns
#%%
# ====================================================================================
# Save to file
# ====================================================================================

full_trial.to_csv('Data/full_trial.csv', index=False)


#%%

# ====================================================================================
# Get Flooding Data
# ====================================================================================

flood = full_trial[['wcode', 'c_flood_sum_p8', 'c_flood_cum_p8', 'c_flood_cat_p8', 'c_flood_d_p8',
                    'r_flood_sum_p8', 'r_flood_cum_p8', 'r_flood_cat_p8', 'r_flood_d_p8',
                    # 'Cluster_Mean_ndvi_p8', 'Region_Mean_ndvi_p8', 'Cluster_Mean_prec_p8', 'Region_Mean_prec_p8',
                    # 'c_ndvi_cum_p8', 'r_ndvi_cum_p8', 'c_prec_cum_p8', 'r_prec_cum_p8',
                    'c_flood_sum_el', 'c_flood_cum_el', 'c_flood_cat_el', 'c_flood_d_el',
                    'r_flood_sum_el', 'r_flood_cum_el', 'r_flood_cat_el', 'r_flood_d_el']]
                    # 'Cluster_Mean_ndvi_el', 'Region_Mean_ndvi_el', 'Cluster_Mean_prec_el', 'Region_Mean_prec_el',
                    # 'c_ndvi_cum_el', 'r_ndvi_cum_el', 'c_prec_cum_el', 'r_prec_cum_el'


flood = flood.drop_duplicates(subset = 'wcode').reset_index().drop(['index'], axis=1)

flood.to_csv('Data/flood.csv', index=False)
flood.columns

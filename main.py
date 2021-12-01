import pandas as pd
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)


df = pd.read_csv('Data/subset_result.csv', low_memory=False)

df = df[['wcode', 'c_code', 'panel', 'treatment', 'new_women', 'Shape_Area', 'Cluster_Diff', 'Region_Diff', 'Maximum',
         'Minimum', 'Mean', 'Stdev', 'md_score', 'md_scale', 'dd10r_score', 'dd10r_min', 'hfias_score', 'hfias',
         'hfias_cat', 'hfias_d', 'preg', 'religion', 'education', 'age', 'wealth', 'dec', 'quint', 'terc', 'wealth2',
         'dec2', 'quint2', 'terc2']]


# Remove new_women (NOTE all new women joined in 2016)
x = df[~df['new_women'].isnull()]
x = list(x['wcode'])
for i in x:
    df = df.loc[df['wcode'] != i]

df = df.drop(['new_women'], axis=1).reset_index().drop(['index'], axis=1)

# ====================================================================================
# DATA CLEANING
# ====================================================================================

# Flood variables
df['Shape_Area'] = df['Shape_Area'].fillna(method='bfill', axis=0)
cols = ['Cluster_Diff', 'Region_Diff', 'Maximum', 'Minimum', 'Mean', 'Stdev']
for i in cols:
    df[i] = df[i].fillna(0)

# Treatment variable - forward fill
df['treatment'] = df['treatment'].fillna(method='ffill', axis=0)
df['religion'] = df['religion'].fillna(method='ffill', axis=0)
df['education'] = df['education'].fillna(method='ffill', axis=0)

# Add Age for endline
groups = df.groupby(['wcode'])
for name, group in groups:
    r = group.loc[group['panel'] == 'base']
    age = r['age']+4
    end = group.loc[group['panel'] == 'end']
    newage_index = list(end.index)
    for i in newage_index:
        df['age'][i] = age

# ====================================================================================
# SUBSET FOR PRELIMINARY ANALYSIS
# ====================================================================================

# Create cumulative variable for flooding
df['c_flood'] = df[['wcode', 'Cluster_Diff']].groupby('wcode').cumsum()
df['r_flood'] = df[['wcode', 'Region_Diff']].groupby('wcode').cumsum()

# Subset to necessary variables
data = df[['wcode', 'c_code', 'panel', 'treatment', 'c_flood', 'r_flood', 'md_score', 'md_scale',
           'dd10r_score', 'dd10r_min', 'hfias_score', 'hfias', 'hfias_cat', 'hfias_d', 'preg', 'religion',
           'education', 'age', 'terc']]  # use md_scale, dd10r_min, hfias_cat


# Separate EL and BL
bl = data.loc[data['panel'] == 'base']
keep_same = {'wcode', 'c_code', 'panel', 'treatment'}
bl.columns = ['{}{}'.format(c, '' if c in keep_same else '_BL') for c in bl.columns]
bl = bl.drop(['panel'], axis=1)

el = data.loc[data['panel'] == 'end']
keep_same = {'wcode', 'c_code', 'panel', 'treatment'}
el.columns = ['{}{}'.format(c, '' if c in keep_same else '_EL') for c in el.columns]
el = el.drop(['panel'], axis=1)

# Merge EL and BL again
subset = pd.merge(bl, el, how='right', on=['wcode', 'c_code', 'treatment'])

#%%
subset.isnull().sum()
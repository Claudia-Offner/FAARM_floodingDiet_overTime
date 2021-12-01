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

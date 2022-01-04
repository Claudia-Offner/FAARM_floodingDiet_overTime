import pandas as pd
from Collators import Statistics, Visualiser
import matplotlib.pyplot as plt
import warnings
import plotly.express as px
warnings.simplefilter(action='ignore', category=FutureWarning)

# Read Data
flood_df = pd.read_csv('Data/flood_df.csv', low_memory=True)
flood_2017 = flood_df.loc[flood_df['year'] == 2017]
flood_2017 = flood_2017.reset_index().drop(['index'], axis=1)

#%%
# ====================================================================================
# 2017 FLOOD: Descriptive Statistics
# ====================================================================================

# Check descriptive stats (2017)
stats = Statistics(flood_2017).general_stats()
print('Regional Flooding \n', stats['r_flood_diff'])
print('Cluster Flooding \n', stats['c_flood_diff'])

# Check Months
r_mstats_2017 = Statistics(flood_2017).stats_by_time('r_flood_diff', 'month', range(1, 13, 1))
print('2017 Region Flooding by Month\n', r_mstats_2017)
c_mstats_2017 = Statistics(flood_2017).stats_by_time('c_flood_diff', 'month', range(1, 13, 1))
print('2017 Region Flooding by Month\n', c_mstats_2017)

# Expect flooding to be worse in cluster 44
y = flood_2017.loc[flood_df['c_flood_diff'] > 10]

# VISUALS
# Compare histograms between regions
flood_2017['c_flood_diff'].hist()
plt.show(block=True)
plt.interactive(False)
flood_2017['r_flood_diff'].hist()
plt.show(block=True)
plt.interactive(False)

# Compare fluctuations over MONTHS between clusters and region
Visualiser(r_mstats_2017, c_mstats_2017).plotter('month', 'max', range(0, 12), [0, 11], '2017 Maximum by Region & Cluster \n (months)')
Visualiser(r_mstats_2017, c_mstats_2017).plotter('month', 'sd', range(0, 12), [0, 11], '2017 StnDev by Region & Cluster \n (months)')
Visualiser(r_mstats_2017, c_mstats_2017).plotter('month', 'mean', range(0, 12), [0, 11], '2017 Mean by Region & Cluster \n (months)')

# SATELLITE IMAGES (by year and month)
r_mstats_2017['num_img'].sum()  # 190 images
x = flood_2017.drop_duplicates(subset='dov')  # 190 images
ym_check_2017 = x.groupby(['year', 'month'], as_index=False).count()
# Weird counting for cluster stat columns - Check december (12) 2015 & June (6) 2017 & May (5) 2019
ym_check_2017 = ym_check_2017[['year', 'month', 'panel']]  # panel reps the number of satellite images collected for time frame

#%%
# Examine correlations - corplots
corrmat = flood_2017.corr()
fig = px.imshow(corrmat, zmin=-1, zmax=1, color_continuous_scale= 'rdbu')
fig.update_layout(title = {'text': '<b>Covariate Correlation Matrix</b>',
                   'font': {'size':20, 'family':'Arial', 'color':'black'},
                   'xanchor':'center',
                   'x': 0.5,
                   'y': 0.95 },
                  font_family= 'Arial',
                  font_color='black',
                  margin = dict(t=70,r=70,b=70,l=70),
                  showlegend = True,
                  width = 700, height = 600,
                  autosize = False )
fig.show(renderer='browser')


#%%
# ====================================================================================
# Compare to FAARM 2017 flood experience survey
# ====================================================================================

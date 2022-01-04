#%%
import pandas as pd
import numpy as np
import json
import re
import matplotlib.pyplot as plt
from sklearn import preprocessing


def get_simple_keys(data):
    # Check JSON Keys
    result = []
    for key in data.keys():
        if type(data[key]) != dict:
            result.append(key)
        else:
            result += get_simple_keys(data[key])
    return result


class Organiser:

    def __init__(self, df):
        self.df = df

    def format(self, date=None):
        d = self.df
        """
        Format dataframe: set wcode & dov, sort df and create timestamp
        """
        if 'wcode1' in d.columns:
            d.rename(columns={'wcode1': 'wcode'}, inplace=True)
        names = ['today_hh', 'today_h', 'today_w']
        for i in names:
            if i in d.columns:
                d.rename(columns={i: 'dov'}, inplace=True)  # Set date to 'dov
        d['dov'] = d['dov'].replace(np.nan, date, regex=True)
        d['dov'] = pd.to_datetime(d['dov'], errors='coerce')  # create time stamp
        res = d.sort_values(by=['wcode', 'dov']).reset_index().drop(['index'], axis=1)  # sort values
        return res

    def set_na(self, cols, date=None):
        d = self.format(date)
        # Identify NA values
        for c in cols:
            d[c] = d[c].replace('-7777', ' ', regex=True)
            d[c] = d[c].replace('.a', ' ', regex=True)
            d[c] = d[c].replace(r'^\s*$', np.nan, regex=True)
            d[f'NA{c}'] = d[c].notna()
        return d

    def unnest_wcodes(self, date=None):

        d = self.set_na(['wcode2', 'wcode3'], date)
        # Unnest values
        unnested = pd.DataFrame(columns=list(d.columns)).drop(['wcode2', 'wcode3', 'NAwcode2', 'NAwcode3'], axis=1)
        for i in d.index:
            if d['NAwcode2'][i] == True:
                row = [d['wcode2'][i]] + list(d.iloc[i, 3:(len(d.columns) - 2)])
                unnested.loc[len(unnested)] = row
            elif d['NAwcode3'][i] == True:
                row = [d['wcode3'][i]] + list(d.iloc[i, 3:(len(d.columns) - 2)])
                unnested.loc[len(unnested)] = row
            else:
                continue
        orig = pd.DataFrame(d).drop(['wcode2', 'wcode3', 'NAwcode2', 'NAwcode3'], axis=1)
        unnested = unnested.append(orig)
        unnested['wcode'] = unnested['wcode'].astype(int)

        return Organiser(unnested).format()


class Panelist:

    def __init__(self, df):
        self.df = df

    def string_to_timestamp(self):
        """
        Get Time Stamps
        - INPUT: df where date col is in string format '12oct2017' and named 'dov'
        - OUTPUT: df with converted dov, day, month, year columns
        """
        d = self.df

        # Separate strings for time stamps
        items = d['dov'].values.tolist()
        time = pd.DataFrame(columns=['day', 'month', 'year'])

        for i in items:
            if pd.isnull([i]) == True:
                time.loc[len(time)] = ['', '', '']
            else:
                i = str(i).replace(" ", "")
                match = re.match(r"([0-9]+)([a-z]+)([0-9]+)", i, re.I)
                if match:
                    x = match.groups()
                    time.loc[len(time)] = x
        # Code months
        time = time.replace('jan', 1, regex=True)
        time = time.replace('feb', 2, regex=False)
        time = time.replace('mar', 3, regex=False)
        time = time.replace('apr', 4, regex=True)
        time = time.replace('may', 5, regex=True)
        time = time.replace('jun', 6, regex=True)
        time = time.replace('jul', 7, regex=True)
        time = time.replace('aug', 8, regex=True)
        time = time.replace('sep', 9, regex=True)
        time = time.replace('oct', 10, regex=True)
        time = time.replace('nov', 11, regex=True)
        time = time.replace('dec', 12, regex=True)

        # Create Timestamps
        time = time.astype(str)
        time['time'] = time['year'] + '-' + time['month'] + '-' +time['day']
        time['dov'] = pd.to_datetime(time['time'], errors='coerce')
        time = time.replace(r'^\s*$', np.nan, regex=True)
        d['dov'] = time['dov']

        return d

    def get_dd_mm_yyyy(self):
        """
        Get Day / Month / Year Columns
        - INPUT:
        - OUTPUT:
        """
        d = self.df
        if d['dov'].dtype == '<M8[ns]':
            d = self.df
        elif d['dov'].dtype == 'O':
            d = self.string_to_timestamp()

        # Assign to df
        idx = d.columns.get_loc('dov')
        d.insert(loc=idx+1, column='day', value=pd.to_datetime(d['dov']).dt.day)
        d.insert(loc=idx+2, column='month', value=pd.to_datetime(d['dov']).dt.month)
        d.insert(loc=idx+3, column='year', value=pd.to_datetime(d['dov']).dt.year)
        cols = ['day', 'month', 'year']
        d[cols] = d[cols].apply(pd.to_numeric, downcast='integer', errors='coerce')

        return d

    def get_panels(self):
        '''
        Get panel categories
        - INPUT: df with timestamp column named 'dov'
        - OUTPUT: df with 'panel' column
        '''
        d = self.get_dd_mm_yyyy()


        rounds = {'base': ['2015-03-01', '2015-06-30'], # baseline
                  'P1': ['2015-07-01', '2015-12-31'],
                  'P2': ['2016-01-01', '2016-06-30'],
                  'P3': ['2016-07-01', '2016-12-31'],
                  'P4': ['2017-01-01', '2017-06-30'],
                  'P5': ['2017-07-01', '2017-12-31'],
                  'P6': ['2018-01-01', '2018-06-30'],
                  'P7': ['2018-07-01', '2018-12-31'],
                  'P8': ['2019-01-01', '2019-06-30'],
                  'end': ['2019-07-01', '2020-02-29']} # endline

        idx = d.columns.get_loc('dov')
        d.insert(loc=idx, column='panel', value= '')
        for key in rounds.keys():
               items = rounds[key]
               start_date = pd.to_datetime(items[0])
               end_date = pd.to_datetime(items[1])
               x = d.loc[:, 'dov'].between(start_date, end_date, inclusive=True)
               x = pd.DataFrame(x)
               for i in x.index:
                      if x.loc[i, ('dov')] == True:
                             d.loc[i, 'panel'] = key
                      else:
                          continue

        panel_categories = ['base', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'end']

        d.loc[:, 'panel'] = pd.Categorical(d.loc[:, 'panel'], categories=panel_categories)
        d = d.sort_values(by=['wcode', 'panel']).reset_index().drop(['index', 'day', 'month', 'year'], axis=1)

        return d


class Flooder:

    def __init__(self, file_name):
        self.file_name = file_name

    def get_json(self):
        """ Function for loading JSON files """
        path = self.file_name

        with open(path, "r") as f:
            itn = json.load(f)
        return itn

    def json_to_df(self):
        r1 = self.get_json()
        # create output data frame
        df = pd.DataFrame(
            columns=['cluster_co', 'panel', 'dov', 'Shape_Area', 'c_flood', 'c_min', 'c_max', 'c_mean', 'c_sd',
                     'r_flood_diff', 'r_max', 'r_min', 'r_mean', 'r_sd', 'OBJECTID', 'OBJECTID_1', 'Shape_Le_1',
                     'Shape_Leng'])
        images = len(r1['features'])
        for i in range(images):  # for every image in geojson
            image = r1['features'][i]['properties']['clusters']
            clusters = len(image)
            for c in range(clusters):  # for every cluster in Clusters
                # Get cluster stats
                prop = image[c]['properties']
                # Get regional stats
                prop['dov'] = r1['features'][i]['properties']['date']
                prop['panel'] = r1['features'][i]['properties']['panel']
                prop['r_flood_diff'] = r1['features'][i]['properties']['r_flood']
                prop['r_max'] = r1['features'][i]['properties']['r_max']
                prop['r_min'] = r1['features'][i]['properties']['r_min']
                prop['r_mean'] = r1['features'][i]['properties']['r_mean']
                prop['r_sd'] = r1['features'][i]['properties']['r_sd']
                df = df.append(prop, ignore_index=True)

        df.rename(columns={'cluster_co': 'c_code', 'Shape_Area': 'c_shape_area', 'c_flood': 'c_flood_diff'}, inplace=True)
        df = df.drop(['OBJECTID', 'OBJECTID_1', 'Shape_Le_1', 'Shape_Leng'], axis=1)

        return df


class Statistics:

    def __init__(self, dataset):
        self.dataset = dataset

    def general_stats(self):
        df = self.dataset
        desc = df.describe()
        desc.loc[len(desc)] = list(df.iloc[:, :].kurtosis())
        desc.loc[len(desc)] = list(df.iloc[:, :].skew())
        desc = desc.rename({8:'kurtosis', 9: 'skew'}, axis='rows')
        return desc

    def stats_by_time(self, col, time, r):
        df = self.dataset
        res = pd.DataFrame(columns=[time, 'max', 'sd', 'mean', 'kurt', 'skew', 'count', 'num_img'])
        for t in r:
            x = df.loc[df[time] == t]
            max = x[col].max().round(decimals=2)
            sd = x[col].std().round(decimals=2)
            mean = x[col].mean().round(decimals=2)
            kurt = x[col].kurtosis()
            skew = x[col].skew()
            count = x[col].count().round(decimals=2)
            num_img = x['dov'].drop_duplicates().count()
            res.loc[len(res)] = [t, max, sd, mean, kurt, skew, count, num_img]
        return res


class Visualiser:

    def __init__(self, df1, df2):
        self.df1 = df1
        self.df2 = df2

    def compare_stats(self, time, stat):
        df1 = self.df1
        df2 = self.df2
        comp = pd.merge(df1, df2, how='right', on=time)
        comp_norm = pd.DataFrame(preprocessing.MinMaxScaler().fit_transform(comp[[f'{stat}_x', f'{stat}_y']]))
        comp_norm.rename(columns={0: f'region_{stat}', 1: f'cluster_{stat}'}, inplace=True)
        if time == 'month':
            comp_norm[time] = pd.to_datetime(comp[time], format='%m').dt.month_name().str.slice(stop=3)
        if time == 'year':
            comp_norm[time] = df1['year'].astype(int).astype(str)
        return comp_norm

    def plotter(self, time, stat, tick_r, x_lim, title):
        df = self.compare_stats(time, stat)
        ax = df.plot()
        ax.xaxis.set_ticks(tick_r)
        ax.set_xlim(x_lim)
        ax.set_xticklabels(list(df[time]), rotation=45)
        ax.set_title(title)
        plt.show(block=True)
        plt.interactive(False)


class Environment:

    def __init__(self, file_name):
        self.file_name = file_name

    def get_json(self):
        """ Function for loading JSON files """
        path = self.file_name

        with open(path, "r") as f:
            itn = json.load(f)
        return itn

    def get_environment(self):
        r1 = self.get_json()
        # create output data frame
        df = pd.DataFrame(
            columns=['cluster_co', 'panel', 'dov', 'Cluster_Mean', 'Region_Mean',
                     'OBJECTID', 'OBJECTID_1', 'Shape_Area', 'Shape_Le_1', 'Shape_Leng'])
        images = len(r1['features'])
        for i in range(images):  # for every image in geojson
            image = r1['features'][i]['properties']['Clusters']
            clusters = len(image)
            for c in range(clusters):  # for every cluster in Clusters
                # Get cluster stats
                prop = image[c]['properties']
                # Get regional stats
                prop['dov'] = r1['features'][i]['properties']['Date']
                prop['panel'] = r1['features'][i]['properties']['Panel']
                prop['Region_Mean'] = r1['features'][i]['properties']['Region_Mean']
                df = df.append(prop, ignore_index=True)

        df = df.drop(['OBJECTID', 'OBJECTID_1', 'Shape_Area', 'Shape_Le_1', 'Shape_Leng'], axis=1)
        df.rename(columns={'cluster_co': 'c_code'}, inplace=True)
        keep_same = {'c_code', 'panel'}
        df.columns = ['{}{}'.format(c, '' if c in keep_same else "_{}".format(self.file_name[:4])) for c in df.columns]

        return df


#%%
# class Flooder:
#
#     def __init__(self, file_name):
#         self.file_name = file_name
#
#     def get_json(self):
#         """ Function for loading JSON files """
#         path = self.file_name
#
#         with open(path, "r") as f:
#             itn = json.load(f)
#         return itn
#
#     def json_to_df(self):
#         r1 = self.get_json()
#         # create output data frame
#         df = pd.DataFrame(
#             columns=['cluster_co', 'panel', 'dov', 'Shape_Area', 'Cluster_Diff', 'Region_Diff', 'Maximum',
#                      'Minimum', 'Mean', 'Stdev', 'OBJECTID', 'OBJECTID_1', 'Shape_Le_1', 'Shape_Leng'])
#         images = len(r1['features'])
#         for i in range(images):  # for every image in geojson
#             image = r1['features'][i]['properties']['Clusters']
#             clusters = len(image)
#             for c in range(clusters):  # for every cluster in Clusters
#                 # Get cluster stats
#                 prop = image[c]['properties']
#                 # Get regional stats
#                 prop['dov'] = r1['features'][i]['properties']['Date']
#                 prop['panel'] = r1['features'][i]['properties']['Panel']
#                 prop['Region_Diff'] = r1['features'][i]['properties']['Region_Diff']
#                 prop['Maximum'] = r1['features'][i]['properties']['Maximum']
#                 prop['Minimum'] = r1['features'][i]['properties']['Minimum']
#                 prop['Mean'] = r1['features'][i]['properties']['Mean']
#                 prop['Stdev'] = r1['features'][i]['properties']['Stdev']
#                 df = df.append(prop, ignore_index=True)
#
#         df = df.drop(['OBJECTID', 'OBJECTID_1', 'Shape_Le_1', 'Shape_Leng'], axis=1)
#
#         return df


#%%
# ag = pd.read_csv('Data/Bi/HH_Ag_Prod_Div.csv', low_memory=False)



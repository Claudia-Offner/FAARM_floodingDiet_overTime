#%%
import pandas as pd
import numpy as np
import json
import re

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
                  1: ['2015-08-01', '2015-10-31'],
                  2: ['2015-11-01', '2015-12-31'],
                  3: ['2016-01-01', '2016-02-29'],
                  4: ['2016-03-01', '2016-04-30'],
                  5: ['2016-05-01', '2016-06-30'],
                  6: ['2016-07-01', '2016-08-31'],
                  7: ['2016-09-01', '2016-10-31'],
                  8: ['2016-11-01', '2016-12-31'],
                  9: ['2017-01-01', '2017-02-28'],
                  10: ['2017-03-01', '2017-04-30'],
                  11: ['2017-05-01', '2017-06-30'],
                  12: ['2017-07-01', '2017-08-31'],
                  13: ['2017-09-01', '2017-10-31'],
                  14: ['2017-11-01', '2017-12-31'],
                  15: ['2018-01-01', '2018-02-28'],
                  16: ['2018-03-01', '2018-04-30'],
                  17: ['2018-05-01', '2018-06-30'],
                  18: ['2018-07-01', '2018-08-31'],
                  19: ['2018-09-01', '2018-10-31'],
                  20: ['2018-11-01', '2018-12-31'],
                  21: ['2019-01-01', '2019-02-28'],
                  22: ['2019-03-01', '2019-04-30'],
                  23: ['2019-05-01', '2019-06-30'],
                  24: ['2019-07-01', '2019-08-31'],
                  'end': ['2019-09-01','2020-02-29']} # endline

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

        round_categories = ['base', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15',
                            '16', '17', '18', '19', '20', '21', '22', '23', '24', 'end']

        d.loc[:, 'panel'] = d.loc[:, 'panel'].astype(str)
        d.loc[:, 'panel'] = pd.Categorical(d.loc[:, 'panel'], categories=round_categories)
        d = d.sort_values(by=['wcode', 'panel']).reset_index().drop(['index'], axis=1)

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
            columns=['dov', 'Panel', 'cluster_co', 'Shape_Area', 'Cluster_Diff', 'Region_Diff', 'Maximum',
                     'Minimum', 'Mean', 'Stdev', 'OBJECTID', 'OBJECTID_1', 'Shape_Le_1', 'Shape_Leng'])
        images = len(r1['features'])
        for i in range(images):  # for every image in geojson
            image = r1['features'][i]['properties']['Clusters']
            clusters = len(image)
            for c in range(clusters):  # for every cluster in Clusters
                # Get cluster stats
                prop = image[c]['properties']
                # Get regional stats
                prop['dov'] = r1['features'][i]['properties']['Date']
                prop['Panel'] = r1['features'][i]['properties']['Panel']
                prop['Region_Diff'] = r1['features'][i]['properties']['Region_Diff']
                prop['Maximum'] = r1['features'][i]['properties']['Maximum']
                prop['Minimum'] = r1['features'][i]['properties']['Minimum']
                prop['Mean'] = r1['features'][i]['properties']['Mean']
                prop['Stdev'] = r1['features'][i]['properties']['Stdev']
                df = df.append(prop, ignore_index=True)

        df = df.drop(['OBJECTID', 'OBJECTID_1', 'Shape_Le_1', 'Shape_Leng'], axis=1)

        return df



#%%





#%%
# os.chdir('C:/Users/offne/Documents/FAARM/')
#
# ag = pd.read_csv('Data/Bi/HH_Ag_Prod_Div.csv', low_memory=False)


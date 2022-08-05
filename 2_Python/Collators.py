# DESCRIPTION

# IMPORT
import pandas as pd
import geopandas as gpd
import numpy as np
import json
import re
import matplotlib.pyplot as plt
from sklearn import preprocessing
from shapely.geometry import Polygon


# OOP

class Extractor:


    def __init__(self, path):
        """
        Method for self
        :param path: directory path and file name
        """
        self.path = path

    def get_json(self):
        """
        Method for loading JSON files to be processed
        :return: Dictionary variable from json
        """
        path = self.path

        with open(path, "r") as f:
            itn = json.load(f)
        return itn

    def json_to_df(self, main_props, out_param=None):
        """
        Method extracts cluster specific data for 1 dimension (i.e. 1 image). Pay attention to the column names, they
        should be specific to data at hand (use get_simple_keys to check).

        :param main_props: Main properties to extract for each image
        :return: Pandas dataframe with one row for every instance.
        """
        r1 = self.get_json()

        # Create output data frame
        df = pd.DataFrame()
        features = len(r1['features'])
        for i in range(features):  # for every image in geojson
            prop = r1['features'][i]['properties']
            if out_param is not None:
                prop['Date'] = r1['features'][i][out_param]  # IF there is feature information outside of properties
            df = df.append(prop, ignore_index=True)

        df = df.filter(main_props)
        df.rename(columns={'enum_c_cod': 'c_code'}, inplace=True)

        return df

    def nested_json_to_df(self, main_props, nested_feature, nested_props):
        """
        This function iterates over an image collection and extracts nested data for each image (i.e. multiple clusters)
        Pay attention to the column names, they should be specific to data at hand (use get_simple_keys to check).

        :param main_props: Main properties to extract for each image (values will be the same for nested features)
        :param nested_feature: JSON key name containing nest information (i.e. 'clusters')
        :param nested_props: Properties to extract for every nested instance
        :return: Pandas dataframe of main & nested instances in long format (i.e. images & clusters as rows; properties
        as cols)
        """

        r1 = self.get_json()

        # create output data frame
        df = pd.DataFrame()
        features = len(r1['features'])
        for f in range(features):  # for every feature in geojson
            main = r1['features'][f]['properties']
            feat = r1['features'][f]['properties'][nested_feature]
            nested = len(feat)
            for n in range(nested):  # for every feature in nested
                # Get nested feature stats
                prop = feat[n]['properties']
                # Get main feature stats
                for m in main_props:
                    prop[m] = main[m]

                df = df.append(prop, ignore_index=True)

        df = df.filter(main_props + nested_props)
        df.rename(columns={'enum_c_cod': 'c_code', 'date': 'dov'}, inplace=True)

        return df


class SpatialProcessor:

    def __init__(self, file_name):
        """
        Method for self
        :param file_name: directory path and file name
        """
        self.file_name = file_name

    def load_file(self):
        """
        Method for loading any spatial data type into a readable format.
        :return:  Shapely dataframe for shapefiles; rasterio dataframe for rasters; GeoPandas data frame for csvs
        """

        file = self.file_name

        if isinstance(file, gpd.GeoDataFrame):
            return file
        elif file.endswith('.shp'):
            df = gpd.read_file(file)  # Geopandas

        return df

    def transform_df(self, crs=None):
        """
        Method to change CRS as desired
        :param crs: EPSG codes in numeric format (i.e. 4326)
        :return:
        """

        d = self.load_file()
        if crs is not None:
            df = d.to_crs(epsg=crs)  # 4326 = WSG84
            return df
        else:
            return d

    def spatial_desc(self, crs=None, plot=False):
        """
        Method to extract spatial information required for data handling decision making (i.e. file CRS, bounding box
        coordinates, plot map). This can be applied to any spatial data type and option to change CRS and plot is
        provided.

        :param crs: EPSG codes in numeric format (i.e. 4326)
        :param plot: maps provided geometry
        :return: Summary of spatial information and dictionary containing said information.
        """

        data = self.transform_df(crs=crs)
        file_name = self.file_name

        if plot is True:
            # Plot
            fig, ax = plt.subplots(figsize=(10, 10))
            data.plot(ax=ax, color='teal')
            ax.set_title(file_name)
            plt.show()

        if isinstance(data, gpd.GeoDataFrame):

            desc = {'crs': data.crs, 'bbox': data.total_bounds}
            # data.bounds  # bbox of every polygon
            # data.centroid  # Centroid of every polygon

        else:
            desc = 'You need to write more code for this file type'

        return desc

    def create_bbox(self, crs=None, buffer=None, plot=True):
        """
        Method for creating a bounding box shapefile from any spatial data type, using coordinates identified from
        spatial description. Option to set CRS, buffer and plot.

        :param crs: EPSG codes in numeric format (i.e. 4326)
        :param buffer: Buffer around bbox, depends on CRS of the shape (check projection is in meters)
        :param plot: maps bounding box against original geometry to assess if the buffer/crs is as desired
        :return: polygon shapefile of data bounding box
        """
        data = self.transform_df(crs=crs)
        desc = self.spatial_desc(crs=crs)
        file_name = self.file_name

        # Extract bounding coordinates from descriptives and create a shapefile.
        bbox = desc['bbox']
        shape = Polygon([[bbox[0], bbox[1]],
                        [bbox[0], bbox[3]],
                        [bbox[2], bbox[3]],
                        [bbox[2], bbox[1]]])

        shape = gpd.GeoDataFrame(pd.DataFrame(['p1'], columns=['geom']),
                         geometry=[shape])

        if buffer is not None:
            shape['geometry'] = shape.geometry.buffer(buffer, join_style=2)

        bbox = shape.set_crs(crs, allow_override=True)

        if plot is True:
            # Plot
            fig, ax = plt.subplots(figsize=(10, 10))
            shape.plot(ax=ax, color='teal')
            data.plot(ax=ax, color='lightgrey', alpha=0.25)
            ax.set_title('bbox_' + file_name)
            plt.show()

        return bbox


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
            if pd.isnull([i]):
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
        time['time'] = time['year'] + '-' + time['month'] + '-' + time['day']
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
        d.insert(loc=idx + 1, column='day', value=pd.to_datetime(d['dov']).dt.day)
        d.insert(loc=idx + 2, column='month', value=pd.to_datetime(d['dov']).dt.month)
        d.insert(loc=idx + 3, column='year', value=pd.to_datetime(d['dov']).dt.year)
        cols = ['day', 'month', 'year']
        d[cols] = d[cols].apply(pd.to_numeric, downcast='integer', errors='coerce')

        return d

    def get_panels(self):
        """
        Get panel categories
        - INPUT: df with timestamp column named 'dov'
        - OUTPUT: df with 'panel' column
        """
        d = self.get_dd_mm_yyyy()

        rounds = {'base': ['2015-03-01', '2015-06-30'],  # baseline
                  'P1': ['2015-07-01', '2015-12-31'],
                  'P2': ['2016-01-01', '2016-06-30'],
                  'P3': ['2016-07-01', '2016-12-31'],
                  'P4': ['2017-01-01', '2017-06-30'],
                  'P5': ['2017-07-01', '2017-12-31'],
                  'P6': ['2018-01-01', '2018-06-30'],
                  'P7': ['2018-07-01', '2018-12-31'],
                  'P8': ['2019-01-01', '2019-06-30'],
                  'end': ['2019-07-01', '2020-02-29']}  # endline

        idx = d.columns.get_loc('dov')
        d.insert(loc=idx, column='panel', value='')
        for key in rounds.keys():
            items = rounds[key]
            start_date = pd.to_datetime(items[0])
            end_date = pd.to_datetime(items[1])
            x = d.loc[:, 'dov'].between(start_date, end_date, inclusive=True)
            x = pd.DataFrame(x)
            for i in x.index:
                if x.loc[i, 'dov']:  # PARENTHESES REMOVED FROM 'DOV'
                    d.loc[i, 'panel'] = key
                else:
                    continue

        panel_categories = ['base', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'end']

        d.loc[:, 'panel'] = pd.Categorical(d.loc[:, 'panel'], categories=panel_categories)
        d = d.sort_values(by=['wcode', 'panel']).reset_index().drop(['index', 'day', 'month', 'year'], axis=1)

        return d


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
            if d['NAwcode2'][i]:
                row = [d['wcode2'][i]] + list(d.iloc[i, 3:(len(d.columns) - 2)])
                unnested.loc[len(unnested)] = row
            elif d['NAwcode3'][i]:
                row = [d['wcode3'][i]] + list(d.iloc[i, 3:(len(d.columns) - 2)])
                unnested.loc[len(unnested)] = row
            else:
                continue
        orig = pd.DataFrame(d).drop(['wcode2', 'wcode3', 'NAwcode2', 'NAwcode3'], axis=1)
        unnested = unnested.append(orig)
        unnested['wcode'] = unnested['wcode'].astype(int)

        return Organiser(unnested).format()


class Statistics:

    def __init__(self, dataset):
        self.dataset = dataset

    def general_stats(self):
        # NOTE: Deprication warning for skew & kurtosis
        df = self.dataset
        desc = df.describe()
        desc.loc[len(desc)] = list(df.iloc[:, :].kurtosis())
        desc.loc[len(desc)] = list(df.iloc[:, :].skew())
        desc = desc.rename({8: 'kurtosis', 9: 'skew'}, axis='rows')
        return desc

    def stats_by_time(self, col, time, r):
        df = self.dataset
        res = pd.DataFrame(columns=[time, 'min', 'max', 'sd', 'mean', 'kurt', 'skew', 'count'])  # num_img
        for t in r:
            x = df.loc[df[time] == t]
            mini = round(x[col].min(), 2)
            maxi = round(x[col].max(), 2)
            sd = round(x[col].std(), 2)
            mean = round(x[col].mean(), 2)
            kurt = x[col].kurtosis()
            skew = x[col].skew()
            count = round(x[col].count(), 2)
            # num_img = x['dov'].drop_duplicates().count()
            res.loc[len(res)] = [t, mini, maxi, sd, mean, kurt, skew, count]  # num_img
        return res


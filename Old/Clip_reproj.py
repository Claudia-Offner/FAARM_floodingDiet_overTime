# https://towardsdatascience.com/zonal-statistics-algorithm-with-python-in-4-steps-382a3b66648a

''' This code is for when you have a folder of raster images (geoTIFF)
and would like to reproject them to match a vector shapefile so that
you can extract zonal statistics for said shapefile (takes multipolygons)'''

from osgeo import gdal
import os
import rasterio
from rasterstats import zonal_stats
import geopandas as gpd


# Functions
def rastClip_multi(vector, input, output):

    # Get list of all file names in inputut directory
    raster_list = os.listdir(input)

    # Loop over every raster
    for raster in raster_list:
        outputut_raster = f"CL_{raster}"
        warp = gdal.Warp(output + outputut_raster,
                         input + raster,
                         cutlineDSName=vector,  # Clip to shape
                         cropToCutline=True)
        warp = None  # Closes the files

def rastReproj_multi(input, output):

    # Get list of all file names in inputut directory
    raster_list = os.listdir(input)

    # Loop over every raster
    for raster in raster_list:
        outputut_raster = f"RP_{raster}"
        warp = gdal.Warp(output + outputut_raster,
                         input + raster,
                         dstSRS='EPSG:32646')  # Transform to CRS
        warp = None  # Closes the files

def zonalStats_multi(vector, input, folder):

    raster_list = os.listdir(input)
    shape = gpd.read_file(vector)

    for raster in raster_list:
        # Load Raster
        dem = rasterio.open(input+raster)
        # ZONAL STATS
        array = dem.read(1)
        affine = dem.transform
        x = zonal_stats(shape, array, affine=affine, stats=['min', 'max', 'mean', 'median', 'majority'],
                        all_touched=True, geojson_out=True)
        geostats = gpd.GeoDataFrame.from_features(x)  # format
        # Column name
        Str = raster[len(raster) - 16:-4]
        shape[Str] = geostats['mean']

    shape.rename(columns={'enum_c_cod': 'c_code'}, inplace=True)
    shape.drop(['area_km2', 'INSIDE_X', 'INSIDE_Y', 'geometry'], axis=1, inplace=True)
    shape.to_csv(f'{folder}/{folder}_clusterTimeSeries.csv', index=False)

    return shape

os.chdir('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/Other/Other Data Sources/World Clime Data/')

#%%
#### MAX TEMP

# Load vector paths
bbox_buff = 'C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/FAARM_buffer_updated/bbox_buffer_5km.shp'
cluster1000 = 'C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/FAARM_buffer_updated/enum_cCode_buffer1000.shp'
tmax_raw = 'tmax/1. RAW/'
tmax_clip = 'tmax/2. CLIPPED/'
tmax_reproj = 'tmax/3. REPROJECTED/'

rastClip_multi(vector=bbox_buff, input=tmax_raw, output=tmax_clip)
rastReproj_multi(input=tmax_clip, output=tmax_reproj)
tmax = zonalStats_multi(cluster1000, tmax_reproj, 'tmax')


#### MIN TEMP

# Load vector paths
bbox_buff = 'C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/FAARM_buffer_updated/bbox_buffer_5km.shp'
cluster1000 = 'C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/FAARM_buffer_updated/enum_cCode_buffer1000.shp'
tmin_raw = 'tmin/1. RAW/'
tmin_clip = 'tmin/2. CLIPPED/'
tmin_reproj = 'tmin/3. REPROJECTED/'

rastClip_multi(vector=bbox_buff, input=tmin_raw, output=tmin_clip)
rastReproj_multi(input=tmin_clip, output=tmin_reproj)
tmin = zonalStats_multi(cluster1000, tmin_reproj, 'tmin')

#%%
#### PREC

# Load vector paths
bbox_buff = 'C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/FAARM_buffer_updated/bbox_buffer_5km.shp'
cluster1000 = 'C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/FAARM_buffer_updated/enum_cCode_buffer1000.shp'
prec_raw = 'prec/1. RAW/'
prec_clip = 'prec/2. CLIPPED/'
prec_reproj = 'prec/3. REPROJECTED/'

rastClip_multi(vector=bbox_buff, input=prec_raw, output=prec_clip)
rastReproj_multi(input=prec_clip, output=prec_reproj)
prec = zonalStats_multi(cluster1000, prec_reproj, 'prec')

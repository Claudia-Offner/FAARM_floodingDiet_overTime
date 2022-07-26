//---------------------------------------------------------------------------------------------//
/////////////////////// RUN THE FOLLOWING SCRIPT IN GOOGLE EARTH ENGINE /////////////////////////

//---------------------------------------- IMPORT DATA  ---------------------------------------//

var elev1 = ee.Image("USGS/SRTMGL1_003"),
    temp1 = ee.ImageCollection("MODIS/061/MOD11A2"),
    prec1 = ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY"),
    ndvi1 = ee.ImageCollection("MODIS/061/MOD13Q1"),
    evap1 = ee.ImageCollection("MODIS/006/MOD16A2"),
    cluster = ee.FeatureCollection("users/offnerclaudia/cluster_buffer1000"),
    evapotranspirationVis = {"min":0,"max":300,"palette":["ffffff","fcd163","99b718","66a000","3e8601","207401","056201","004c00","011301"]},
    ndviVis = {"min":0,"max":8000,"palette":["FFFFFF","CE7E45","DF923D","F1B555","FCD163","99B718","74A901","66A000","529400","3E8601","207401","056201","004C00","023B01","012E01","011D01","011301"]},
    precipitationVis = {"min":1,"max":17,"palette":["001137","0aab1e","e7eb05","ff4a2d","e90000"]},
    landSurfaceTemperatureVis = {"min":13000,"max":16500,"palette":["040274","040281","0502a3","0502b8","0502ce","0502e6","0602ff","235cb1","307ef3","269db1","30c8e2","32d3ef","3be285","3ff38f","86e26f","3ae237","b5e22e","d6e21f","fff705","ffd611","ffb613","ff8b13","ff6e08","ff500d","ff0000","de0101","c21301","a71001","911003"]};

//---------------------------------------------------------------------------------------------//

// 1. Set bounding box
var bound = ee.Geometry.Polygon([[[91.3550689370853064, 24.3921216248458990], [91.6038783082803008, 24.3921216248458990], [91.6038783082803008, 24.7049478124276014], [91.3550689370853064, 24.7049478124276014]]]);
Map.centerObject(bound, 9); //Center map view to AOI

// 2. Filter data to bounding box & time frame
var elev = elev1.clip(bound); // 1 element
var temp = temp1.filter(ee.Filter.date('2014-12-31', '2020-01-01')).select('LST_Day_1km').map(clip_geo);
var prec = prec1.filter(ee.Filter.date('2014-12-31', '2020-01-01')).select('precipitation').map(clip_geo);
var ndvi = ndvi1.filter(ee.Filter.date('2014-12-31', '2020-01-01')).select('NDVI').map(clip_geo);
var evap = evap1.filter(ee.Filter.date('2014-12-31', '2020-01-01')).select('ET').map(clip_geo);

// 3. Visualise
Map.addLayer(elev, {min: 0, max: 60}, 'Elev');
Map.addLayer(temp, landSurfaceTemperatureVis, 'Temp');
Map.addLayer(prec, precipitationVis, 'Prec');
Map.addLayer(ndvi, ndviVis, 'NDVI');
Map.addLayer(evap, evapotranspirationVis, 'Evap');
Map.addLayer(cluster, {min: 0, max: -30}, 'Clusters');


// 4. Get zonal stats (mean)
var elev_res = cluster.map(for_every_cluster);
var temp_res = temp.map(for_every_TEMP); // yes
var prec_res = prec.map(for_every_PREC); // yes
var ndvi_res = ndvi.map(for_every_NDVI); // yes
var evap_res = evap.map(for_every_EVAP); // yes
// print(temp_res);

// 5. Export the FeatureCollection.

Export.table.toDrive({
  collection: elev_res,
  description: 'elev_res',
  folder: 'GEE',
  fileFormat: 'json'
});

Export.table.toDrive({
  collection: temp_res,
  description: 'temp_res',
  folder: 'GEE',
  fileFormat: 'json'
});

Export.table.toDrive({
  collection: prec_res,
  description: 'prec_res',
  folder: 'GEE',
  fileFormat: 'json'
});

Export.table.toDrive({
  collection: ndvi_res,
  description: 'ndvi_res',
  folder: 'GEE',
  fileFormat: 'json'
});

Export.table.toDrive({
  collection: evap_res,
  description: 'evap_res',
  folder: 'GEE',
  fileFormat: 'json'
});


// ########################
// PROCESSING FUNCTIONS
// ########################

function clip_geo(img){
    return img.clip(bound);
  }

function for_every_TEMP(img) {

  // EXTRACT SUM OF FLOODED AREA BY CLUSTER
  var for_every_cluster = function(cluster) {

  // Create geometry of feature
    var geometry = cluster.geometry();

    // Get mean
    var mean= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.mean(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var mean2 = ee.Number(mean.get('LST_Day_1km'));

    // Get MIN
    var min= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.min(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var min2 = ee.Number(min.get('LST_Day_1km'));

    // Get MAX
    var max= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.max(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var max2 = ee.Number(max.get('LST_Day_1km'))

    return cluster.set({'mean': mean2, 'min': min2, 'max': max2});

  };

  var cluster_img = cluster.map(for_every_cluster);
  var listOfImages = cluster_img.toList(cluster_img.size());

  return img.set({'clusters': listOfImages, 'date': img.date().format('YYYY-MM-dd')});

}

function for_every_PREC(img) {

  // EXTRACT SUM OF FLOODED AREA BY CLUSTER
  var for_every_cluster = function(cluster) {

  // Create geometry of feature
    var geometry = cluster.geometry();

    // Get mean
    var mean= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.mean(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var mean2 = ee.Number(mean.get('precipitation'))

  // Get MIN
    var min= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.min(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var min2 = ee.Number(min.get('precipitation'));

    // Get MAX
    var max= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.max(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var max2 = ee.Number(max.get('precipitation'))

    return cluster.set({'mean': mean2, 'min': min2, 'max': max2});

  };

  var cluster_img = cluster.map(for_every_cluster);
  var listOfImages = cluster_img.toList(cluster_img.size());

  return img.set({'clusters': listOfImages, 'date': img.date().format('YYYY-MM-dd')});

}

function for_every_NDVI(img) {

  // EXTRACT SUM OF FLOODED AREA BY CLUSTER
  var for_every_cluster = function(cluster) {

  // Create geometry of feature
    var geometry = cluster.geometry();

    // Get mean
    var mean= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.mean(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var mean2 = ee.Number(mean.get('NDVI')).multiply(0.0001); // Convert to get appropriate NDVI values (-1 to 1)

    // Get MIN
    var min= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.min(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var min2 = ee.Number(min.get('NDVI')).multiply(0.0001);

    // Get MAX
    var max= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.max(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var max2 = ee.Number(max.get('NDVI')).multiply(0.0001);

    return cluster.set({'mean': mean2, 'min': min2, 'max': max2});

  };

  var cluster_img = cluster.map(for_every_cluster);
  var listOfImages = cluster_img.toList(cluster_img.size());

  return img.set({'clusters': listOfImages, 'date': img.date().format('YYYY-MM-dd')});

}

function for_every_EVAP(img) {

  // EXTRACT SUM OF FLOODED AREA BY CLUSTER
  var for_every_cluster = function(cluster) {

  // Create geometry of feature
    var geometry = cluster.geometry();

    // Get mean
    var mean= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.mean(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var mean2 = ee.Number(mean.get('ET'));

    // Get MIN
    var min= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.min(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var min2 = ee.Number(min.get('ET'));

    // Get MAX
    var max= img.reduceRegion({
      geometry: geometry, // boundary shape file
      reducer: ee.Reducer.max(),
      scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
      bestEffort: true,
      maxPixels: 1e10
    });
    var max2 = ee.Number(max.get('ET'));

    return cluster.set({'mean': mean2, 'min': min2, 'max': max2});
  };

  var cluster_img = cluster.map(for_every_cluster);
  var listOfImages = cluster_img.toList(cluster_img.size());

  return img.set({'clusters': listOfImages, 'date': img.date().format('YYYY-MM-dd')});

}

function for_every_cluster(cluster) {

  // Create geometry of feature
  var geometry = cluster.geometry();

  // Get mean
  var mean= elev.reduceRegion({
    geometry: geometry, // boundary shape file
    reducer: ee.Reducer.mean(),
    scale: 250, // IMPORTANT - Don't go below 50 that will exceed memory
    bestEffort: true,
    maxPixels: 1e10
  });
  var mean2 = ee.Number(mean.get('elevation')).subtract(273.15);

  return cluster.set({'elev': mean2});

}



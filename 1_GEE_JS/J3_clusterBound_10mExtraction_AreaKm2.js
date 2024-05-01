//---------------------------------------------------------------------------------------------//
/////////////////////// RUN THE FOLLOWING SCRIPT IN GOOGLE EARTH ENGINE /////////////////////////

//---------------------------------------- IMPORT DATA  ---------------------------------------//

var s1 = ee.ImageCollection("COPERNICUS/S1_GRD"),
    JRC = ee.Image("JRC/GSW1_3/GlobalSurfaceWater"),
    WWF = ee.Image("WWF/HydroSHEDS/03VFDEM"),
    reference = ee.Image("users/offnerclaudia/FAARM_reference_image"),
    clusters = ee.FeatureCollection("users/offnerclaudia/cluster_buffer1000");

//------------------------------------------ GET DATA  ---------------------------------------//

// Get Relevant Districts - Districts Habiganj, Sunamganj - Sylhet division
var district = ee.Geometry.Polygon([[[91.3550689370853064, 24.3921216248458990], [91.6038783082803008, 24.3921216248458990], [91.6038783082803008, 24.7049478124276014], [91.3550689370853064, 24.7049478124276014]]])
Map.centerObject(district, 9); //Center map view to AOI
// Map.addLayer(district, {min: 0, max: -30, palette: 'pink'}, 'Clusters District');
Map.addLayer(reference, {min: 0, max: -30, palette: 'pink'}, 'Clusters District');

// Get Satellite Data
var collection = s1
  .filter(ee.Filter.eq('instrumentMode','IW')) // this is the sentinel band we are interested in
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
  .filter(ee.Filter.eq('orbitProperties_pass', 'DESCENDING'))
  .filter(ee.Filter.eq('resolution_meters', 10)) // makes sure you get the same resolution data
  .filterBounds(district)
  .select('VH');

// var refCollection = collection.filterDate('2017-03-01', '2017-03-31');
// var reference = refCollection.first();
//---------------------------- GET DATA FOR EACH SURVEILLANCE PERIOD --------------------------//
var result = get_flood(collection, '2017-04-01', '2017-05-01', 'test');
// var result = get_flood(collection, '2015-07-01', '2015-12-31', 'P1');
// var result = get_flood(collection, '2016-01-01', '2016-06-30', 'P2');
// var result = get_flood(collection, '2016-07-01', '2016-12-31', 'P3');
// var result = get_flood(collection, '2017-01-01', '2017-06-30', 'P4');
// var result = get_flood(collection, '2017-07-01', '2017-12-31', 'P5');
// var result = get_flood(collection, '2018-01-01', '2018-06-30', 'P6');
// var result = get_flood(collection, '2018-07-01', '2018-12-31', 'P7');
// var result = get_flood(collection, '2019-01-01', '2019-06-30', 'P8');
// var result = get_flood(collection, '2019-07-01', '2019-12-31', 'P9');
// var result = get_flood(collection, '2019-11-01', '2020-02-29', 'end');

print('Flood Results', result);

// Export the FeatureCollection.
Export.table.toDrive({
  collection: result,
  description: 'test',
  folder: 'GEE',
  fileFormat: 'json'
});

// ##################
// ANALYSIS FUNCTIONS
// ##################

function get_flood(collection, start, end, panel){

  var x = filter_Collection(collection, start, end);
  var result = x.map(get_data(panel));

  return result;

}

// Function for UN-SPIDER method for flood extraction
function get_data(panel){

  var wrap = function(img){
    // UN SPIDER FLOOD ANALYSIS

    // Subtract difference between the reference image and satellite image (Division is best method here)
    var difference = img.divide(reference);

    // Define Threshold
    var diffThreshold = 1.15;

    // Initial Estimate of flooded pixels
    var flooded = difference.gt(diffThreshold).rename('water').selfMask();

    // Remove all semi-permenant bodies of water w/ JRC
    var permanentWater = ee.Image(JRC.select('seasonality').gte(12).clip(district)); // any  pixel that has water 12months out of the year is a permenant body
    var flooded2 = flooded.where(permanentWater,0).selfMask();

    // Remove areas with high slope w/ WWF Hydroshed
    var slopeThreshold = 5; // depends on region
    var terrain = ee.Algorithms.Terrain(WWF);
    var slope = terrain.select('slope');
    var flooded3 = flooded2.updateMask(slope.lt(slopeThreshold));
    var connectedPixelsThreshold = 8;
    var connections = flooded3.connectedPixelCount(25);

    // Actual flooded area
    var flooded_area = flooded3.updateMask(connections.gt(connectedPixelsThreshold));


    // EXTRACT SUM OF FLOODED AREA BY REGION
    var areaImage  = flooded_area.multiply(ee.Image.pixelArea().reproject({crs:'EPSG:4326',scale:50}));
    var floodedSum = areaImage.reduceRegion({
        geometry: district, // boundary shape file
        reducer: ee.Reducer.sum(),
        scale: 10, // IMPORTANT - Don't go below 50 that will exceed memory
        bestEffort: true,
        maxPixels: 1e10
      });
    var floodedSumFin = ee.Number(floodedSum.get('water')).divide(1e6);


    // EXTRACT SUM OF FLOODED AREA BY CLUSTER
    var for_every_cluster = function(cluster) {

    // Create geometry of feature
      var geometry = cluster.geometry();

      // Extract Division Area (Km2)
      var clusArea = geometry.area();
      var clusAreaSqKm = ee.Number(clusArea).divide(1e6);

      var areaCluster = flooded_area.multiply(ee.Image.pixelArea());
      var floodedSum = areaCluster.reduceRegion({
        geometry: geometry, // boundary shape file
        reducer: ee.Reducer.sum(),
        scale: 10, // IMPORTANT - Don't go below 50 that will exceed memory
        bestEffort: true,
        maxPixels: 1e10
      });
      var floodedSum2 = ee.Number(floodedSum.get('water')).divide(1e6);

      return cluster.set({'c_floodedAreakm2': floodedSum2, 'c_Areakm2': clusAreaSqKm});

    };

    var cluster_img = clusters.map(for_every_cluster);
    var listOfImages = cluster_img.toList(cluster_img.size());
    var date = img.get('system:index');

    return img.set({'clusters': listOfImages, 'r_floodedAreakm2': floodedSumFin, 'date': date, 'panel': panel});
  };

  return wrap;
}

// ########################
// PRE-PROCESSING FUNCTIONS
// ########################

// Function to get date from image
function get_date(img) {
  return img.set('date', img.date().format('YYYY-MM-dd'));
  }

// Function to get clip image to geometry (district)
function clip_geo(img){
    return img.clip(district);
  }

// Function to smooth image collection w/ speckle filter
function smoothCollection(img){
  return ee.Image(toDB(RefinedLee(toNatural(img))));
}

// Function to get summary statistics from collection (min,max, stdv, mean)
function get_stats(img) {

  var mini = ee.Number(img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.min(),
    scale: 50,
    bestEffort: true,
  }).get('sum'));

  var maxi = ee.Number(img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.max(),
    scale: 50,
    bestEffort: true,
  }).get('sum'));

  var stdv = ee.Number(img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.stdDev(),
    scale: 50,
    bestEffort: true,
  }).get('sum'));

  var mean = ee.Number(img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.mean(),
    scale: 50,
    bestEffort: true,
  }).get('sum'));

  return img.set({'r_min': mini, 'r_max': maxi, 'r_sd': stdv, 'r_mean': mean});
  }

// Function to clean collection to specified date
function filter_Collection(collection, start, end){

  // Get all images within all available dry period prior to trial (Jan-May 2015)
  var collectionFilter = collection.filterDate(start, end);
  var collectionClip = collectionFilter.map(clip_geo); // clip geometry to district
  var collectionDate = collectionClip.map(get_date); // get timestamp
  // Mosaic images taken on the same day
  var mosaicList = collectionDate.aggregate_array('date').distinct()
    .map(function(date) {
      var d = ee.Date(date);
      return collectionDate.filterDate(d, d.advance(1, 'day')).mosaic().set('system:index', date);
    });
  var collectionMosaic = ee.ImageCollection.fromImages(mosaicList);
  // Smooth Images w/ Speckle Filtering Function
  var collectionSmooth = collectionMosaic.map(smoothCollection);
  // Check Summary Stats of mosaicked collection
  var result = collectionSmooth.map(get_stats);

  return result;
}



// ##########################
// Speckle Filtering Function
// ##########################

// Function to convert from dB
function toNatural(img) {
  return ee.Image(10.0).pow(img.select(0).divide(10.0));
}

//Function to convert to dB
function toDB(img) {
  return ee.Image(img).log10().multiply(10.0);
}

//Apllying a Refined Lee Speckle filter as coded in the SNAP 3.0 S1TBX:

//https://github.com/senbox-org/s1tbx/blob/master/s1tbx-op-sar-processing/src/main/java/org/esa/s1tbx/sar/gpf/filtering/SpeckleFilters/RefinedLee.java
//Adapted by Guido Lemoine

function RefinedLee(img) {
  // img must be in natural units, i.e. not in dB!
  // Set up 3x3 kernels
  var weights3 = ee.List.repeat(ee.List.repeat(1,3),3);
  var kernel3 = ee.Kernel.fixed(3,3, weights3, 1, 1, false);

  var mean3 = img.reduceNeighborhood(ee.Reducer.mean(), kernel3);
  var variance3 = img.reduceNeighborhood(ee.Reducer.variance(), kernel3);

  // Use a sample of the 3x3 windows inside a 7x7 windows to determine gradients and directions
  var sample_weights = ee.List([[0,0,0,0,0,0,0], [0,1,0,1,0,1,0],[0,0,0,0,0,0,0], [0,1,0,1,0,1,0], [0,0,0,0,0,0,0], [0,1,0,1,0,1,0],[0,0,0,0,0,0,0]]);

  var sample_kernel = ee.Kernel.fixed(7,7, sample_weights, 3,3, false);

  // Calculate mean and variance for the sampled windows and store as 9 bands
  var sample_mean = mean3.neighborhoodToBands(sample_kernel);
  var sample_var = variance3.neighborhoodToBands(sample_kernel);

  // Determine the 4 gradients for the sampled windows
  var gradients = sample_mean.select(1).subtract(sample_mean.select(7)).abs();
  gradients = gradients.addBands(sample_mean.select(6).subtract(sample_mean.select(2)).abs());
  gradients = gradients.addBands(sample_mean.select(3).subtract(sample_mean.select(5)).abs());
  gradients = gradients.addBands(sample_mean.select(0).subtract(sample_mean.select(8)).abs());

  // And find the maximum gradient amongst gradient bands
  var max_gradient = gradients.reduce(ee.Reducer.max());

  // Create a mask for band pixels that are the maximum gradient
  var gradmask = gradients.eq(max_gradient);

  // duplicate gradmask bands: each gradient represents 2 directions
  gradmask = gradmask.addBands(gradmask);

  // Determine the 8 directions
  var directions = sample_mean.select(1).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(7))).multiply(1);
  directions = directions.addBands(sample_mean.select(6).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(2))).multiply(2));
  directions = directions.addBands(sample_mean.select(3).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(5))).multiply(3));
  directions = directions.addBands(sample_mean.select(0).subtract(sample_mean.select(4)).gt(sample_mean.select(4).subtract(sample_mean.select(8))).multiply(4));
  // The next 4 are the not() of the previous 4
  directions = directions.addBands(directions.select(0).not().multiply(5));
  directions = directions.addBands(directions.select(1).not().multiply(6));
  directions = directions.addBands(directions.select(2).not().multiply(7));
  directions = directions.addBands(directions.select(3).not().multiply(8));

  // Mask all values that are not 1-8
  directions = directions.updateMask(gradmask);

  // "collapse" the stack into a singe band image (due to masking, each pixel has just one value (1-8) in it's directional band, and is otherwise masked)
  directions = directions.reduce(ee.Reducer.sum());

  //var pal = ['ffffff','ff0000','ffff00', '00ff00', '00ffff', '0000ff', 'ff00ff', '000000'];
  //Map.addLayer(directions.reduce(ee.Reducer.sum()), {min:1, max:8, palette: pal}, 'Directions', false);

  var sample_stats = sample_var.divide(sample_mean.multiply(sample_mean));

  // Calculate localNoiseVariance
  var sigmaV = sample_stats.toArray().arraySort().arraySlice(0,0,5).arrayReduce(ee.Reducer.mean(), [0]);

  // Set up the 7*7 kernels for directional statistics
  var rect_weights = ee.List.repeat(ee.List.repeat(0,7),3).cat(ee.List.repeat(ee.List.repeat(1,7),4));

  var diag_weights = ee.List([[1,0,0,0,0,0,0], [1,1,0,0,0,0,0], [1,1,1,0,0,0,0],
    [1,1,1,1,0,0,0], [1,1,1,1,1,0,0], [1,1,1,1,1,1,0], [1,1,1,1,1,1,1]]);

  var rect_kernel = ee.Kernel.fixed(7,7, rect_weights, 3, 3, false);
  var diag_kernel = ee.Kernel.fixed(7,7, diag_weights, 3, 3, false);

  // Create stacks for mean and variance using the original kernels. Mask with relevant direction.
  var dir_mean = img.reduceNeighborhood(ee.Reducer.mean(), rect_kernel).updateMask(directions.eq(1));
  var dir_var = img.reduceNeighborhood(ee.Reducer.variance(), rect_kernel).updateMask(directions.eq(1));

  dir_mean = dir_mean.addBands(img.reduceNeighborhood(ee.Reducer.mean(), diag_kernel).updateMask(directions.eq(2)));
  dir_var = dir_var.addBands(img.reduceNeighborhood(ee.Reducer.variance(), diag_kernel).updateMask(directions.eq(2)));

  // and add the bands for rotated kernels
  for (var i=1; i<4; i++) {
    dir_mean = dir_mean.addBands(img.reduceNeighborhood(ee.Reducer.mean(), rect_kernel.rotate(i)).updateMask(directions.eq(2*i+1)));
    dir_var = dir_var.addBands(img.reduceNeighborhood(ee.Reducer.variance(), rect_kernel.rotate(i)).updateMask(directions.eq(2*i+1)));
    dir_mean = dir_mean.addBands(img.reduceNeighborhood(ee.Reducer.mean(), diag_kernel.rotate(i)).updateMask(directions.eq(2*i+2)));
    dir_var = dir_var.addBands(img.reduceNeighborhood(ee.Reducer.variance(), diag_kernel.rotate(i)).updateMask(directions.eq(2*i+2)));
  }

  // "collapse" the stack into a single band image (due to masking, each pixel has just one value in it's directional band, and is otherwise masked)
  dir_mean = dir_mean.reduce(ee.Reducer.sum());
  dir_var = dir_var.reduce(ee.Reducer.sum());

  // A finally generate the filtered value
  var varX = dir_var.subtract(dir_mean.multiply(dir_mean).multiply(sigmaV)).divide(sigmaV.add(1.0));

  var b = varX.divide(dir_var);

  var result = dir_mean.add(b.multiply(img.subtract(dir_mean)));
  return(result.arrayFlatten([['sum']]));
}

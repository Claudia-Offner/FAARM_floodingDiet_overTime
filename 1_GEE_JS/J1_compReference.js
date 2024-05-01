//---------------------------------------------------------------------------------------------//
/////////////////////// RUN THE FOLLOWING SCRIPT IN GOOGLE EARTH ENGINE /////////////////////////

//---------------------------------------- IMPORT DATA  ---------------------------------------//

var Admin2 = ee.FeatureCollection("FAO/GAUL/2015/level2"),
    s1 = ee.ImageCollection("COPERNICUS/S1_GRD")

//------------------------------------------ GET DATA  ---------------------------------------//

// Get Relevant Districts - Districts Habiganj, Sunamganj - Sylhet division
// USE refClean JSON  with this ROI
var district = ee.FeatureCollection(Admin2).filter(ee.Filter.eq('ADM1_NAME', 'Sylhet')).geometry();
// USE this ROI for reference image creation
var district = ee.Geometry.Polygon([[[91.3550689370853064, 24.3921216248458990], [91.6038783082803008, 24.3921216248458990], [91.6038783082803008, 24.7049478124276014], [91.3550689370853064, 24.7049478124276014]]])
Map.centerObject(district, 9); //Center map view to AOI

// Get Satellite Data
var collection = s1
  .filter(ee.Filter.eq('instrumentMode','IW')) // this is the sentinel band we are interested in
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
  .filter(ee.Filter.eq('orbitProperties_pass', 'DESCENDING'))
  .filter(ee.Filter.eq('resolution_meters', 10)) // makes sure you get the same resolution data
  .filterBounds(district)
  .select('VH');

//------------------------------- CREATE COMPOSITE REFERENCE IMAGE --------------------------//

// Clean collection for reference period (includes speckle filtering & statistics)
var refClean = filter_Collection(collection, '2018-01-01', '2018-05-31'); // = 22 images
print('Reference Images (before selection)', refClean);

// Manually select images with standard deviations less than 2 = 9 images
// 8, 9, 10, 11, 12, 13, 14, 15, 16  - Sylhet
var listOfImages = refClean.toList(refClean.size());
var x0 = ee.Image(listOfImages.get(8));
var x1 = ee.Image(listOfImages.get(9));
var x2 = ee.Image(listOfImages.get(10));
var x3 = ee.Image(listOfImages.get(11));
var x4 = ee.Image(listOfImages.get(12));
var x5 = ee.Image(listOfImages.get(13));
var x6 = ee.Image(listOfImages.get(14));
var x7 = ee.Image(listOfImages.get(15));
var x8 = ee.Image(listOfImages.get(16));
// Create composite images via mean()
var refCollection = ee.ImageCollection([x0, x1, x2, x3, x4, x5, x6, x7, x8]);
var reference = refCollection.mean();

// print(refCollection);
Map.addLayer(reference, {min: -25, max: 5,palette: ['0000FF', 'FF0000']}, 'Composite Reference Image');

//---------------------------------- EXPORT REFERENCE IMAGE -----------------------------//

// Check CRS of satellte images
var projection = reference.select('sum').projection();
print(projection);

// Export the FeatureCollection.
Export.table.toDrive({
  collection: refClean,
  description: 'ReferenceSelection',
  folder: 'GEE',
  fileFormat: 'json'
});

// Export the image, specifying scale and region.
Export.image.toDrive({
  image: reference,
  description: 'reference_image',
  folder: 'GEE',
  scale: 30, // resolution of 30m (because 10-20m exceeds memory)
  crs: 'EPSG:3857', // Bangaldesh WSG proejeciton system
  maxPixels: 1e13 //'maxPixels' is the largest size dataset that you are able to export (in pixels)
});


// Use EPSG:3857 (projected coordinate system) for EPSG:4326 (geographic coordinate system)

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

  var mini0 = img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.min(),
    scale: 150,
    bestEffort: true,
  });

  var maxi0 = img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.max(),
    scale: 150,
    bestEffort: true,
  });

  var stdv0 = img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.stdDev(),
    scale: 150,
    bestEffort: true,
  });

  var mean0 = img.reduceRegion({
    geometry: district,
    reducer: ee.Reducer.mean(),
    scale: 150,
    bestEffort: true,
  });

  var mini = ee.Number(mini0.get('sum'));
  var maxi = ee.Number(maxi0.get('sum'));
  var stdv = ee.Number(stdv0.get('sum'));
  var mean = ee.Number(mean0.get('sum'));

  return img.set({'Minimum': mini, 'Maximum': maxi, 'Stdev': stdv, 'Mean': mean});
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

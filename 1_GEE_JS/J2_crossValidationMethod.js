//---------------------------------------------------------------------------------------------//
/////////////////////// RUN THE FOLLOWING SCRIPT IN GOOGLE EARTH ENGINE /////////////////////////

//---------------------------------------- IMPORT DATA  ---------------------------------------//

var Admin2 = ee.FeatureCollection("FAO/GAUL/2015/level2"),
    s1 = ee.ImageCollection("COPERNICUS/S1_GRD"),
    JRC = ee.Image("JRC/GSW1_3/GlobalSurfaceWater"),
    WWF = ee.Image("WWF/HydroSHEDS/03VFDEM"),
    gfd = ee.ImageCollection("GLOBAL_FLOOD_DB/MODIS_EVENTS/V1"),
    reference_img = ee.Image("users/offnerclaudia/FAARM_reference_image");
// Global Flood Database Parameters
var country = 'Bangladesh';         // Select counry to extract all national flood records Global Flood Database

// 4382: July 25, 2016 (2016-07-25) - 1.62 (89.39%) 10m ; 1.53 (90.17%) 30m
// var databaseID = 4382;              // Manually chec individual flooding events from Global Flood Database
// var start = '2016-07-25';           // Start date of event according to Global Flood Database
// var end = '2016-08-25';             // End date - mosaic all images within a month of start date

// 4459: March 30, 2017 (2017-03-30) - 1.60 (92.09%) 30m
var databaseID = 4459;              // Manually chec individual flooding events from Global Flood Database
var start = '2017-03-30';           // Start date of event according to Global Flood Database
var end = '2017-04-30';             // End date - mosaic all images within a month of start date

// 4508:  August 10, 2017 (2017-08-10) - 1.65 (86.26%) 10m; 1.58 (87.29%) 30m
// var databaseID = 4508;              // Manually chec individual flooding events from Global Flood Database
// var start = '2017-08-10';           // Start date of event according to Global Flood Database
// var end = '2017-09-10';             // End date - mosaic all images within a month of start date


// Extraction Method Paramters
// var region = 'Sylhet';              // Select study of region of method application
var polarization = 'VH';            // or 'VV' - VH mostly is the preferred polarization for flood mapping.However, it always depends on your study area, you can select 'VV' as well.
var pass_direction = 'DESCENDING';  // or 'ASCENDING' when images are being compared use only one pass direction. Consider changing this parameter, if your image collection is empty. In some areas more Ascending images exist than than descending or the other way around.
var difference_threshold = 1.6;    // threshold to be applied on the difference image (after flood - before flood). It has been chosen by trial and error (check model accuracy - aim to be over 80%). In case your flood extent result shows0 many false-positives or negatve signals, consider changing it!
var months = 12;                    // Months out of the year that a pixel has water, to identify it is a permenant water body to be removed from flood analysis
var slope = 5;                      // Analysis will remove areas with high slope, so this should be set according to region

// Cross Validation
var scale = 30;                    // Set scale of area extraction

// Results Sylhet: [4382 (82%), 4459 (83%), 4508 (85%)] - diff threshold of 1.15; scale 130m
// Results Cluster: [4382 (88%), 4459 (90%), 4508 (87% - 1.60)] - diff threshold of 1.60; scale 30m


//---------------------------------- GET SATELLITE DATA ---------------------------------//

// GET RELEVANT DISTRICTS - Districts Habiganj, Sunamganj - Sylhet division
// var district = ee.FeatureCollection(Admin2).filter(ee.Filter.eq('ADM1_NAME', region)).geometry();
var district = ee.Geometry.Polygon([[[91.3550689370853064, 24.3921216248458990], [91.6038783082803008, 24.3921216248458990], [91.6038783082803008, 24.7049478124276014], [91.3550689370853064, 24.7049478124276014]]])
// var habiganj = ee.FeatureCollection(Admin2).filter(ee.Filter.eq('ADM2_NAME', 'Habiganj')).geometry();
// var sunamganj = ee.FeatureCollection(Admin2).filter(ee.Filter.eq('ADM2_NAME', 'Sunamganj')).geometry();
// var district = habiganj.union(sunamganj, ee.ErrorMargin(1));

Map.centerObject(district, 9); //Center map view to AOI
Map.addLayer(district, {color: 'grey'}, 'Selected Districts');

// GET SATELLITE DATA
var collection = s1
  .filter(ee.Filter.eq('instrumentMode','IW')) // this is the sentinel band we are interested in
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', polarization))
  .filter(ee.Filter.eq('orbitProperties_pass', pass_direction))
  // .filter(ee.Filter.eq('resolution_meters', 10)) // makes sure you get the same resolution data
  .filterBounds(district)
  .select(polarization);

// SET REFERENCE IMAGE
var reference = reference_img;

//------------------------------- SET SURVEILLANCE PERIOD --------------------------//

// Create a collection of images for each Survellience Periods & Mosaic smae days together
var survCollection = collection.filterDate(start, end);
var survCollection = survCollection.map(clip_geo); // clip geometry to district
var survCollection = survCollection.map(get_date); // get date
var mosaicList = survCollection.aggregate_array('date')
  .distinct()
  .map(function(date) {
    var d = ee.Date(date);
    return survCollection.filterDate(d, d.advance(1, 'day')).mosaic().set('system:index', date);
  });
var survMosaic = ee.ImageCollection.fromImages(mosaicList);
var survSmooth = survMosaic.map(smoothCollection); // Smooth image collection
var surveillance = survSmooth.mosaic(); // takes most recent images to create full visual

//------------------------------- UN-SPIDER METHOD ANALYSIS --------------------------//

  // Smooth Images w/ Speckle Filtering Function (Below)
  var beforeFiltered = reference; // filter already applied
  var afterFiltered = surveillance;

  // Map.addLayer(beforeFiltered, {min:-25,max:0}, 'Reference Filtered', false);
  // Map.addLayer(afterFiltered, {min:-25,max:0}, 'Surveillance Filtered', false);

    // Subtract difference between the before and after (Division is best method here)
  var difference = afterFiltered.divide(beforeFiltered);

  // INITIAL ESTIMATE of flooded pixels
  var flooded = difference.gt(difference_threshold).rename('water').selfMask();

  // Map.addLayer(flooded, {min:0, max:1, palette: ['orange']}, 'Initial Flood Area', false);

  // Remove all semi-permenant bodies of water w/ JRC
  var permanentWater = ee.Image(JRC.select('seasonality').gte(months).clip(district)); // any  pixel that has water X months out of the year is a permenant body
  var flooded2 = flooded.where(permanentWater,0).selfMask();

  Map.addLayer(permanentWater.selfMask(), {min:0, max:1, palette: ['blue']}, 'Permanent Water', false);

  // Remove areas with high slope w/ WWF Hydroshed
  var slopeThreshold = slope; // depends on region
  var terrain = ee.Algorithms.Terrain(WWF);
  var slope = terrain.select('slope');
  var flooded3 = flooded2.updateMask(slope.lt(slopeThreshold));

  // Remove pixels that are not connected to other pixels
  var connectedPixelsThreshold = 15;
  var connections = flooded3.connectedPixelCount(25);

  // Map.addLayer(slope.gte(slopeThreshold).selfMask(), {min:0, max:1, palette: ['cyan']}, 'Steep Areas', false);

  // ACTUAL AREA of flooded pixels
  var flooded_area = flooded3.updateMask(connections.gt(connectedPixelsThreshold));

  Map.addLayer(flooded_area, {min:0, max:1, palette: ['red']}, 'Flooded Areas', false);


//------------------------------- CROSS VALIDATION ------------------------------- //

// Filter By country & Date
var country_range = gfd.filter(ee.Filter.eq('dfo_country', country)).filterDate('2014-10-01', '2019-12-31'); // filter to SARS data range
print('All National Floods (after Oct 2014)', country_range);

// Event id
var flood_ID = databaseID;
var flood_img = ee.Image(gfd.filterMetadata('id', 'equals', flood_ID).first());

// An individual flood event - unseasonal flooding in Bangladesh (shown in black)
Map.addLayer(flood_img.select('flooded').selfMask(),
            {min: 0, max: 1, palette: '001133'}, 'Flooding Event - Inundation Extent', false);

// The duration (number of days a flood event lasted).
var durationPalette = ['C3EFFE', '1341E8', '051CB0', '001133'];
Map.addLayer(flood_img.select('duration').selfMask(),
            {min: 0, max: 4, palette: durationPalette}, 'Flooding Event - Duration', false);

// Map all floods to generate the satellite-observed historical flood plain.
var gfdFloodedSum = gfd.select('flooded').sum();
Map.addLayer(gfdFloodedSum.selfMask(), {min: 0, max: 10, palette: durationPalette}, 'GFD Satellite Observed Flood Plain', false);

// SEPARATE RASTER PIXEL AREAS TO FLOODED/NON-FLOODED
// FLOOD Flood Database - Sylhet
var flood = flood_img.select('flooded').selfMask();
// NON-FLOOD Flood Database - Sylhet
var flood_Mask = flood_img.select('flooded').unmask(); // remove masks
var flood_Inverse = flood_Mask.not().clip(district).selfMask(); // get inverse
// Map.addLayer(flood_Inverse, {}, 'Flood Inverse') // check map

// FLOOD Model - Sylhet
var model = flooded_area.select('water').selfMask();
// NON-FLOOD Model - Sylhet
var model_Mask = flooded_area.select('water').unmask(); // remove masks
var model_Inverse = model_Mask.not().clip(district).selfMask(); // get inverse
// Map.addLayer(model_Inverse, {}, 'Model Inverse') // check map

//------------------------------- MODEL ACCURACY ------------------------------- //

// Get True Positive (TP)
var tp = flood.eq(1).and(model.eq(1)); // Intersection between flood & model
// Map.addLayer(tp, {palette: 'C3EFFE'}, 'TP'); // check map
var tp = tp.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: district, // boundary shape file
    scale: scale,
    maxPixels: 1e10
  }); // Get area (pixels)
var tp = ee.Number(tp.get('flooded'));

// Get True Negative (TN)
var tn = flood_Inverse.eq(1).and(model_Inverse.eq(1)); // Intersection between non flooded flood & non flooded model
// Map.addLayer(tn, {palette: 'C3EFFE'}, 'TN'); // check map
var tn = tn.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: district, // boundary shape file
    scale: scale,
    maxPixels: 1e10
  }); // Get area (pixels)
var tn = ee.Number(tn.get('flooded'));

// Get False Positive (FP)
var fp = flood_Inverse.eq(1).and(model.eq(1)); // Intersection between non flooded flood & flooded model
// Map.addLayer(fp, {palette: 'C3EFFE'}, 'FP'); // check map
var fp = fp.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: district, // boundary shape file
    scale: scale,
    maxPixels: 1e10
  }); // Get area (pixels)
var fp = ee.Number(fp.get('flooded'));

// Get Flase Negative (FN)
var fn = flood.eq(1).and(model_Inverse.eq(1)); // Intersection between flooded flood & non flooded model
// Map.addLayer(fn, {palette: 'C3EFFE'}, 'FN'); // check map
var fn = fn.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: district, // boundary shape file
    scale: scale,
    maxPixels: 1e10
  }); // Get area (pixels)
var fn = ee.Number(fn.get('flooded'));

// Accuracy = TP+TN / (TP+TN+FP+FN)
var accuracy = (tp.add(tn)).divide(tp.add(tn.add(fp.add(fn)))).multiply(100);
print('Method Accuracy (%)', accuracy);


//!!! GET PIXEL AREA AS KM2 !!!
  // var areaImage  = flooded_area.multiply(ee.Image.pixelArea().reproject({crs:'EPSG:4326',scale: 10})); //get pixel area
  // var area = areaImage.reduceRegion({
  //   reducer: ee.Reducer.sum(),
  //   geometry: district,
  //   scale: 10,
  //   maxPixels: 1e10
  //   });
  // var floodedAreaSqKm = ee.Number(area.get('water')).divide(1e6).round(); //get pixel area in km 2

  // print('Flood Area (km2)', floodedAreaSqKm);




// ##################
// ANALYSIS FUNCTIONS
// ##################

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

  return img.set({Minimum: mini, Maximum: maxi, Stdev: stdv, Mean: mean});
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

// by Guido Lemoine
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

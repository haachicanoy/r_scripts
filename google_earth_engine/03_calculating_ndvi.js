// Calculating NDVI on Google Earth Engine
// H. Achicanoy
// CIAT, 2017

// Location for bounds, in this case the city of El Paso. Use the inspector tool
// Create a variable using the Geometry function Point, lat and lon
var city = ee.Geometry.Point(-106.48207, 31.76247);

// Add the point to the map
Map.addLayer(city);

// Dates of interest
var start = ee.Date('2013-05-30');
var finish = ee.Date('2014-05-30');

// Create image collection
var ElPaso = ee.ImageCollection('LANDSAT/LC8_L1T')
.filterBounds(city)
.filterDate(start, finish)
.sort('CLOUD_COVER', false);

// Get the number of images
var count = ElPaso.size();
print('Size of collection El Paso', count);

// Sort by a cloud cover property, get the least cloudy image
var best = ee.Image(ElPaso.sort('CLOUD_COVER').first());
print('Least cloudy image: ', best);

var Color = {bands:['B3','B2','B1'], min:5000, max:15000, gamma:[0.95, 1.1, 1]};
Map.addLayer(best, Color, 'True Color');

// Get metadata
var date = best.get('DATE_ACQUIRED');
print('Date taken', date);

// Create Band variables
var B4_Red = best.select('B4');
var B5_NIR = best.select('B5');

// Calculation of NDVI using GEE Help Methods
var ndvi1 = B5_NIR.subtract(B4_Red).divide(B5_NIR.add(B4_Red));

// Calculation of NDVI Using Personal Method
var ndvi2 = best.expression(
    '(B5-B4)/(B5+B4)', {
        'B5' : B5_NIR, // Band
        'B4' : B4_Red
    }
);

// Used for display only - Obtained from GEE Help
var ndvi_palette =
    'FFFFFF, CE7E45, DF923D, F1B555, FCD163, 99B718, 74A901, 66A000, 529400,' +
    '3E8601, 207401, 056201, 004C00, 023B01, 012E01, 011D01, 011301';

// Map.addLayer(best);
Map.addLayer(ndvi1, {min: -0.1, max: 1.0, palette: ndvi_palette}, 'NDVI 1');
Map.addLayer(ndvi2, {min: -0.1, max: 1.0, palette: ndvi_palette}, 'NDVI 2');

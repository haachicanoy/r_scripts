// Displaying data on Google Earth Engine
// H. Achicanoy
// CIAT, 2017

// Loading the image using the image ID
var South_Texas = ee.Image('LANDSAT/LC8_L1T/LC80260412016037LGN00');

// Zoom to image
Map.centerObject(South_Texas, 10);

// Add the image to the map as a layer
var Color = {bands:['B3','B2','B1'], min:5000, max:15000, gamma:[0.95, 1.1, 1]};
// Map.addLayer(South_Texas);
Map.addLayer(South_Texas, Color, 'True Color');

// Script by Mike Koontz, Earth Lab
// Export Hotter Drought for integration into offline disturbance stack


var exportFolder = "GEE_Exports";

var drought1999 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_1999"),
    drought2000 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2000"),
    drought2001 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2001"),
    drought2002 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2002"),
    drought2003 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2003"),
    drought2004 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2004"),
    drought2005 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2005"),
    drought2006 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2006"),
    drought2007 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2007"),
    drought2008 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2008"),
    drought2009 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2009"),
    drought2010 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2010"),
    drought2011 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2011"),
    drought2012 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2012"),
    drought2013 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2013"),
    drought2014 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2014"),
    drought2015 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2015"),
    drought2016 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2016"),
    drought2017 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2017"),
    drought2018 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2018"),
    drought2019 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2019"),
    drought2020 = ee.Image("projects/cires-gg-earthlab/HotterDrought/HotterDrought_2020"),
    template = ee.Image("projects/cires-gg-earthlab/landfire-annual-disturbance/landfire-disturbance_western-conus_1999"),
    states = ee.FeatureCollection("TIGER/2018/States"),
    terraclim = ee.ImageCollection("IDAHO_EPSCOR/TERRACLIMATE"),
    drought_ic = ee.ImageCollection("projects/cires-gg-earthlab/HotterDrought/HotterDrought-western-conus");


print(template.projection());
print(terraclim.first().projection());

var export_proj = template.projection();
var export_region = template.geometry().buffer(10000);
print(export_region);
Map.addLayer(export_region);

// var western_conus =
// ee.Feature(
//   states
//     .filter(
//       ee.Filter.inList('NAME',
//         ['Washington', 'Oregon', 'California', 
//         'Idaho', 'Nevada', 'Montana', 'Wyoming', 
//         'Utah', 'Arizona', 'Colorado', 'New Mexico']))
//     .union()
//     .first()
//   )
// .geometry();

// print(western_conus);

// https://gis.stackexchange.com/questions/443078/export-an-image-in-gee-to-open-it-in-arcgis-pro
export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought1999,
    description: 'hammond-hotter-drought_1999',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2000,
    description: 'hammond-hotter-drought_2000',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2001,
    description: 'hammond-hotter-drought_2001',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2002,
    description: 'hammond-hotter-drought_2002',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2003,
    description: 'hammond-hotter-drought_2003',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2004,
    description: 'hammond-hotter-drought_2004',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2005,
    description: 'hammond-hotter-drought_2005',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2006,
    description: 'hammond-hotter-drought_2006',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2007,
    description: 'hammond-hotter-drought_2007',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2008,
    description: 'hammond-hotter-drought_2008',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2009,
    description: 'hammond-hotter-drought_2009',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2010,
    description: 'hammond-hotter-drought_2010',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2011,
    description: 'hammond-hotter-drought_2011',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2012,
    description: 'hammond-hotter-drought_2012',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2013,
    description: 'hammond-hotter-drought_2013',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2014,
    description: 'hammond-hotter-drought_2014',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2015,
    description: 'hammond-hotter-drought_2015',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2016,
    description: 'hammond-hotter-drought_2016',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2017,
    description: 'hammond-hotter-drought_2017',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2018,
    description: 'hammond-hotter-drought_2018',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2019,
    description: 'hammond-hotter-drought_2019',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

export_proj.evaluate(function(proj_obj) {
  Export.image.toDrive({
    image: drought2020,
    description: 'hammond-hotter-drought_2020',
    folder: exportFolder,
    region: export_region,
    crs: proj_obj.crs,
    crsTransform: proj_obj.transform,
    maxPixels: 5e9
  });
});

// for (i = 0; i < drought_ic.size().getInfo(); i++) {
  
// }
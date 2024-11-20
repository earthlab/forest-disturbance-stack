////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////// SETUP //////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// Climate Disturbance Stack Analysis: Hotter-hot fingerprint climate analysis
// Tyler McIntosh, CU Boulder Earth Lab, 1/19/2023

// This script takes in the TerraClimate dataset, and performs calculations on the full dataset to produce
// rasters of annual 'hotter-hot drought'. These calculations are the same as those made for point-locations in Hammond et al 2022: https://www.nature.com/articles/s41467-022-29289-2.

// 'neondomainsconus' shapefile table is the NEON 'Domain Polygons' from: https://www.neonscience.org/data-samples/data/spatial-data-maps, but clipped to only include CONUS.

// DIRECT LINK TO GEE SCRIPT: https://code.earthengine.google.com/?scriptPath=users%2Ftymc5571%2FCompoundDisturbance%3AClimate_DisturbanceStack

// OUTPUTS
// The script outputs are final image collections: ICcleanFAvg, ICcleanFMax, and ICcleanFMin.
// Each image within the image collection has a 'warmFingerprint' and 'coldFingerprint' band.
// These are the clean fingerprints showing only those regions where more variables passed their thresholds than 'numVarThreshold'.
// Avg, Min, and Max correspond to z-score thresholds set at the beginning of the script
// The global values identified in Hammond et al 2022 are:
// var warmDryTAvg = ee.List([0.37, 0.30, 0.49, -0.21, -0.39, -0.73]);
// var warmDryTMax = ee.List([0.41, 0.34, 0.53, -0.24, -0.42, -0.77]);
// var warmDryTMin = ee.List([0.33, 0.26, 0.45, -0.18, -0.36, -0.69]);

// EXPORTING
// Exports selected imagecollection interest using an adaptation of batch download from fitoprincipe repo: https://github.com/fitoprincipe/geetools-code-editor
// To auto-run all tasks directly in GEE console, can follow instructions at bottom of this blog:
// https://benny.istan.to/blog/20220319-batch-task-execution-in-google-earth-engine-code-editor
// which utilizes code from https://github.com/kongdd/gee_monkey


//////////// DATA

//Data imports copied from GEE
var terraclimate = ee.ImageCollection("IDAHO_EPSCOR/TERRACLIMATE"),
    conus = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-130.44414062500002, 49.685186433002514],
          [-130.44414062500002, 23.508285084424926],
          [-62.15312500000001, 23.508285084424926],
          [-62.15312500000001, 49.685186433002514]]], null, false),
    colorado = 
    /* color: #98ff00 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-109.06103589975199, 41.026450911300536],
          [-109.06103589975199, 37.27942746087441],
          [-101.94189527475199, 37.27942746087441],
          [-101.94189527475199, 41.026450911300536]]], null, false),
    west = 
    /* color: #0b4a8b */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-124.96923902475199, 48.90527784208563],
          [-124.96923902475199, 31.855302630433012],
          [-113.80712964975199, 31.855302630433012],
          [-113.80712964975199, 48.90527784208563]]], null, false),
    neondomainsconus = ee.FeatureCollection("users/tymc5571/NEON_Domains_CONUS");

////////////////////////////////// USER-SET PARAMETERS /////////////////////////////////////

//Google Drive folder for outputs
var foldername = 'GEE_Exports';

//Domain of interest. Here, relevant NEON domains
var domain = neondomainsconus.filter("DomainName == 'Northern Rockies' || DomainName == 'Great Basin' || \
DomainName == 'Pacific Northwest' || DomainName == 'Pacific Southwest' || DomainName == 'Desert Southwest' || \
DomainName == 'Southern Rockies / Colorado Plateau'");

print('Domain', domain);
Map.addLayer(domain);

//View terraclimate data structure
print('All TerraClimate', terraclimate);
Map.addLayer((terraclimate.select('tmmx')
  .filter(ee.Filter.calendarRange(1,1,'month'))
  .filter(ee.Filter.calendarRange(1958,1958,'year'))),
  {min: -100, max: 500, palette:['blue', 'red']}, 'raw_tmmx_1958_1', false);


//Select only variables of interest. Variables shown below, along with negative or positive direction indicating warm/dry
// TMAX = tmmx, VPD = vpd, CWD = def, SOIL M = soil, PPT = pr, PDSI = pdsi
// Positive variables: tmmx, vpd, def (need max month)
// Negative variables: soil, pr, pdsi (need min month)

//List of variable names of interest
var variable_names = ee.List(['tmmx', 'vpd', 'def', 'soil', 'pr', 'pdsi']);

print(variable_names);

//Dictionary showing variable warm/dry direction
var directions = ['positive', 'positive', 'positive', 'negative', 'negative', 'negative'];
var direction_dictionary = ee.Dictionary.fromLists(variable_names, directions);
print("Direction dictionary", direction_dictionary);

//Here we set variable thresholds to use for thresholding the data in the same order as our variable names list
//Our negative variables have opposite-direction z values
//(e.g. a -1.1 z-score is equivalent to a 1.1 z-score because the negative direction is warmer/drier)
var warmDryTAvg = ee.List([0.37, 0.30, 0.49, -0.21, -0.39, -0.73]);
var warmDryTMax = ee.List([0.41, 0.34, 0.53, -0.24, -0.42, -0.77]);
var warmDryTMin = ee.List([0.33, 0.26, 0.45, -0.18, -0.36, -0.69]);


//Use opposite thresholds for cold/wet
var flip = function(x) {return ee.Number(x).multiply(ee.Number(-1))};
var coldWetTAvg = warmDryTAvg.map(flip);
var coldWetTMax = warmDryTMax.map(flip);
var coldWetTMin = warmDryTMin.map(flip);

//In order to create a binary output, we need to set a number of variables that must surpass the thresholds above
var numVarThreshold = ee.Number(4);

//List of years of interest; will be mapped over. At time of writing, 2021 is most recent
var years = ee.List.sequence(1958, 2021);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// DROUGHT ANALYSIS - DO NOT CHANGE BELOW /////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////// SET UP FOR ANALYSIS ////////////////////

//List of months to map over
var months = ee.List.sequence(1, 12);

//Select only variables of interest
var vars = terraclimate.select(variable_names);

//Function to add the image month as a band called 'month'
function addmonthband(image) {
  var monthVal = image.date().get('month');
  return image.addBands(ee.Image.constant(monthVal).toInt().rename('month'));
}

vars = vars.map(addmonthband); //Add month band to all images


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////// CALCULATE MONTHLY AVERAGES OVER CLIMATE HISTORY, THEN GET MONTHS OF "TYPICALLY DRIEST/WARMEST AND WETTEST/COLDEST"////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////FUNCTIONS////////

//Function to pull monthly data over entire time frame, average, and reset image properties/band names
var getmonthmean = function(month) {
  var means = vars.filter(ee.Filter.calendarRange(month,month,'month')).mean(); // Calculate mean for the given month
  //Create a list of names with _mean appended, and 'month' as an additional name
  var meannamelist = variable_names.map(function(name) {
    return ee.String(name).cat('_mean');
  });
  meannamelist = meannamelist.add('mean_month');
  //Rename bands and set properties
  means = means.rename(meannamelist);
  means = means.set('month', ee.Number(month).toInt());
  means = means.set('monthclean', ee.Number(month).toInt().format('%02d'));
  return means;
};

//Function to create a min reducer that retains band names & values for all other bands, append 'min' to band names
function argminReduce(imageCollection) {
  var bandNames = imageCollection.first().bandNames(); //Store band names
  var output = imageCollection.reduce( //Min reducer
    ee.Reducer.min(bandNames.size())
    .setOutputs(bandNames)
  );
  output = output.rename(output.bandNames().map(function(bandName) { //Append min to names by mapping over all bands
    return ee.String(bandName).cat('_min');
  }));
  return output;
}

//Function to create a max reducer that retains band names & values for all other bands, append 'max' to band names
function argmaxReduce(imageCollection) {
  var bandNames = imageCollection.first().bandNames(); //Store names
  var output = imageCollection.reduce( //Max reducer
    ee.Reducer.max(bandNames.size())
      .setOutputs(bandNames)
  );
  output = output.rename(output.bandNames().map(function(bandName) { //Append max to names by mapping over all bands
    return ee.String(bandName).cat('_max');
  }));
  return output;
}

//Function to calculate both minimum and maximum means, as well as months of occurrence, for a given variable. Return an image with four bands, named appropriately
function minmaxmean(variable) {
  var varmeanname = ee.String(variable).cat("_mean");
  var varmeans = monthmeans.select(varmeanname, 'mean_month'); //Select variable of interest & month band
  
  //Rename 'mean_month' to have the variable name included (e.g. 'tmmx_mean_month)
  var varmeanname2 = ee.String(variable).cat("_mean_month");
  varmeans = varmeans.map(function(img) {
    return img.rename(varmeanname, varmeanname2);
  });
  
  var minmeans = argminReduce(varmeans); //calculate minimums
  var maxmeans = argmaxReduce(varmeans); //calculate maximums
  var minmax = minmeans.addBands(maxmeans); //combine mins and maxs into one image
  
  minmax = minmax.set('variable', variable); //Set property for image telling us what variable the data is for
  
  return minmax;
}

////////PERFORM TYPICALLY WARMEST/DRIEST & COLDEST/WETEST ANALYSIS//////////

//Create list of images, one for each month of the year, with 6 bands each containing the average of a given variable over the entire timeframe 

//Map monthmeans function over list of months, print output, test by visualizing a sample variable
var monthmeans = ee.ImageCollection.fromImages(months.map(getmonthmean));
print('Monthly means image collection:', monthmeans);
Map.addLayer(monthmeans.filter(ee.Filter.eq('month',1)).select('tmmx_mean'), {min: -100, max: 500, palette:['blue', 'red']}, 'temp', false);

//Map minmaxmean over all variables of interest in monthmeans
var allminmax = ee.ImageCollection.fromImages(variable_names.map(minmaxmean)); //Create an image collection from the list output
allminmax = allminmax.toBands(); // Turn image collection into a single image
allminmax = allminmax.rename(allminmax.bandNames().map(function(bandname) { //Remove 1_... from band names
  return ee.String(bandname).slice(2);
}));
print("Values and months of warmest/driest and coldest/wetest:", allminmax);

//Test layers for map
Map.addLayer(allminmax.select('tmmx_mean_min'), {min: -100, max: 500, palette:['blue', 'red']}, 'temp_min_mean', false);
Map.addLayer(allminmax.select('tmmx_mean_month_min'), {min: 1, max: 12, palette:['blue', 'red']}, 'temp_min_mean_month', false);
Map.addLayer(allminmax.select('tmmx_mean_max'), {min: -100, max: 500, palette:['blue', 'green']}, 'temp_max_mean', false);
Map.addLayer(allminmax.select('tmmx_mean_month_max'), {min: 1, max: 12, palette:['blue', 'red']}, 'temp_max_mean_month', false);


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////// CALCULATE CLIMATE ANOMALIES FOR THE TYPICALLY DRIEST/WARMEST MONTH IN EACH YEAR /////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//FUNCTION TO GET ANOMALIES RESULTING IN AN IMAGE COLLECTION WITH AN IMAGE FOR EACH VARIABLE, YEARS ARE BANDS //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
var getallanomaliesforvariable = function(variable) {
  
  //Set parameters related to the working variable
  variable = ee.String(variable);
  var varlen = variable.length(); //Length of variable name, for band names
  var dir = ee.String(direction_dictionary.get(variable)); //Get direction of variable from dictionary
  var varname = ee.String(ee.Algorithms.If(dir.equals('positive'), variable.cat("_mean_max"), variable.cat("_mean_min"))); //set name of min/max band from min/max analysis to use based on direction & variable
  var monthname = ee.String(ee.Algorithms.If(dir.equals('positive'), variable.cat("_mean_month_max"), variable.cat("_mean_month_min"))); //set name of min/max month band from min/max analysis to use based on direction & variable
  
  //Function to get anomaly for one year, one variable
  var getanomaly_oneyear = function(year) {
    
    //calculate anomaly for one month
    var getanomaly_onemonth = function(month) {
      var dats = ee.Image(vars.filter(ee.Filter.calendarRange(year, year, 'year'))
        .filter(ee.Filter.calendarRange(month, month, 'month'))
        .select(variable)
        .toList(1).get(0)); //Turn into single image by getting from list, note that index starts at 0
      var anom = allminmax.select(monthname);
      anom = anom.remap([month], [1]); //Map month of interest to "1" i.e. true. Default value left as null, so non-remapped pixels will be masked out
      //CALCULATE ANOMALY: (data from month for variable - average for month over time frame)*1 IF min/max is month in question
      anom = anom.multiply(dats.select(variable).subtract(allminmax.select(varname))); 
      return anom;
    };
    
    //Map anomaly calculator for one month over all months
    var monthanomseparate = months.map(getanomaly_onemonth);
    
    //Combine all outputs from mapping over list into a single blended image
    var yearanomcombined = ee.Image(monthanomseparate.get(0))
      .blend(ee.Image(monthanomseparate.get(1)))
      .blend(ee.Image(monthanomseparate.get(2)))
      .blend(ee.Image(monthanomseparate.get(3)))
      .blend(ee.Image(monthanomseparate.get(4)))
      .blend(ee.Image(monthanomseparate.get(5)))
      .blend(ee.Image(monthanomseparate.get(6)))
      .blend(ee.Image(monthanomseparate.get(7)))
      .blend(ee.Image(monthanomseparate.get(8)))
      .blend(ee.Image(monthanomseparate.get(9)))
      .blend(ee.Image(monthanomseparate.get(10)))
      .blend(ee.Image(monthanomseparate.get(11)));
// WAS TRYING TO NAME EACH BAND BY YEAR & VARIABLE; TURNS OUT FUTURE REDUCING NEEDS THEM NAMED CONSISTENTLy      
//    //Rename the image band to something that makes sense
    var yrstring = ee.Number(year).format('%04d'); //Zero-fill year to length of 4 numbers, ensure clean names
//    yearanomcombined = yearanomcombined.rename([variable.cat(yrstring)]);
    yearanomcombined = yearanomcombined.rename(variable);
    //try adding the year as a property
    yearanomcombined = yearanomcombined.set('year', yrstring);
    return(yearanomcombined);
  };
  
  //Map getanomaly_oneyear over entire image collection & clean up result
  var allyearanoms = years.map(getanomaly_oneyear); //Create image collection with anomalies for one variable for all years
  //var allyearanoms = ee.ImageCollection.fromImages(years.map(getanomaly_oneyear)); //Create image collection with anomalies for one variable for all years
//  allyearanoms = allyearanoms.toBands(); //Turn into a single image
  // allyearanoms = allyearanoms.rename(allyearanoms.bandNames().map(function(bandname) { //Clean up band names of the combined image
  //   return ee.String(bandname).slice(ee.Number(-4).subtract(varlen)); //system index numbers are auto-added to front after using toBands(). Slice from back using length of variable name. Note needs to be ee.Number since in mapped function
  // }));
 // allyearanoms = allyearanoms.set('variable', variable);
  return allyearanoms; //Return anomalies for all years for the working variable
};

/////////Map function to get anomalies over all variables included in the variable_names list
var allanomsraw = variable_names.map(getallanomaliesforvariable); //Create image collection from the list returned
//var allanomsraw = ee.ImageCollection.fromImages(variable_names.map(getallanomaliesforvariable)); //Create image collection from the list returned
print("Raw anomalies: ", allanomsraw);

var viz = ee.Image(ee.List(allanomsraw.get(0)).get(0)); //Have to cast as list item because doesn't know that the list item in the first list is again a list
print('One raster of raw anomalies (one year, one variable)', viz);
Map.addLayer(ee.Image(ee.List(allanomsraw.get(0)).get(0)), {min: -50, max: 50, palette:['blue', 'red']}, 'anom viz test yr 1', false);
Map.addLayer(ee.Image(ee.List(allanomsraw.get(0)).get(1)), {min: -50, max: 50, palette:['blue', 'red']}, 'anom viz test yr 2', false);





/////////////////////////////////////////////////////////////////////////
////////////////// STANDARDIZE ANOMALIES INTO Z-SCORES /////////////////
////////////////////////////////////////////////////////////////////////

//A function to standardize a list of images into Z-scores; returns a list of images
//Takes in any image, and standardizes all images pixel-wise.
//E.g. for 10 images of data (e.g. years), the data for a single location will be standardized
var standardize_list = function(list) {

  list = ee.List(list);
  var imageCollection = ee.ImageCollection.fromImages(list); //Convert to image collection to use reducers on next two lines
  var stack_sd = imageCollection.reduce(ee.Reducer.stdDev()); // stdev is consistent across space, is this right?... this seems strange; wouldn't some locations have more variability than others?
  var stack_mean = imageCollection.reduce(ee.Reducer.mean()); // mean = 0, consistent across space, is this right?...  this seems right based on how anomalies are defined

  //A function to do the z-score calculation on a single image now that pixel-wise sd & mean have been found
  var calculate_z_score = function(image) {
    image = ee.Image(image);
    var z_scored = image.subtract(stack_mean).divide(stack_sd); //actual calculation
    // get the year from original image and re-set property
    var yrprop = image.get('year');
    z_scored = z_scored.set('year', yrprop);
    return z_scored;
  };

  //Map function that calculates z-score for an image over the entire image collection
  var standardized = imageCollection.map(calculate_z_score);
  standardized = standardized.toList(years.length()); //cast as a list to return (list in, list out!)
  return standardized;
};

//For testing purposes
//var test = allanomsraw.get(0);
//print('Test', test);
// var stack_sd = test.reduce(ee.Reducer.stdDev());
// var stack_mean = test.reduce(ee.Reducer.mean()).rename('mean');
// print('Test sd', stack_sd, 'test mean', stack_mean);
// Map.addLayer(stack_sd, {min: -1, max: 1, palette:['blue', 'red']}, 'sd test', false);
//var standardtest = ee.ImageCollection.fromImages(standardize_list(test));
//print("Standardtest", standardtest);
//Map.addLayer(ee.Image(standardtest.toList(1).get(0)), {min: -3, max: 3, palette:['blue', 'red']}, 'test_standard_yr1', false);
//Map.addLayer(ee.Image(standardtest.toList(2).get(1)), {min: -3, max: 3, palette:['blue', 'red']}, 'test_standard_yr2', false);

//Map function over all anomalies
var standardizeAll = allanomsraw.map(standardize_list);
print('All standardized anomalies', standardizeAll);


var testSD = ee.ImageCollection.fromImages(allanomsraw.get(0)).reduce(ee.Reducer.stdDev());
Map.addLayer(testSD, {min: 5, max: 20, palette:['blue', 'red']}, 'tmmxSD', false);
print("Anomalies tmmx", ee.ImageCollection.fromImages(allanomsraw.get(0)));

//CHECKING SD & MEAN



//standardizeAll = ee.ImageCollection.fromImages(standardizeAll.flatten()); //flatten into a list of all the images, which have bands by variable & properties for year, then turn into single image collection

//////////////////////////////////////////////////////////////
////////////// THRESHOLD STANDARDIZED ANOMALIES //////////////
//////////////////////////////////////////////////////////////

//Zip up thresholds
var avgThresholds = warmDryTAvg.zip(coldWetTAvg);
var minThresholds = warmDryTMin.zip(coldWetTMin);
var maxThresholds = warmDryTMax.zip(coldWetTMax);

//Function to threshold z scores, where 'dats' is a zipped list containing:
//0 index - list of images containing z scores
//1 index - zipped list with 0 index warm thresholds, 1 index cold thresholds
var threshold_z_scores = function(dats) {
  
  //Extract data from zipped list contents & cast
  dats = ee.List(dats);
  var zAnoms = ee.List(dats.get(0));
  var thresholds = ee.List(dats.get(1));
  var warmT = ee.Number(thresholds.get(0));
  var coldT = ee.Number(thresholds.get(1));
  
  //Function to threshold a single image
  var threshold_image = function(img) {
    img = ee.Image(img);
    var imYr = img.get('year');

    //Create warm mask
    var warmMask = ee.Algorithms.If(warmT.lt(ee.Number(0)),
      img.lte(warmT), //when warmT < 0 (i.e. negative variable)
      img.gte(warmT)); //when warmT > 0 (i.e. postive variable)
    warmMask = ee.Image(warmMask).set('year', imYr);
    
    //Create cold mask
    var coldMask = ee.Algorithms.If(coldT.lt(ee.Number(0)),
      img.lte(coldT), //when coldT < 0 (i.e. positive variable)
      img.gte(coldT)); //when coldT > 0 (i.e. negative variable)
    
    //Remap cold masks to be filled with -1s instead of 1s. Return a list containing the warm & cold data
    var cold = ee.Image(coldMask).remap([ee.Number(1), ee.Number(0)], [ee.Number(-1), ee.Number(0)]);
    cold = cold.set('year', imYr);

    //var results = ee.Image(warmMask).add(cold);
    //return(results);
    return(ee.List([warmMask, cold]));
  };
  
  //Map threshold_image over all images & return output
  var thresholdImages = zAnoms.map(threshold_image);
  return(thresholdImages);
};

//Run for each set of thresholds
var avgThresholdImages = (standardizeAll.zip(avgThresholds)).map(threshold_z_scores);
var minThresholdImages = (standardizeAll.zip(minThresholds)).map(threshold_z_scores);
var maxThresholdImages = (standardizeAll.zip(maxThresholds)).map(threshold_z_scores);

print('Avg Threshold Images', avgThresholdImages);
//Map.addLayer(ee.Image(ee.List(standardizeAll.get(0)).get(0)), {min: -4, max: 4, palette:['blue', 'pink']}, 'tmmxStandard', true);
//Map.addLayer(ee.Image(ee.List(ee.List(avgThresholdImages.get(0)).get(0)).get(0)), {min: -1, max: 1, palette:['blue', 'red']}, 'tmmxThresh', true);
//Map.addLayer(ee.Image(ee.List(maxThresholdImages.get(0)).get(0)), {min: -1, max: 1, palette:['blue', 'red']}, 'tmmxThreshMax', true);
//Map.addLayer(ee.Image(ee.List(minThresholdImages.get(0)).get(0)), {min: -1, max: 1, palette:['blue', 'red']}, 'tmmxThreshMin', true);

//Function to zip a list containing lists with one element per year
var zip_list = function(masterList) {
  //Set up indices to map over
  var numYears = ee.List(years).length();
  var indices = ee.List.sequence(0, numYears.subtract(1));
  
  //Function that takes in an index and zips whatever list of lists is set to 'zippingList'
  var zip_index = function(index) {
    masterList = ee.List(masterList);
    var collected = masterList.map(function(list){ //Create a list of everything held at the current index in the lists contained in 'zippingList'
      return ee.List(list).get(index);
    });
    return(collected); //Return the list
  };
  //Zip all six variable lists together such that each year is zipped together
  var zippedList = indices.map(zip_index);
  return(zippedList);
};

//Run zip up all of the threshold lists
var zippedAvgThresholdImages = zip_list(avgThresholdImages);
var zippedMinThresholdImages = zip_list(minThresholdImages);
var zippedMaxThresholdImages = zip_list(maxThresholdImages);

print('Zipped Avg Thresh Images', zippedAvgThresholdImages);

//For each year, produce a single image that includes both warm & cold fingerprints.
//Function takes in a zipped list that has both warm & cold data from each variable for each year.
//Returns a list of images, one per year
var make_fingerprint = function(yearData) {
  
  yearData = ee.List(yearData);
  
  //Extract all data for the year
  var var1warm = ee.Image(ee.List(yearData.get(0)).get(0));
  var var2warm = ee.Image(ee.List(yearData.get(1)).get(0));
  var var3warm = ee.Image(ee.List(yearData.get(2)).get(0));
  var var4warm = ee.Image(ee.List(yearData.get(3)).get(0));
  var var5warm = ee.Image(ee.List(yearData.get(4)).get(0));
  var var6warm = ee.Image(ee.List(yearData.get(5)).get(0));
  var var1cold = ee.Image(ee.List(yearData.get(0)).get(1));
  var var2cold = ee.Image(ee.List(yearData.get(1)).get(1));
  var var3cold = ee.Image(ee.List(yearData.get(2)).get(1));
  var var4cold = ee.Image(ee.List(yearData.get(3)).get(1));
  var var5cold = ee.Image(ee.List(yearData.get(4)).get(1));
  var var6cold = ee.Image(ee.List(yearData.get(5)).get(1));
  
  var fYr = var1warm.get('year');
  
  //Create warm fingerprint
  var warmF = ee.Image(var1warm.add(var2warm).add(var3warm).add(var4warm).add(var5warm).add(var6warm));
  warmF = warmF.rename('warmFingerprint');
  
  //Create cold fingerprint
  var coldF = ee.Image(var1cold.add(var2cold).add(var3cold).add(var4cold).add(var5cold).add(var6cold));
  coldF = coldF.rename('coldFingerprint');

  //Combine & set year, return fingerprint
  var fingerprint = warmF.addBands(coldF);
  fingerprint = fingerprint.set('year', fYr);
  return(fingerprint);
  
};

//Make fingerprint for all sets of data
var fingerprintAvg = zippedAvgThresholdImages.map(make_fingerprint);
var fingerprintMin = zippedMinThresholdImages.map(make_fingerprint);
var fingerprintMax = zippedMaxThresholdImages.map(make_fingerprint);

print('A finished fingerprint', fingerprintAvg);

Map.addLayer(ee.Image(fingerprintMax.get(62)).select("warmFingerprint"), {min: 0, max: 6, palette:['blue', 'red']}, 'warmFingerprintMaxT', false);


//Take the image bands that have 0 - 6 / 0 - -6 in them, and feed through filter to remove unwanted data
var cleanFingerprint = function(yearImg) {
  yearImg = ee.Image(yearImg);
  var warmMask = yearImg.select("warmFingerprint").gte(numVarThreshold);
  var coldMask = yearImg.select("coldFingerprint").lte(numVarThreshold.multiply(ee.Number(-1)));
  var cleanWarm = yearImg.select("warmFingerprint").updateMask(warmMask);
  var cleanCold = yearImg.select("coldFingerprint").updateMask(coldMask);
  return(cleanWarm.addBands(cleanCold));
};

//Create 'clean' fingerprints
var cleanFAvg = fingerprintAvg.map(cleanFingerprint);
var cleanFMax = fingerprintMax.map(cleanFingerprint);
var cleanFMin = fingerprintMin.map(cleanFingerprint);
//Print one of the fingerprints for one year!
Map.addLayer(ee.Image(cleanFAvg.get(62)).select("warmFingerprint"), {min: 0, max: 6, palette:['blue', 'red']}, 'warmFingerprintClean2020', false);

// Turn lists of images into imagecollections
var ICcleanFAvg = ee.ImageCollection.fromImages(cleanFAvg);
var ICcleanFMax = ee.ImageCollection.fromImages(cleanFMax);
var ICcleanFMin = ee.ImageCollection.fromImages(cleanFMin);


//Quick test comparison to TerraClimate raw PDSI values
var yr = ee.Number(1990);
var summerPDSI = terraclimate.select('pdsi').
  filter(ee.Filter.calendarRange(7,8,'month')).
  filter(ee.Filter.calendarRange(yr,yr,'year')).
  mean();
var summerPDSImask = summerPDSI.lte(-400);
summerPDSI = summerPDSI.updateMask(summerPDSImask);
Map.addLayer(ee.Image(summerPDSI), {min: -500, max: 0, palette:['orange', 'blue']}, 'PDSI summer 1990', false);


/////////////////////////////////////////////////////////
///////////// EXPORT AS ASSET FOR USE ///////////////////
/////////////////////////////////////////////////////////

// Retain only warm fingerprint, clip to domain
var cleanAvgWarm = ICcleanFAvg.select('warmFingerprint').filterBounds(domain);
print("To Export:", cleanAvgWarm);
Map.addLayer(ee.Image(cleanAvgWarm.first()).clip(domain), {min: 0, max: 6, palette:['blue', 'red']}, 'testClip', false);

// We cannot use 'map' to export images, as Export is a client side function. Instead we have to use a loop (yuck)
// Here we use an adapted version of  batch download from fitoprincipe repo: https://github.com/fitoprincipe/geetools-code-editor

// CODE INDENTED BELOW TAKEN FROM FITOPRINCIPE REPO & ADAPTED AS NOTED IN CODE

            var tools = require('users/fitoprincipe/geetools:tools');
            var helpers = require('users/fitoprincipe/geetools:helpers_js');
            
            var getRegion = function(object, bounds) {
              bounds = bounds || false;
              try {
                var name = object.name();
                if (name in ['Image', 'Feature', 'ImageCollection', 'FeatureCollection']) {
                  var geom = object.geometry();
                } else {
                  var geom = object;
                }
                if (bounds) {
                  geom = geom.bounds();
                }
                return geom;
              } catch(err) {
                print(err.message);
                return object;
              }
            };
            
            exports.getRegion = getRegion;
            
            
            
            // TASK CLASS
            var Task = function(taskId, config) {
              this.id = taskId;
              this.config = config;
            };
            
            Task.prototype.start = function() {
                ee.data.startProcessing(this.id, this.config);
            };
            
            var IMAGE_TYPES = function(img, type) {
             var types = {  "float":img.toFloat(), 
                            "byte":img.toByte(), 
                            "int":img.toInt(),
                            "double":img.toDouble(),
                            "long": img.toLong(),
                            "short": img.toShort(),
                            "int8": img.toInt8(),
                            "int16": img.toInt16(),
                            "int32": img.toInt32(),
                            "int64": img.toInt64(),
                            "uint8": img.toUint8(),
                            "uint16": img.toUint16(),
                            "uint32": img.toUint32()};
              
              return types[type];
            };
            
            var Download = {'ImageCollection': {}, 'Table': {}, 'Image':{}};
            
            Download.ImageCollection.toAsset = function(collection, assetFolder, options) {
              var root = ee.data.getAssetRoots()[0]['id'];
              var folder = assetFolder;
              if (folder !== null && folder !== undefined) {
                var assetFolder = root+'/'+folder+'/';
              } else {
                var assetFolder = root+'/';
              }
              
              var defaults = {
                  name: null,
                  scale: 1000,
                  maxPixels: 1e13,
                  region: null
                };
                
              var opt = tools.get_options(defaults, options);
              var n = collection.size().getInfo();
                
              var colList = collection.toList(n);
              
              for (var i = 0; i < n; i++) {
                var img = ee.Image(colList.get(i));
                var id = img.id().getInfo() || 'image_'+i.toString();
            
                var yr = img.get('year');                                       ////////// CHANGED
                var nm = ee.String('HotterDrought_').cat(yr).getInfo();        ////////// CHANGED
                var region = opt.region || img.geometry().bounds().getInfo()["coordinates"];
                var assetId = assetFolder+nm;                                  ////////// CHANGED
                
                Export.image.toAsset({
                  image: img,
                  description: nm,                                             ////////// CHANGED
                  assetId: assetId,
                  region: region,
                  scale: opt.scale,
                  maxPixels: opt.maxPixels});
              }
            };


// Run batch export -- it takes a while to run since it's client-side, so be patient in waiting for tasks to appear
// var batch = require('users/fitoprincipe/geetools:batch');
var asset = 'HotterDrought';
var options = {
  name: 'HotterDrought_{year}',
  scale: 4638.3,
  region: domain
};
// batch.Download.ImageCollection.toAsset(cleanAvgWarm, asset, options); <- adapted batch script above to allow for correct naming scheme. Instead run on line below
Download.ImageCollection.toAsset(cleanAvgWarm, asset, options);




// // THIS WORKS for batch-exporting calculated monthmeans
// //var batch = require('users/fitoprincipe/geetools:batch');
// var asset = 'testMonthMeans';
// var options = {
//   name: 'MonthMeans_{monthclean}',
//   scale: 4638.3,
//   region: domain
// };
// //batch.Download.ImageCollection.toAsset(monthmeans, asset, options);
// Download.ImageCollection.toAsset(monthmeans, asset, options);




///////////////////////////////////////////////////////////////////
/////////////////////// VISUALIZE OUTPUT //////////////////////////
///////////////////////////////////////////////////////////////////

// var warmViz = ee.ImageCollection(binaryFMax.select('warmFingerprint'));

// //Args for video
// var videoArgs = {
//   dimensions: 768,
// //  region: colorado,
//   region: west,
//   framesPerSecond: 3,
//   min: 0,
//   max: 1,
//   palette: ['blue', 'red']
// };

// //Args for filmstrip
// var filmArgs = {
//   dimensions: 128,
//   region: conus,
//   min: 0,
//   max: 1,
//   palette: ['blue', 'red']
// };

// print(warmViz.getVideoThumbURL(videoArgs));
// print(warmViz.getFilmstripThumbURL(filmArgs));



// //////////////////////////////////////////////////////////////////////////////////////
// ///////////// PULL ALL VALUES FOR A FEW TEST POINTS TO CALCULATE ANOMALY BY HAND /////
// /////////////& COMPARE TO OUTPUT CALCULATION FROM R //////////////////////////////////
// //////////////////////////////////////////////////////////////////////////////////////

//Create feature collection from list of points
var pts = ee.FeatureCollection([
  ee.Feature(ee.Geometry.Point([-105.24249040058604,40.00981039217258]), {plot_id: "BoulderCO"}), //Boulder, CO, SEEC
  ee.Feature(ee.Geometry.Point([-110.786763, 43.432451]), {plot_id: "JacksonWY"}), //Jackson WY, Rafter J
  ee.Feature(ee.Geometry.Point([-122.486757, 48.733972]), {plot_id: "BellinghamWA"}), //Bellingham, WA, WWU
  ee.Feature(ee.Geometry.Point([-122.170020, 37.428193]), {plot_id: "StanfordCA"}), //Stanford, CA
]);

//Function to buffer points
function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}
//Buffer points by 15m radius to create visible areas
var buffPts = pts.map(bufferPoints(15, true));
//print("Points:", buffPts);

//Display data & points to ensure correct. Check a few values by hand
var testyr = 7;
//print('Anoms to take from', allanomsraw.get(0));
//Map.addLayer(ee.Image(ee.List(allanomsraw.get(5)).get(testyr - 1)), {min:0,max:500, palette:['blue', 'red']}, 'raw', false);
//Map.addLayer(ee.Image(ee.List(standardizeAll.get(5)).get(testyr - 1)), {min:0,max:500, palette:['blue', 'red']}, 'z-score', false);
//Map.addLayer(buffPts, {}, 'pts', true);

//print('Year is ', ee.Image(ee.List(allanomsraw.get(5)).get(testyr - 1)).get('year'));
//print('Band is ', ee.Image(ee.List(allanomsraw.get(5)).get(testyr - 1)).bandNames());


//Empty collection to fill
var ft = ee.FeatureCollection(ee.List([]));

//Function to fill feature collection with data from image collection
var fill = function(img, ini) {
  // type cast
  var inift = ee.FeatureCollection(ini);
  // gets the values for the points in the current img
  var ft2 = img.reduceRegions(pts, ee.Reducer.first(),30);
  // gets the year and variable of the img
  var yr = img.get('year');
  var v = (img.bandNames()).get(0);
  // writes the date in each feature
  var ft3 = ft2.map(function(f){return f.set("year", yr)});
  ft3 = ft3.map(function(f){return f.set("variable", v)});
  // merges the FeatureCollections
  return inift.merge(ft3);
};

//Function to use on list
var exportData = function(list) {
  var ic = ee.ImageCollection.fromImages(list);
  var thisft = ee.FeatureCollection(ic.iterate(fill,ft));
  return(thisft);
};

//Export all raw data for pts
var allRawExport = exportData(allanomsraw.get(0)).
  merge(exportData(allanomsraw.get(1))).
  merge(exportData(allanomsraw.get(2))).
  merge(exportData(allanomsraw.get(3))).
  merge(exportData(allanomsraw.get(4))).
  merge(exportData(allanomsraw.get(5)));
print('allRawExport', allRawExport);

//Export all z data for pts
var allZExport = exportData(standardizeAll.get(0)).
  merge(exportData(standardizeAll.get(1))).
  merge(exportData(standardizeAll.get(2))).
  merge(exportData(standardizeAll.get(3))).
  merge(exportData(standardizeAll.get(4))).
  merge(exportData(standardizeAll.get(5)));
print('allZExport', allZExport);

// //Export results
// Export.table.toDrive({
//   collection: allRawExport,
//   folder: foldername,
//   description: 'GEE_locations_calculated_raw_anomalies',
//   fileFormat: 'CSV'
// });
// Export.table.toDrive({
//   collection: allZExport,
//   folder: foldername,
//   description: 'GEE_locations_calculated_z_anomalies',
//   fileFormat: 'CSV'
// });





// -------------------------------------------
// 1. Load your lat/lon CSV into GEE
// -------------------------------------------
// Upload your CSV to GEE Assets as a Table
// Replace asset ID below with yours
var islands = ee.FeatureCollection('users/yourusername/ime_crep_lat_lon')
  .map(function(f) {
    return f.setGeometry(ee.Geometry.Point([f.get('lon'), f.get('lat')]));
  });

// -------------------------------------------
// 2. Load CHIRPS daily and convert to monthly totals
// -------------------------------------------
var chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY')
  .filterDate('1991-01-01', '2020-12-31');

// Function to sum a month for each year, then take the average over years
function monthMean(m) {
  m = ee.Number(m);
  var monthly = chirps.filter(ee.Filter.calendarRange(m, m, 'month'))
    .map(function(img) {
      return img.set('year', img.date().get('year'));
    })
    .aggregate_array('year'); // for debugging

  var perYear = chirps.filter(ee.Filter.calendarRange(m, m, 'month'))
    .reduce(ee.Reducer.sum()); // sum over all years
  
  var climatology = chirps.filter(ee.Filter.calendarRange(m, m, 'month'))
    .map(function(img) {
      return img.set('year', img.date().get('year'));
    })
    .reduce(ee.Reducer.mean()); // mean across all days
  
  // Simpler method: sum daily, then divide by number of years
  var monthSumAllYears = chirps.filter(ee.Filter.calendarRange(m, m, 'month'))
    .sum();
  var meanMonth = monthSumAllYears.divide(30); // 30 years in 1991–2020
  
  return meanMonth.set('month', m);
}

// List of months and abbreviations
var monthNums = ee.List.sequence(1, 12);
var monthAbbr = ee.List(['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']);

// Get monthly climatology images
var monthlyImages = monthNums.map(monthMean);

// -------------------------------------------
// 3. Sample at island points and combine into wide format
// -------------------------------------------
var results = islands.map(function(f) {
  var props = ee.Dictionary();
  
  // Add each month as a property
  monthNums.zip(monthAbbr).iterate(function(mPair, acc) {
    mPair = ee.List(mPair);
    var mNum = mPair.get(0);
    var mName = ee.String(mPair.get(1));
    
    var monthImg = ee.Image(monthlyImages.get(ee.Number(mNum).subtract(1)));
    var val = monthImg.reduceRegion({
      geometry: f.geometry(),
      reducer: ee.Reducer.mean(),
      scale: 5000
    }).values().get(0);
    
    acc = ee.Dictionary(acc).set(mName, val);
    return acc;
  }, props);
  
  // Compute annual mean (mean of monthly means)
  var monthlyVals = monthAbbr.map(function(m) {
    return ee.Number(ee.Dictionary(props).get(ee.String(m)));
  });
  var annualMean = ee.Array(monthlyVals).reduce('mean', [0]).get([0]);
  
  return f.set(props).set('AnnualMean', annualMean);
});

// -------------------------------------------
// 4. Export as CSV
// -------------------------------------------
Export.table.toDrive({
  collection: results,
  description: 'CHIRPS_1991_2020_MonthlyClimatology_Wide',
  fileFormat: 'CSV'
});

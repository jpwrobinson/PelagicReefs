// ============================
// 1. LOAD & CLEAN ISLAND LIST
// ============================
// This must be uploaded with lat/lon defined so each row is a point geometry
var islands = ee.FeatureCollection('projects/strava-394014/assets/ime_crep_lat_lon');

// ================= SETTINGS =================
var startYear = 1991;
var endYear = 2020;

var chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY')
  .filterDate(startYear + '-01-01', endYear + '-12-31');

var monthNames = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

// ================= FUNCTION =================
function monthClimoTotal(month) {
  month = ee.Number(month);

  // Compute total per year for this month
  var perYearTotals = ee.List.sequence(startYear, endYear).map(function(y) {
    y = ee.Number(y);
    var start = ee.Date.fromYMD(y, month, 1);
    var end = start.advance(1, 'month');
    return chirps.filterDate(start, end).sum()
                 .set('system:time_start', start.millis());
  });

  // Make sure we end up with ONE image (mean across years)
  var meanImage = ee.ImageCollection.fromImages(perYearTotals).mean();

  // Rename band to month name
  return meanImage.rename(monthNames[month.subtract(1).int()]);
}

// ================= BUILD IMAGE =================
var monthlyImgs = ee.List.sequence(1, 12).map(function(m) {
  return monthClimoTotal(m);
});

// Convert to ImageCollection → multiband image
var climatology = ee.ImageCollection.fromImages(monthlyImgs).toBands()
                  .rename(monthNames);

// Annual mean image
var annualImg = climatology.reduce(ee.Reducer.mean()).rename('AnnualMean');

// ================= SAMPLE AT GEOMETRIES =================
var results = islands.map(function(f) {
  var vals = climatology.reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: f.geometry(),
    scale: 5566,
    maxPixels: 1e9
  });

  var annualVal = annualImg.reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: f.geometry(),
    scale: 5566,
    maxPixels: 1e9
  }).get('AnnualMean');

  return f.set(vals).set('AnnualMean', annualVal);
});

print('Results preview', results.limit(5));

// ================= EXPORT =================
Export.table.toDrive({
  collection: results,
  description: 'CHIRPS_monthly_climatology_1991_2020',
  fileFormat: 'CSV'
});

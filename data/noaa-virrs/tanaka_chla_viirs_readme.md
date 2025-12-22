1. Yes. “ESA” here refers to the ESA Ocean Colour Climate Change Initiative (OC-CCI). I believe ESA is a multi-sensor, harmonized ocean-colour product (SeaWiFS, MODIS-Aqua, MERIS, VIIRS, etc.) bias-corrected to provide temporal consistency. And VIIRS is the NASA–NOAA SNPP/JPSS VIIRS single-sensor product. I will double check on these later. Both are global. I've been told ESA is explicitly for long-term consistency and VIIRS for contemporary monitoring. To my understanidng, ESA minimizes inter-sensor discontinuities but smooths some sensor-specific features, and VIIRS can show sharper spatial/temporal structure but is more sensitive to sensor-specific calibration and algorithm assumptions. I think ESA assumes cross-sensor harmonization is preferable for climate baselines and VIIRS assumes internal sensor stability over a shorter window. Using both allows long climatological context (ESA) plus recent, sensor-pure variability (VIIRS).

2. Correct. ESA OC-CCI spans 1997–present. VIIRS spans 2012– present in these outputs (sorry I initially said 2002)

3. I do have island-scale masked fields. I can export a small set of representative netCDF snapshots (or PNGs) showing the 30 m coastal mask applied to ESA and VIIRS means for visualization. Just let me know which islands you want for these visualizations.

4. Each variable encodes: sensor × statistic × temporal window, evaluated relative to the survey date.
   01dy, 01wk, 01mo: rolling windows (1 day, 1 week, 1 month) immediately preceding the survey date.
   03yr, 05yr: rolling multi-year windows ending at the survey date (e.g., mean chlorophyll over the previous 3 or 5 years).
   10yr_1_yr: a long-term baseline (10 years) excluding the most recent year, intended to contrast “background” vs near-term conditions.
   all_before: all available data prior to the survey date, effectively a site-specific climatological reference.

5. Yes, all outputs are from the 30 m coastal mask. The underlying bathymetry layer used to define this mask is the ETOPO Global Relief Model (15 arc-second resolution). Bathymetry is used only to generate the ≤30 m mask and does not otherwise enter the chlorophyll calculations.
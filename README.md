# FAARM_floodingDiet_overTime
Data collection, cleaning and modelling tasks for project exploring the relationship between flooding & dietary diversity over time using FAARM trial data.

## 1_GEE_JS : JavaScript code to be run in the Google Earth Engine (GEE) code editor. Extracts flooding as a series over time.
 - __G1_compReference.js__:
 - __G2_crossValidationMethod.js__:
 - __G3_Environ.js__:
 - __G3_clusterBound_10mExtraction_AreaKm2.js__:

## 2_Python Folder: Python code to pre-processes and combines exogenous data extracted from GEE with FAARM trial data.
 - __Collators.py__:
 - __P1_GEE_Compilation.py__:
 - __P2_FAARM_Compilation.py__:
 - __P3_Flood_Metrics.py__:

## 3_R Folder: R code for processing, analysing, and modelling the relationship between flooding and dietary diversity over time. 
 - __R0_DataFormatting.R__:
 - __R1_RSMetric_Validation.R__:
 - __R2_STDA_DietFlooding.R__:
 - __R3_RINLA_SpatioTemporal_Fitting.R__:
 - __R4_Results.R__:
 - __RINLA_FloodDiet_Modelling.Rproj__:

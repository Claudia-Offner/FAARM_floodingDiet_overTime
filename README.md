# FAARM_floodingDiet_overTime
Data collection, cleaning and modelling tasks for project exploring the relationship between flooding & dietary diversity over time using FAARM trial data.

## 1_GEE_JS
JavaScript code to be run in the Google Earth Engine (GEE) code editor. Extracts flooding as a series over time.
 - __G1_compReference__: Creates a composite reference image used to difference with time series using driest known period.
 - __G2_crossValidationMethod__: Cross validate and tune parameters prior to data extraction using Global Flood Database events (3).
 - __G3_Environ__: Extract environmental variables relevant to outcomes and exposures (i.e. NDVI, precipitaiton, temperature, evapotranspiraiton).
 - __G3_clusterBound_10mExtraction_AreaKm2__: Extract cluster specific flooding time series for ROI at 10m resolution.

## 2_Python 
Python code to pre-processes and combines exogenous data extracted from GEE with FAARM trial data.
 - __Collators__: OOP Classes used for data processing.
 - __P1_GEE_Compilation__: Pre-process and merge flood time series with other environmental variables extracted from GEE.
 - __P2_FAARM_Compilation__: Pre-process and merge GEE variables with FAARM trial data. 
 - __P3_Flood_Metrics__: Check descrptive statistics of flood time series and create different flood metrics to test for analysis phase.

## 3_R Folder
R code for processing, analysing, and modelling the relationship between flooding and dietary diversity over time. 
 - __R0_DataFormatting__: Load and format data for analysis in R. NOTE: This file must be run before running any other R files. 
 - __R1_RSMetric_Validation__: Test the strength associations between flood time series metrics & flood experience in April 2017 (Still in process)
 - __R2_STDA_DietFlooding__: Spatial-Temporal Data Analysis (STDA) of outcome and exposure variables to determine the lags required and levels of autocorrelation to control for. Includes confounder selection process and non-spatial EDA.
 - __R3_RINLA_SpatioTemporal_Fitting__: Building optimal RINLA model parameters by testing integrated spatial & temporal random effects. 
 - __R4_Results__: Optimized model results testing for interactions (2-way and 3-way) and quadratic effects.
 - __RINLA_FloodDiet_Modelling__: R projct file.

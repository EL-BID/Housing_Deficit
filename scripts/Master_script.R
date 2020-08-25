
### Housing Deficit Estimation: MASTER SCRIPT

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

## This script will run the *entire analysis* - data prep, indicator calculation, luminosity data extraction,
## and deficit prediction/now-casting. To run, update the working directory in line 14, highlight all the lines 
## below and click Run. This will run all 4 scripts for the analysis and save the data in the corresponding 
## folders. To see specific visualizations you may need to enter the specific script and reproduce manually.



working <- "\\Users\\user\\Documents"    

OS <- .Platform$OS.type
if (OS == "unix"){
  working <- str_replace_all(working, '\\\\', '/\\')
  setwd(working)
  source('Scripts/script1_Data_prep.R')
  source('Scripts/script2_Indicators.R')
  source('Scripts/script3_Stat_test.R')
  source('Scripts/script4_Luminosity.R')
  source('Scripts/script5_Prediction.R')
} else {
  setwd(working)
  source('Scripts\\script1_Data_prep.R')
  source('Scripts\\script2_Indicators.R')
  source('Scripts\\script3_Stat_test.R')
  source('Scripts\\script4_Luminosity.R')
  source('Scripts\\script5_Prediction.R')
}





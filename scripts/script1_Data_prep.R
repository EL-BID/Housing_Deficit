### Housing Deficit Estimation: data prep

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

# This script provides code that will help standardize census data to produce a clean dataset 
# for housing deficit estimation calculations. However, data cleaning is highly specific to each 
# dataset, and each new dataset must be reviewed and verified based on its own particularities. 
# No matter what, the dataset should contain information that allows the user to identify where 
# households are located, or geographic identifiers: This desaggregation will have to match
# available GIS shapefiles. 



# ---------- load data ---------- # 

## if .csv: 
census_path <- "Data\\Census.csv"
if (OS == "unix"){    
  census_path <- str_replace_all(census_path, '\\\\', '/\\')    # file path adjustment for Macs
}
Census <- read.csv(census_path)

## if .dta (STATA)
if (!require("readstata13")) install.packages("readstata13")
library(readstata13)

census_path <- "Data\\Census.dta"
if (OS == "unix"){    
  census_path <- str_replace_all(census_path, '\\\\', '/\\')    # file path adjustment for Macs
}
Census <- read.dta13(census_path)

## if .sav (SPSS)
if (!require("foreign")) install.packages("foreign")
library(foreign)
census_path <- "Data\\Census.sav"
if (OS == "unix"){    
  census_path <- str_replace_all(census_path, '\\\\', '/\\')    # file path adjustment for Macs
}
Census <- read.spss(census_path, to.data.frame=TRUE)

# loading large datasets can take time in R. Making a copy inside R saves you that time if you need to start fresh in your data cleaning/processing 
Census_copy <- Census     # this step makes a copy
#Census <- Census_copy     # this step replaces the original with the copy in case you make an irrecoverable mistake in the original



# ---------- Verify and standardize the Census data ---------- # 

attach(Census)    # keep census in R's immediate memory cache for ease of coding

## standardize administrative division labels
Census$province <- paste(Census$adm1,Census$adm2, sep="")
Census$dist <- paste(Census$adm1,Census$adm2,Census$adm3, sep="")
length(unique(Census$province))   # verify correct number provinces
length(unique(Census$dist))    # verify correct number of districts

## ensure administrative division number is in the correct format (such as 'ubigeo' in Peru) for consistency with shapefiles
Census$province <- as.integer(as.character(Census$province))
Census$province<-sprintf("%04d", Census$province)   # this ensures that leading zeros are preserved

## if administrative division labels are names, recode them as necessary for consistency with shapefiles
if (!require("dplyr")) install.packages("dplyr")
library("dplyr")
table(Census$Adn2_name)
Census$Adn2_name <- dplyr::recode(Census$Adn2_name, incorrectname1 = "Correct_Name_1", 
                                  incorrectname2 = "Correct_Name_2", 
                                  incorrectname3 = "Correct_Name_3", 
                                  incorrectname4 = "Correct_Joined_Name_4", 
                                  incorrectname5 = "Correct_Joined_Name_5")
table(Census$Adn2_name)   # check that results are correct


# ---------- Dropping observations ---------- # 
# It may be necessary to discard certain observations in certain cases - for example, in Peru, 
# households that the census-takers tried and failed to contact are included in the final dataset.
# however these observations are missing data that is indispensable for Housing Deficit calculations. 
# In this case we drop these unresponsive households, but not all NA values indicate that the data 
# should be dropped. See the Methodology and Guide document for more information. 
Census <- Census[(Census$household_response=="household unresponsive"),]


# ---------- Exploring NAs (null values) ---------- # 

sum(is.na(Census))
sum(is.na(Census$variable1))
table(Census$variable1, useNA="always")

## check stratification of these NAs to make sure that removing them will not significantly skew the data
NAtable <- aggregate(is.na(Census$variable1) ~ adm2, C2011_hh, sum)
PopTable <- aggregate(Census$population ~ adm2, C2011_hh, sum)
barplot((NAtable$`is.na(Census$variable1)`/PopTable$Census$population), main="% NAs by adm2", names.arg=NAtable$adm2,las=2)
## ---> shows % of answers that are null values 

## if it is decided that the NAs should be recoded, the line below can be used
Census$variable1[is.na(Census$variable1)] <- "Not Stated"


# ---------- Ensur ing that variables are numeric where they should be ---------- # 

## Household size must be made numeric (if currently coded as factor)
library(dplyr)
class(Census$hh_size)    # this shows if the variable is currently numeric or a factor
table(Census$hh_size, useNA="always")
Census$hh_size <- dplyr::recode(Census$hh_size, 'pob 1'=1, 'pob 2'=2, 'pob 3'=3, 'pob 4'=4, 'pob 5'=5, 
                                'pob 6'=6, 'pob 7'=7, 'pob 8'=8, 'pob 9'=9, 'pob 10'=10, 'pob 11'=11, 
                                'pob 12'=12, 'pob 13'=13, 'pob 14'=14, 'pob 15'=15, 'pob 16'=16, 
                                'pob 17'=17, 'pob 18'=18, 'pob 19'=19, 'pob 20'=20, 'pob 21'=21, 
                                'pob 22'=22, 'pob 23'=23, 'pob 24'=24, 'pob 25'=25, 'pob 26'=26, 
                                'pob 27'=27, 'pob 28'=28, 'pob 29'=29, 'pob 30'=30)
table(Census$hh_size, useNA="always")
hist(Census$hh_size)

## room data must be made numeric (if currently coded as factor)
class(Census$n_rooms)    # this shows if the variable is currently numeric or a factor
table(Census$n_rooms, useNA="always")
Census$n_rooms <- dplyr::recode(Census$n_rooms, '1 habitaci<f3>n'=1, '2 habitaciones'=2, 
                                '3 habitaciones'=3, '4 habitaciones'=4, '5 habitaciones'=5, 
                                '6 habitaciones'=6, '7 habitaciones'=7, '8 habitaciones'=8, 
                                '9 habitaciones'=9, '10 habitaciones'=10, '11 habitaciones'=11, 
                                '12 habitaciones'=12, '13 habitaciones'=13, '14 habitaciones'=14,
                                '15  habitaciones'=15) 
hist(Census$n_rooms)



# ---------- Save prepped file as csv ---------- # 

prepped_path <- "Data\\interrim_data_files\\Census_clean.csv"
if (OS == "unix"){     # file path adjustment for Macs
  prepped_path <- str_replace_all(prepped_path, '\\\\', '/\\')
}
write.csv(Census, file = prepped_path, row.names=FALSE)   




print('')
print('finished running script 1: Data Prep')
print('')


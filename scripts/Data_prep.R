

### Housing Deficit Estimation: data prep


#### This script standardizes the census data, producing a clean dataset for calculations. The dataset 
#### should contain information related to the geographic identifiers: in Guyana, these are region, 
#### NDC, village, and ED. This desaggregation can be refined based on available GIS shapefiles. 



# Set working directory:

setwd("C:\\Users\\jfisher\\Documents\\PROJECTS\\Guyana\\Housing Strategy\\Baseline Housing Needs\\Deliverables\\3. Calculations\\Data\\Microdata\\Census")


# Data on villages and NDCs from Guyana Bureau of Statistics. 
# This line imports the NDC's definition provided by The BoS in excel. 

NDCs <- read.csv("C:\\Users\\jfisher\\Documents\\PROJECTS\\Guyana\\Housing Strategy\\Baseline Housing Needs\\Deliverables\\3. Calculations\\Data\\Microdata\\Other\\NDC_villages.csv")


# Load data

if (!require("readstata13")) install.packages("readstata13")
library(readstata13)
phc_2012_hh <- read.dta13("guyana_phc_2012_hh_17may2017_rev13feb19.dta", add.rownames = FALSE)

# standardize the Census data:
phc_2012_hh$villno[phc_2012_hh$villno == "035" & phc_2012_hh$regno == "08"] <- "034"     #due to a coding error in the census data, Region 8's village 34 was coded as 35 (Region 8 has no Village 35)
phc_2012_hh$villno <- as.numeric(phc_2012_hh$villno)      #ensure that villno and regno are numeric
phc_2012_hh$regno <- as.numeric(phc_2012_hh$regno) 
phc_2012_hh <- phc_2012_hh[!(phc_2012_hh$dwno==""),]      #drop rows with insufficient information

phc_2012_hh$ednew <-paste(phc_2012_hh$regno, phc_2012_hh$edno)   #create composite variable 


#merge data from NDC file
joined_data <- inner_join(phc_2012_hh, NDCs, by = c("villno",  "regno"))


#save prepped file
write.csv(joined_data, file = "c12_ind_TEMP.csv", row.names=FALSE)




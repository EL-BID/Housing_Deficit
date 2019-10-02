
### Housing Deficit Estimation: indicators

# This script uses the cleaned census dataset to build the housing deficit indicators 
# as defined in the methodological note.


# ---------- load data ---------- # 

# set working directory
setwd("C:\\Users\\jfisher\\Documents\\PROJECTS\\Guyana\\Housing Strategy\\Baseline Housing Needs\\Deliverables\\3. Calculations\\Data\\Microdata")

# load cleaned census data
Census2012 <- read.csv("C:\\Users\\jfisher\\Documents\\PROJECTS\\Guyana\\Housing Strategy\\Baseline Housing Needs\\Deliverables\\3. Calculations\\Data\\Microdata\\Census\\c12_ind_TEMP.csv")
attach(Census2012)

# ---------- prepare variables ---------- # 

## BUILDING TYPE: deficit = non-residential or makeshift
Census2012$def_type_build <- ifelse(h11 == "community service" | h11 == "other" | h22 == "barracks" | h22 =="makeshift" | h22 == "other", 1, 0)

## COHABITATION: more than one household living in the dwelling
##     --> generate the household id and dwelling id
Census2012$hhid <- paste(regno, villno, edno, bldgno, dwno, hhno, sep="")
Census2012$dwid <- paste(regno, villno, edno, bldgno, dwno, sep="")
##     --> mark duplicates and create cohabitation variable
Census2012$dup <- ave(Census2012$dwid, Census2012$dwid, FUN = length) > 1L     # households sharing a dwelling are living in overcrowded conditions
Census2012$cohabitation <- ifelse(Census2012$dup == "TRUE", 1, 0)
Census2012 <- subset(Census2012, select = -c(dup))

## OVERCROWDING: > 3 people per room
##     --> overcrowding can be determined using rooms OR bedrooms, depending on the methodology 
##     --> in this case we will use bedrooms, based on typical housing patterns in Guyana

#Census2012$ratio_rms <- Census2012$hhtot/Census2012$h47
#Census2012$overcrowding <- ifelse(Census2012$ratio_rms > 3, 1, 0)
#Census2012 <- subset(Census2012, select = -c(ratio_rms))

## ACUTE OVERCROWDING: > 5 people per bedroom
Census2012$ratio_bdrms <- Census2012$hhtot/Census2012$h48
Census2012$overcrowding <- ifelse(Census2012$ratio_bdrms > 3, 1, 0)
Census2012$acute_overcrowd <- ifelse(Census2012$ratio_bdrms > 5, 1, 0)
Census2012 <- subset(Census2012, select = -c(ratio_bdrms))

## WALL MATERIAL: deficit = materials considered 'non-durable' (this should be revised based on climatological and cultural context)
Census2012$def_wall_mat <- ifelse(h12=="adobe & troolie palm"|h12=="makeshift"|h12=="galvanize"|h12=="other", 1, 0)

## ROOF MATERIAL: deficit = materials considered 'non-durable' (this should be revised based on climatological and cultural context)
Census2012$def_roof_mat <- ifelse(h13=="thatched/troolie palm"|h13=="makeshift"|h13=="other", 1, 0)

## ACCESS TO ELECTRICITY: deficit = no access 
Census2012$def_light_acc <- ifelse(h42=="kerosene"|h42=="gas lantern"|h42=="other", 1, 0)

## ACCESS TO PIPED WATER: deficit = inadequate access to water or drinking water (defined differently)
Census2012$def_water_acc <- ifelse(h43=="spring/river/pond"|h43=="other"|h44=="rain water collection"|h44=="unprotected dug-well/spring"|h44=="pond/river/stream"|h44=="other", 1, 0)

## ACCESS TO SEWERAGE: deficit = no access to piped sewerage
Census2012$def_sewar_acc <- ifelse(h45 =="w.c (flush toilet) linked to sewer"|h45=="w.c. (flush toilet) linked to septic tank/soak-away", 0, 1)  # NOTE: 0 and 1 were switched in this case for simplicity of coding

## ACCESS TO GARBAGE DISPOSAL: deficit = improvised disposal
Census2012$def_garba_acc <- ifelse(h49=="burning"|h49=="dumping/throwing into river/sea/pond"|h49=="other", 1, 0)



# ---------- estimate quantitative deficit ---------- # 

#Two characteristics are used to determine if a houshold is in quantitative deficit:
#  1. Cohabitation: > 1 household living in the same dwelling 
#  2. Acute overcrowding: more than 5 people per bedroom

attach(Census2012)  #this step should be repeated to add new variables to loaded library
Census2012$def_quanti <- ifelse(cohabitation==1|acute_overcrowd==1, 1, 0)



# ---------- estimate qualitative deficit ---------- # 

# Seven characteristics are used to determine if a houshold is in qualitative deficit:
#  1. House has low quality wall material
#  2. Roofing material
#  3. Overcrowding
#  4. Water
#  5. Electricity
#  6. Sewerage
#  7. Garbage disposal
  
Census2012$def_quali <- ifelse(def_wall_mat==1|def_roof_mat==1|overcrowding==1|def_light_acc==1|def_water_acc==1|def_sewar_acc==1|def_garba_acc==1, 1, 0)



# ---------- estimate total housing deficit ---------- # 

#Total housing deficit: households with either Quantitative 
#or Qualitative deficit are considered in housing deficit

Census2012$total_def <- ifelse(Census2012$def_quali==1|Census2012$def_quanti==1, 1, 0)

Census2012$total_no_def <- 1-Census2012$total_def
Census2012$total_h <- 1

#write final dataset to csv
write.csv(Census2012, file = "Census_indicators_full.csv", row.names=FALSE)



# ---------- create table by NDC ---------- # 

if (!require("tidyverse")) install.packages("tidyverse")   #if this installation fails you may need to update R
library(tidyverse)
#install.packages("installr"); library(installr) # install+load installr
#updateR() # updating R.


indicators_table <- Census2012 %>% 
  group_by(NDCno) %>% 
  summarise(total_h = sum(total_h), 
            total_no_def = mean(total_no_def), 
            total_def = mean(total_def), 
            def_quali = mean(def_quali),
            def_wall_mat = mean(def_wall_mat),
            def_roof_mat = mean(def_roof_mat),
            overcrowding = mean(overcrowding), 
            def_water_acc = mean(def_water_acc),
            def_sewar_acc = mean(def_sewar_acc), 
            def_light_acc = mean(def_light_acc),
            def_garba_acc = mean(def_garba_acc))

#write table to csv
write.csv(indicators_table, file = "Census_indicators_byNDC.csv", row.names=FALSE)




# ---------- association analysis ---------- # 

## prep data for association analysis
##     --> keep only data that will be used in analysis plus ID 
AA_data <- Census2012
AA_data <- AA_data[c("hhid", "def_wall_mat", "def_roof_mat", "overcrowding", "def_water_acc", "def_sewar_acc", "def_light_acc", "def_garba_acc")]

##     --> reshape and keep only deficit
library(reshape2)
Census_melt <- melt(AA_data, id=c("hhid"))
Census_melt <- Census_melt[!(Census_melt$value==0),]   # drop 1057597 (79%) observations not showing deficit to examine only sub-optimal circumstances
Census_melt$value <- NULL   


## prep data for association analysis
if (!require("arules")) install.packages("arules")
library(arules)
##     --> run association analysis (grouping items together)
Housing_Def_AA <- as(split(Census_melt[,"variable"], Census_melt[,"hhid"]), "transactions")   
# association rules format can be viewed using: inspect(head(Housing_Def_AA, 20))

##     --> collect rules from associations 
assoc_rules = apriori(Housing_Def_AA, parameter=list(minlen=2, support = 0.05)) 
inspect(head(assoc_rules, 15, by = "lift"))




# ---------- visualize associations ---------- # 

if (!require("arulesViz")) install.packages("arulesViz")
library(arulesViz)

rules_viz <- head(assoc_rules, n = 15, by = "lift")   
      # lift shows the relationship between the lhs and the rhs. 
      #     Lift > 1 indicates that lhs and rhs are dependent
      #     Lift = 1 indicates that LHS and rhs are independent
      #     Lift < 1 indicates that lhs and rhs replace each other

plot(rules_viz, method = "graph", engine = "htmlwidget")
#for more polished but non-interactive version, try:
#plot(rules_viz, method = "graph")










# ---------- coefficient of variation: test conditions for model ---------- # 

# Before beginning the predictions, the statistical accuracy of the indicators is measured by a coefficient 
# of variation (CV). The CV captures the indicators’ dispersion, or how informative the indicator is in 
# statistical terms. The CV is defined as the standard deviation of the indicator divided by indicator’s mean  
# multiplied by 100. The lower the CV, the more precise the estimation is. Colombia's DANE defines a scale to 
# determine how informative or precise a housing indicator is: 
#            Good precision: CV <7.5
#      Acceptable precision: 7.5 <= CV <= 15
#             Low precision: 15 <= CV < 30
#             Bad precision: 30<= CV
# We will use the same scale here. 


#calculate CV for each indicator
CV <- data.frame("value")
CV$CV_walls = mean(Census2012$def_wall_mat)*100
CV$CV_roof = mean(Census2012$def_roof_mat)*100
CV$CV_overc = mean(Census2012$overcrowding)*100
CV$CV_water = mean(Census2012$def_water_acc)*100
CV$CV_sewar = mean(Census2012$def_sewar_acc)*100
CV$CV_light = mean(Census2012$def_light_acc)*100
CV$CV_garba = mean(Census2012$def_garba_acc)*100
CV$CV_cohab = mean(Census2012$cohabitation)*100
CV$CV_acute = mean(Census2012$acute_overcrowd)*100
CV




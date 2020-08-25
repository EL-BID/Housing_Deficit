### Housing Deficit Estimation: Statistical test for prediction methodology

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

# This script tests the calculated housing deficit indicators to determine if the luminosity 
# regression used to now-cast updated deficit values by administrative division will 
# be an appropriate methodology for the given dataset. Two exploratory tests are used:
#    1. Association analysis
#    2. Coefficient of variance (CV)




# ---------- load data ---------- # 

# load full indicators dataset
indicators_path <- "Data\\interrim_data_files\\Census_indicators_full.csv"
if (OS == "unix"){    
  indicators_path <- str_replace_all(indicators_path, '\\\\', '/\\')    # file path adjustment for Macs
}
indicators_full <- read.csv(indicators_path)



# ---------- association analysis ---------- # 

# Association analysis is a rules-based ML technique that reveals relationships between variables, 
# i.e., how likely it is that a household exhibiting one characteristic will also exhibit another. 
# The main metric to look at in an association analysis is called Lift. Lift shows the relationship 
# between the left-hand side (LHS) and the right-hand side (RHS), i.e., how likely it is that the 
# right-hand side will occur in a case where the left-hand side is already occurring. 
#    -	Lift > 1 indicates that LHS and RHS are dependent
#    -	Lift = 1 indicates that LHS and RHS are independent
#    -	Lift < 1 indicates that lhs and rhs replace each other


## prep data for association analysis
##     --> keep only data that will be used in analysis plus ID 
AA_data <- indicators_full
AA_data <- AA_data[c("hhid", "def_type_build", "def_wall_mat", "def_roof_mat", "overcrowding", "acute_overcrowd", "def_water_acc", "def_sewar_acc", "def_light_acc")]

##     --> reshape and keep only deficit
library(reshape2)
Census_melt <- melt(AA_data, id=c("hhid"))
Census_melt <- Census_melt[!(Census_melt$value==0),]   # drop observations not showing deficit to examine only sub-optimal circumstances
Census_melt$value <- NULL   


## prep data for association analysis
if (!require("arules")) install.packages("arules")
library(arules)
##     --> run association analysis (grouping items together)
Housing_Def_AA <- as(split(Census_melt[,"variable"], Census_melt[,"hhid"]), "transactions")   
#inspect(head(Housing_Def_AA, 20))       # association rules format can be viewed using this line

##     --> collect rules from associations 
assoc_rules = apriori(Housing_Def_AA, parameter=list(minlen=2, support = 0.05)) 
inspect(head(assoc_rules, 15, by = "lift"))


# Check the strongest rules showing what types of deficit commonly afflict households at the
# same time. Lift shows how much more likely the LHS is if the RHS is given. For example the rule: 
# 
#      lhs                                rhs             support   confidence lift     count
# [1]  {overcrowding,def_water_acc}    => {def_light_acc} 0.1448574 0.9151139  5.895750 59476
# 
# indicates that an overcrowded households where there is a lack of access to piped water, is 
# almost 6 time as likely to also lack access to electric light. 



# ---------- visualize associations ---------- # 

if (!require("arulesViz")) install.packages("arulesViz")
library(arulesViz)

rules_viz <- head(assoc_rules, n = 10, by = "lift")   
# lift shows the relationship between the lhs and the rhs. 
#     Lift > 1 indicates that lhs and rhs are dependent
#     Lift = 1 indicates that LHS and rhs are independent
#     Lift < 1 indicates that lhs and rhs replace each other

plot(rules_viz, method = "graph", engine = "htmlwidget")
#for more polished but non-interactive version, try:
#plot(rules_viz, method = "graph")




# ---------- coefficient of variation ---------- # 

# Before beginning the predictions, the statistical accuracy of the indicators is measured by a coefficient 
# of variation (CV). The CV captures the indicators' dispersion, or how informative the indicator is in 
# statistical terms. The CV is defined as the standard deviation of the indicator divided by indicator's mean  
# multiplied by 100. The lower the CV, the more precise the estimation is. Colombia's DANE defines a scale to 
# determine how informative or precise a housing indicator is: 
#            Good precision: CV <7.5
#      Acceptable precision: 7.5 <= CV <= 15
#             Low precision: 15 <= CV < 30
#             Bad precision: 30 <= CV
# We will use the same scale here. 


#calculate CV for each indicator
CV <- data.frame("value")
CV$CV_walls = mean(indicators_full$def_wall_mat)*100      # CV = 1.7 , very good precision
CV$CV_roof = mean(indicators_full$def_roof_mat)*100       # CV = 0.5 , very good precision
CV$CV_overc = mean(indicators_full$overcrowding)*100      # CV = 15.8, low precision
CV$CV_water = mean(indicators_full$def_water_acc)*100     # CV = 7.6 , acceptable precision
CV$CV_sewar = mean(indicators_full$def_sewar_acc)*100     # CV = 24.3, low precision
CV$CV_light = mean(indicators_full$def_light_acc)*100     # CV = 15.5, low precision
CV$CV_garba = mean(indicators_full$def_garba_acc)*100     # CV = 8.8 , acceptable precision
CV$CV_cohab = mean(indicators_full$cohabitation)*100      # CV = 1.2 , very good precision
CV$CV_acute = mean(indicators_full$acute_overcrowd)*100   # CV = 23.9, low precision
CV


# Note that if the CV for electric light is too high, thus indicating less than acceptable precision
# for this indicator, it is likely that the results of the predictions obtained from the luminosity
# regression will not be significant with high confidence. 




print('')
print('finished running script 3: Stat test')
print('')




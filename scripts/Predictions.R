
### Housing Deficit Estimation: Predictions

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

# This script 'predicts' qualitative and total housing deficit for the year of the most recently 
# available satellite imagery. This script uses the csv files produced using QGIS explained in 
# Section IV of the Methogology and Guide documentation. Qualitative and total housing deficit
# are combined with the nightlight data from the NASAS's VIIRS program for the base year and 
# the target year. All predictions are made by the selected administrative division.



# ---------- load nightlight data ---------- # 

# set working directory
setwd("C:\\Users\\jfisher\\Documents\\PROJECTS\\HousingDef")

## base year
night_base <- read.csv("night_base_year.csv")

## target year
night_target <- read.csv("night_target_year.csv")


# ---------- open housing deficit data ---------- # 

HousingDef <- read.csv("Deficit_indicators_byAdm.csv")

#merge with night lights data
night_base_indicators <- merge(HousingDef, night_base, by.x = "Adm2", by.y = "NAME_2")
night_target_indicators <- merge(HousingDef, night_target, by.x = "Adm2", by.y = "NAME_2")




# ---------- regression estimation of housing deficit on night lights ---------- # 

# This excercise generates the parameters needed from base year data to predict the deficits using data 
# from the target year. This excercise follows the methodology proposed by Henderson et.al 2012, and 
# World Bank 2015 (Ukraine). 

# The script contains the following parts:
#   1. model is estimated:  deficit_baseyr=b0+b1*(nightlight_baseyr)+e 
#      where estimation of b0 and b1 and the prediction of e are obtained
#   2. using target year data, target year predictions are found using:  
#      deficit_targetyr=b0_baseyr+b1_baseyr*(nightlight_targetyr)+e_targetyr
#      These estimates are contrasted with base year data to check goodness of fit, and then used to 
#      plot a map.



# ---------- fit models ---------- # 

# Regress TOTAL deficit against luminosity in the base year
fit <- lm(total_def ~ light_mean, data=night_base_indicators)
summary(fit)
predictions <- predict(fit, night_base, se.fit = TRUE)
##     --> save cofficients and prediction errors for use in the target year data
night_target_indicators$errors_totaldef <- predictions$se.fit
b0 <- fit$coefficients[1]
b1 <- fit$coefficients[2]
##     --> predict target year total deficit
night_target_indicators$predict_total_def <- b0 + b1*night_target_indicators$light_mean + night_target_indicators$errors_totaldef


## Regress QUALITATIVE deficit against luminosity in the base year
fit_q <- lm(def_quali ~ light_mean, data=night_base_indicators)
summary(fit_q)
predictions_q <- predict(fit_q, night_base, se.fit = TRUE)
##     --> save cofficients and prediction errors for use in target year data
night_target_indicators$errors_qualdef <- predictions_q$se.fit
b0_q <- fit_q$coefficients[1]
b1_q <- fit_q$coefficients[2]
##     --> predict target year qualitative deficit
night_target_indicators$predict_qual_def <- b0_q + b1_q*night_target_indicators$light_mean + night_target_indicators$errors_qualdef


# visually check goodness of fit
if (!require("ggplot2")) install.packages("ggplot2") 
library(ggplot2)
ggplot(night_target_indicators, aes(Adm2, group = 1)) + theme_classic() +
  geom_line(aes(y = total_def, colour = "deficit base")) + 
  geom_line(aes(y = predict_total_def, colour = "predicted deficit")) + 
  ggtitle("Total Housing Deficit") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#save predictions to csv file
write.csv(night_target_indicators, file = "Prediction_deficit.csv")




# ---------- maps of predictions ---------- # 

# the following code uses administrative division 2 shapefiles to map the calculated types of deficit 

if (!require("rgdal")) install.packages("rgdal") 
library(rgdal)
library(sp)

adm2 <- readOGR("adm2.shp")
plot(adm2)
## check data already embedded in shapefile
head(adm2@data)

#night_target_indicators <- read.csv("Prediction_deficit.csv")

 
# merge target indicators table to shapefile by adm2 
list(night_target_indicators$Adm2)
list(adm2@data$NAME_2)      # verify that adm2 names or numbers match 
library("dplyr")
colnames(adm2@data)[colnames(adm2@data)=="NAME_2"] <- "Adm2"      # column names must match too


## now merge data and check the results
Pred_map <- sp::merge(adm2, TT_pred, by='Adm2')
head(Pred_map@data)
plot(Pred_map)


# save as shapefile!
writeOGR(Pred_map, "predictions_by_adm2.shp", layer="Pred_map", driver="ESRI Shapefile")
#Pred_map <- readOGR("predictions_by_adm2.shp")




# mapping in detail
if (!require("sf")) install.packages("sf") 
library(sf)
if (!require("tmap")) install.packages("tmap") 
library(tmap)
if (!require("rgeos")) install.packages("rgeos") 
library(rgeos)

## convert shapefile to sf for graphing
Pred_map2 <- st_as_sf(Pred_map)
class(Pred_map2)
summary(Pred_map2)


tmap_mode('plot')


## total deficit
td <- tm_shape(Pred_map2) + 
        tm_polygons("predict_total_def", 
                    title = "Propotion hh",
                    breaks=seq(0, .1, by=.1)
                    , border.col = "white") + 
        tm_layout(frame=F, title="Predicted overall Deficit by Adm2, target year", 
                    title.size = 1.5, inner.margins=c(.25,.25,.1,.1)) +
        tm_legend(position = c("left", "center"))
td
save_tmap(td, "Pred_otal_def.pdf")    # save map as PDF


## qualitative deficit
qd <- tm_shape(Pred_map2) + 
        tm_polygons("predict_qual_def", 
                    title = "Propotion hh",
                    breaks=seq(0, .1, by=.1)
                    , border.col = "white") + 
        tm_layout(frame=F, title="Predicted qualitative Deficit by Adm2, target year", 
                    title.size = 1.5, inner.margins=c(.25,.25,.1,.1)) +
        tm_legend(position = c("left", "center"))
qd
save_tmap(qd, "Pred_def_quali.pdf")








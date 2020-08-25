### Housing Deficit Estimation: Predictions

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

# This script 'predicts' qualitative and total housing deficit for the year of the most recently 
# available satellite imagery. This script uses the csv files produced using QGIS explained in 
# Section IV of the Methogology and Guide documentation. Qualitative and total housing deficit
# are combined with the nightlight data from the NASAS's VIIRS program for the base year and 
# the target year. All predictions are made by the selected administrative division.




# ---------- load nightlight data ---------- # 

## base year
night_baseyr_path <- "Data\\interrim_data_files\\nighttime_baseyear_per_adm.csv"
if (OS == "unix"){
  night_baseyr_path <- str_replace_all(night_baseyr_path, '\\\\', '/\\')     # file path adjustment for Macs
}
night_baseyr <- read.csv(night_baseyr_path)  

## target year
night_targetyr_path <- "Data\\interrim_data_files\\nighttime_targetyear_per_adm.csv"
if (OS == "unix"){
  night_targetyr_path <- str_replace_all(night_targetyr_path, '\\\\', '/\\')     # file path adjustment for Macs
}
night_targetyr <- read.csv(night_targetyr_path)  



# ---------- open housing deficit data ---------- # 

indicators_path <- "Data\\interrim_data_files\\Deficit_indicators_byAdm.csv"   # update to reflect calculations
if (OS == "unix"){     # file path adjustment for Macs
  indicators_path <- str_replace_all(indicators_path, '\\\\', '/\\')
}
HousingDef <- read.csv(indicators_path)  


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

final_path <- "Data\\interrim_data_files\\predicted_indicators.csv"
if (OS == "unix"){     # file path adjustment for Macs
  final_path <- str_replace_all(final_path, '\\\\', '/\\')
}
write.csv(night_target_indicators, file = final_path, row.names=FALSE)





# ---------- plot map of predictions ---------- # 


# load second administrative division shapefile
adm2_path <- "Data\\GIS\\Shape\\COUNTRY_adm\\COUNTRY_adm2.shp"
if (OS == "unix"){     # file path adjustment for Macs
  adm2_path <- str_replace_all(adm2_path, '\\\\', '/\\')
}
adm2 <- readOGR(adm2_path)


## simplify object to make plotting quicker
object.size(adm2)
adm2 <- rmapshaper::ms_simplify(adm2)  
object.size(adm2)
plot(adm2)

crs <- st_crs(adm2)
divisions <- st_as_sf(adm2, coords = c("longitude", "latitude"), crs = crs)  # make NDC shapefile df format


#merge with indicators data
indicators_geom_target <- merge(divisions, night_target_indicators, by.x = "ID_2", by.y = "NDCno")

ggplot() +
  geom_sf(data = indicators_geom_target, color='white', aes(fill=pred_def), lwd = 0.3) + 
  theme_void()


# save image as pdf
pred_img_path <- "Data\\imgs\\Pred_qual_def.pdf"
if (OS == "unix"){     # file path adjustment for Macs
  pred_img_path <- str_replace_all(pred_img_path, '\\\\', '/\\')
}
ggsave(pred_img_path)



print('')
print('finished running script 5: Predictions')
print('')




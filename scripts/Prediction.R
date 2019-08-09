
### Housing Deficit Estimation: predictions

# This dofile predicts qualitative and total housing deficit. Qualitative and total housing deficit are combined
# with the nightlight data from the NASAS's VIIRS program for 2012 and 2019. All predictions are by NDC, for a 
# total of 115 observations.


# ---------- load nightlight data ---------- # 

## 2012
night2012 <- read.csv("C:\\Users\\jfisher\\Documents\\PROJECTS\\Guyana\\Housing Strategy\\Baseline Housing Needs\\Deliverables\\3. Calculations\\Data\\GIS\\Excel\\nighttime_2012_11_GY_NDC_raw.csv")

## 2019
night2019 <- read.csv("C:\\Users\\jfisher\\Documents\\PROJECTS\\Guyana\\Housing Strategy\\Baseline Housing Needs\\Deliverables\\3. Calculations\\Data\\GIS\\Excel\\nighttime_2019_04_GY_NDC_raw.csv")


# ---------- open housing deficit data ---------- # 

HousingDef <- read.csv("C:\\Users\\jfisher\\Documents\\PROJECTS\\Guyana\\Housing Strategy\\Baseline Housing Needs\\Deliverables\\3. Calculations\\Data\\Microdata\\Census_indicators_byNDC.csv")

#merge with night lights data
night2012_indicators <- merge(HousingDef, night2012, by.x = "NDCno", by.y = "ndcno")
night2019_indicators <- merge(HousingDef, night2019, by.x = "NDCno", by.y = "ndcno")


# ---------- regression estimation of housing deficit on night lights ---------- # 

# This excercise generates the parameters needed from 2012 data to predict the deficits using data from 2019.
# This excercise follows the methodology proposed by Henderson et.al 2012, and World Bank 2015 (Ukraine)


# Models

## Regress TOTAL deficit against luminosity in 2012
fit <- lm(total_def ~ meanDN, data=night2012_indicators)
summary(fit)
predictions <- predict(fit, night2012, se.fit = TRUE)
##     --> save cofficients and prediction errors for use in 2019 data
night2019_indicators$errors_totaldef <- predictions$se.fit
b0 <- fit$coefficients[1]
b1 <- fit$coefficients[2]
##     --> predict 2019 total deficit
night2019_indicators$predict_total_def_19 <- b0 + b1*night2019_indicators$meanDN + night2019_indicators$errors_totaldef


## Regress QUALITATIVE deficit against luminosity in 2012
fit_q <- lm(def_quali ~ meanDN, data=night2012_indicators)
summary(fit_q)
predictions_q <- predict(fit_q, night2012, se.fit = TRUE)
##     --> save cofficients and prediction errors for use in 2019 data
night2019_indicators$errors_qualdef <- predictions_q$se.fit
b0_q <- fit_q$coefficients[1]
b1_q <- fit_q$coefficients[2]
##     --> predict 2019 qualitative deficit
night2019_indicators$predict_qual_def_19 <- b0_q + b1_q*night2019_indicators$meanDN + night2019_indicators$errors_qualdef


# check goodness of fit
if (!require("ggplot2")) install.packages("ggplot2") 
library(ggplot2)
ggplot(night2019_indicators, aes(NDCno)) + theme_classic() +
  geom_line(aes(y = total_def, colour = "deficit 2012")) + 
  geom_line(aes(y = predict_total_def_19, colour = "predicted deficit 2019")) + 
  ggtitle("Total Housing Deficit") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")







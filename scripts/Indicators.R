
### Housing Deficit Estimation: indicators

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

# This script uses the cleaned census dataset to build the housing deficit indicators as defined
# in the methodological note, writes the calculations into a table by administrative division, 
# and maps the calculated deficit by administrative division using the provided shapefile



# ---------- load data ---------- # 

# set working directory
setwd("C:\\Users\\jfisher\\Documents\\PROJECTS\\HousingDef")

# load cleaned census data
Census_c <- read.csv("Census_clean.csv")
attach(Census_c)



# ---------- prepare indicators ---------- # 

# check variables and factors
str(Census_c)     # check variables in dataset



## BUILDING TYPE: deficit = non-residential, Other or Not Stated 
table(type_build, useNA = 'always')       # check factors in variable of interest
Census_c$def_type_build <- ifelse(type_build=="Community Service"|type_build=="Other"|type_build=="Not Stated", 1, 0)
pie(table(Census_c$def_type_build), labels = c("no deficit", "deficit"), main="Type of building")    # pie chart quickly shows proportion of households experiencing this type of deficit


## COHABITATION: more than one household living in a dwelling
##     --> generate unique household id and dwelling id, households with the same dwelling ID are cohabiting
Census_c$hhid <- paste(adm1, adm2, adm3, building, dwelling, hh, sep="")
Census_c$dwid <- paste(adm1, adm2, adm3, building, dwelling, sep="")
##     --> mark duplicates and create cohabitation variable
Census_c$dup <- ave(Census_c$dwid, Census_c$dwid, FUN = length) > 1L     # households sharing a dwelling are cohabiting
Census_c$cohabitation <- ifelse(Census_c$dup == "TRUE", 1, 0)
Census_c <- subset(Census_c, select = -c(dup))     # drop dummy variable
pie(table(Census_c$cohabitation), labels = c("no deficit", "deficit"), main="Cohabitation")


## OVERCROWDING: > 3 people per room
##     --> overcrowding can be determined using rooms OR bedrooms, depending on the methodology
##     --> overcrowding standards might be different for urban and rural communities
##         (refer to Methodological Report and Guide for more info)

## The following is an example using bedrooms, with no diferentiation for urban/rural 
table(n_bedrooms, useNA = "always")       # use table to identify factor names in variable of interest
Census_c$ratio_bdrms <- Census_c$hh_size/Census_c$n_bedrooms
Census_c$overcrowding <- ifelse(Census_c$ratio_bdrms > 3 | is.na(Census_c$ratio_bdrms), 1, 0)
pie(table(Census_c$overcrowding), labels = c("no deficit", "deficit"), main="Overcrowding")

## The following is an example using bedrooms, diferentiating for urban/rural 
table(n_bedrooms, useNA = "always")
Census_c$ratio_bdrms <- Census_c$hh_size/Census_c$n_bedrooms
Census_c$overcrowding <- ifelse(urban==1, 
                                  (ifelse(ratio_rms>4|
                                            is.na(ratio_rms), 1, 0)),
                                  (ifelse(ratio_rms>3|
                                            is.na(ratio_rms), 1, 0)))
Census_c$overcrowding <- ifelse(Census_c$overcrowding==1|is.na(Census_c$overcrowding), 1, 0)
pie(table(Census_c$overcrowding), labels = c("no deficit", "deficit"), main="Overcrowding")

## The following is an example using rooms, with no diferentiation for urban/rural 
table(n_rooms, useNA = "always")
Census_c$ratio_rms <- Census_c$hh_size/Census_c$n_rooms
Census_c$overcrowding <- ifelse(Census_c$ratio_rms > 3 | is.na(Census_c$ratio_rms), 1, 0)
Census_c <- subset(Census_c, select = -c(ratio_rms))
pie(table(Census_c$overcrowding), labels = c("no deficit", "deficit"), main="Overcrowding")


## ACUTE OVERCROWDING: > 5 people per bedroom
table(n_bedrooms, useNA = "always")
Census_c$ratio_bdrms <- Census_c$hh_size/Census_c$n_bedrooms
Census_c$acute_overcrowd <- ifelse(Census_c$ratio_bdrms > 5 | is.na(Census_c$ratio_bdrms), 1, 0)
Census_c <- subset(Census_c, select = -c(ratio_bdrms))
pie(table(Census_c$acute_overcrowd), labels = c("no deficit", "deficit"), main="Acute Overcrowding")


## WALL MATERIAL: deficit = materials considered 'non-durable' (this should be revised based on climatological and cultural context)
table(wall_mat, useNA='always')       # use table to identify factor names in variable of interest
Census_c$def_wall_mat <- ifelse(wall_mat=="Wood & Galvanize"|wall_mat=="Wattle/ Adobe/ Tapia"|wall_mat=="Not Stated"|wall_mat=="Other", 1, 0)
pie(table(Census_c$def_wall_mat), labels = c("no deficit", "deficit"), main="Wall Materials")


## ROOF MATERIAL: deficit = materials considered 'non-durable' (this should be revised based on climatological and cultural context)
table(roof_mat, useNA='always')       # use table to identify factor names in variable of interest
Census_c$def_roof_mat <- ifelse(roof_mat=="Thatch/ Makeshift"|roof_mat=="Not Stated"|roof_mat=="Other", 1, 0)
pie(table(Census_c$def_roof_mat), labels = c("no deficit", "deficit"), main="Roof Materials")


## ACCESS TO ELECTRICITY: deficit = no access to electric lighting
table(type_light, useNA='always')       # use table to identify factor names in variable of interest
Census_c$def_light_acc <- ifelse(type_light=="Kerosene"|type_light=="Gas"|type_light=="Not Stated"|type_light=="Other", 1, 0)
pie(table(Census_c$def_light_acc), labels = c("no deficit", "deficit"), main="Access to Electric Light")


## ACCESS TO PIPED WATER: deficit = inadequate access to water or drinking water (defined differently)
table(water, useNA='always')       # use table to identify factor names in variables of interest
Census_c$def_water_acc <- ifelse(water=="Private Catchment"|water=="Spring/River/Well/Pond"|water=="Not Stated"|water=="Other", 1, 0)
pie(table(Census_c$def_water_acc), labels = c("no deficit", "deficit"), main="Access to Piped Water")


## ACCESS TO SEWERAGE: deficit = no access to piped sewerage
table(sewerage, useNA='always')       # use table to identify factor names in variable of interest
Census_c$def_sewer_acc <- ifelse(sewerage =="WC linked Septic Tank"|sewerage=="WC linked Sewer", 0, 1)  # NOTE: 0 and 1 were switched in this case for simplicity of coding
pie(table(Census_c$def_sewer_acc), labels = c("no deficit", "deficit"), main="Access to Piped Sewerage")


## ACCESS TO GARBAGE DISPOSAL: deficit = improvised disposal
table(waste, useNA='always')       # use table to identify factor names in variable of interest
Census_c$def_garba_acc <- ifelse(waste=="burning"|waste=="dumping/throwing into river/sea/pond"|waste=="other", 1, 0)
pie(table(Census_c$def_garba_acc), labels = c("no deficit", "deficit"), main="Waste Disposal")


attach(Census_c)  #this step should be repeated to add new variables to loaded library




# ---------- estimate quantitative deficit ---------- # 

#Two characteristics are used to determine if a household is in quantitative deficit:
#  1. Cohabitation: > 1 household living in the same dwelling 
#  2. Acute overcrowding: more than 5 people per bedroom
# These indicators may be adjusted at the discretion of the user based on country 
# standards, conditions specific to the area of study, and available data

Census_c$def_quanti <- ifelse(cohabitation==1|acute_overcrowd==1, 1, 0)



# ---------- estimate qualitative deficit ---------- # 

# Seven characteristics are considered to determine if a household is in qualitative deficit:
#  1. House has low quality wall material
#  2. Roofing material
#  3. Overcrowding
#  4. Water
#  5. Electricity
#  6. Sewerage
#  7. Garbage disposal 
# These indicators may be adjusted at the discretion of the user based on country 
# standards, conditions specific to the area of study, and available data

Census_c$def_quali <- ifelse(def_wall_mat==1|def_roof_mat==1|overcrowding==1|def_light_acc==1|def_water_acc==1|def_sewer_acc==1|def_garba_acc==1, 1, 0)



# ---------- estimate total housing deficit ---------- # 

# Total housing deficit: households with either Quantitative 
# or Qualitative deficit are considered in overall housing deficit

Census_c$total_def <- ifelse(Census_c$def_quali==1|Census_c$def_quanti==1, 1, 0)

Census_c$total_no_def <- 1-Census_c$total_def
Census_c$total_h <- 1

#write final dataset to csv
write.csv(Census_c, file = "Census_indicators_full.csv", row.names=FALSE)




# ---------- create table by Administrative Division ---------- # 

if (!require("tidyverse")) install.packages("tidyverse")   #if this installation fails you may need to update R
library(tidyverse)
#install.packages("installr"); library(installr) # install+load installr
#updateR() # updating R.


indicators_table <- Census_c %>% 
  group_by(Adm2) %>% 
  summarise(total_hh = sum(hh_q), 
            total_no_def = mean(total_no_def), 
            total_def = mean(total_def), 
            def_quali = mean(def_quali),
            def_quanti = mean(def_quanti),
            def_wall_mat = mean(def_wall_mat),
            def_roof_mat = mean(def_roof_mat),
            overcrowding = mean(overcrowding), 
            acute_overcrowd = mean(acute_overcrowd),
            cohabitation = mean(cohabitation),
            def_water_acc = mean(def_water_acc),
            def_sewar_acc = mean(def_sewar_acc), 
            def_light_acc = mean(def_light_acc))

#write table to csv
write.csv(indicators_table, file = "Deficit_indicators_byAdm.csv", row.names=FALSE)
#indicators_table <- read.csv("Deficit_indicators_byAdm.csv")


detach(Census2017)




# ---------- maps of indicators ---------- # 

# the following code uses administrative division 2 shapefiles (provinces) 
# to map the calculated types of deficit and even individual indicators by province

if (!require("rgdal")) install.packages("rgdal") 
library(rgdal)
library(sp)

adm2 <- readOGR("ADM\\Adm2.shp")
plot(adm2)
## check data already embedded in shapefile
head(adm2@data)


# merge indicators table to shapefile by adm2 
list(indicators_table$Adm2)
list(adm2@data$NAME_2)      # verify that adm2 names or numbers match 
library("dplyr")
colnames(adm2@data)[colnames(adm2@data)=="NAME_2"] <- "Adm2"      # column names must match too

## now merge data and check the results
Country_map <- sp::merge(adm2, indicators_table, by='Adm2')
head(Country_map@data)
plot(Country_map)


# save as shapefile!
writeOGR(Country_map, "Map_indicators.shp", layer="Country_map", driver="ESRI Shapefile")
#Country_map <- readOGR("Map_indicators.shp.shp")



# mapping in detail
if (!require("sf")) install.packages("sf") 
library(sf)
if (!require("tmap")) install.packages("tmap") 
library(tmap)
if (!require("rgeos")) install.packages("rgeos") 
library(rgeos)

## convert shapefile to sf for graphing
Country_map2 <- st_as_sf(Country_map)
#class(Country_map2)
#summary(Country_map2)

tmap_mode('plot')


# mapping types of deficit

## total deficit
td <- tm_shape(Country_map2) + 
        tm_polygons("total_def", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
        tm_layout(frame=F, title="Overall Deficit by Adm2, [year]", title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
        tm_legend(position = c("left", "center"))
td
save_tmap(td, "Total_def.pdf")    # save map as PDF

## qualitative deficit
qd <- tm_shape(Country_map2) + 
        tm_polygons("def_quali", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
        tm_layout(frame=F, title="Qualitative Deficit by Adm2, base year", title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
        tm_legend(position = c("left", "center"))
qd
save_tmap(qd, "Def_quali.pdf")    # save map as PDF

## quantitative deficit
qnd<- tm_shape(Country_map2) + 
        tm_polygons("def_quanti", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
        tm_layout(frame=F, title="Quantitative Deficit by Adm2, base year", title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
        tm_legend(position = c("left", "center"))
qnd
save_tmap(qnd, "Def_quanti.pdf")    # save map as PDF


# mapping deficit drivers

## deficit access to electric light
tm_shape(Country_map2) + 
  tm_polygons("def_light_acc", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
  tm_layout(frame=F, title="Deficit in access to electric light by Adm2, base year", 
            title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
  tm_legend(position = c("left", "center"))

## deficit access to piped water
tm_shape(Country_map2) + 
  tm_polygons("def_water_acc", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
  tm_layout(frame=F, title="Deficit in access to piped water by Adm2, base year", 
            title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
  tm_legend(position = c("left", "center"))

## deficit access to adequate sewerage
tm_shape(Country_map2) + 
  tm_polygons("def_sewer_acc", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
  tm_layout(frame=F, title="Deficit in access to adequate sewerage by Adm2, base year", 
            title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
  tm_legend(position = c("left", "center"))

## deficit due to cohabitation
tm_shape(Country_map2) + 
  tm_polygons("cohabitation", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
  tm_layout(frame=F, title="Deficit due to cohabitation by Adm2, base year", 
            title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
  tm_legend(position = c("left", "center"))

## deficit due to overcrowding
tm_shape(Country_map2) + 
  tm_polygons("overcrowding", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
  tm_layout(frame=F, title="Deficit due to overcrowding by Adm2, base year", 
            title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
  tm_legend(position = c("left", "center"))

## deficit due to acute overcrowding
tm_shape(Country_map2) + 
  tm_polygons("acute_overcrowd", title = "Propotion hh", breaks=seq(0, 1, by=.1), border.col = "white") + 
  tm_layout(frame=F, title="Deficit due to acute overcrowding by Adm2, base year", 
            title.size = 1.5, inner.margins=c(.2,.2,.2,.2)) +
  tm_legend(position = c("left", "center"))








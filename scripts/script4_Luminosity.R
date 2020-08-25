
### Housing Deficit Estimation: luminosity data processing

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

# This script will process geotiff files (.tif) that can be downloaded from NASAS's VIIRS website
# (https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html). In this case, tiles 1 and 2 were used
# for each the base year and the target year. In this case, we will use 2012 as base year and 2019 as 
# target year. This script will cover the following steps to extract nocturnal luminosity data:
#    1) Merging raster files
#    2) clipping the merged file to the area of study
#    3) Polygonizing (vectorizing) luminosity data within the clipped area
#    4) joining the vectorized data with administrative division data (spatial join)
#    5) exporting the data in tabular form


if (!require("raster")) install.packages("raster") 
library(raster)
if (!require("rgdal")) install.packages("rgdal") 
library(rgdal)
library(sp)
library(ggplot2)
library(dplyr)
library(sf)
if (!require("maptools")) install.packages("maptools") 
library(maptools)
if (!require("spex")) install.packages("spex") 
library(spex)



############ 2012 satellite imagery ############

# ---------- load satellite images (raster files) (2012) ---------- # 

# Most countries fall within the boundaries of a single satellite imagery tile. However in cases where 
# multiple tiles are needed to cover the land area of a country, the tiles can be merged as shown in 
# the next section. 

tile1_path <- "Data\\GIS\\SVDNB_npp_20121101-20121130_75N060W_vcmcfg_v10_c201601270845.avg_rade9h.tif"
if (OS == "unix"){
  tile1_path <- str_replace_all(tile1_path, '\\\\', '/\\')     # file path adjustment for Macs
}
tile1 <- raster(tile1_path)

tile1@crs
col=hcl.colors(5, palette = "viridis")
image(tile1, col=col)  # it is very hard to perceive the lights in this visualization, this is normal. 


tile2_path <- "Data\\GIS\\SVDNB_npp_20121101-20121130_75N180W_vcmcfg_v10_c201601270845.avg_rade9h.tif"
if (OS == "unix"){     # file path adjustment for Macs
  tile2_path <- str_replace_all(tile2_path, '\\\\', '/\\')
}
tile2 <- raster(tile2_path)

tile2@crs
image(tile2, col=col)


# ---------- merge raster files (2012) ---------- # 

# working with this file will be computationally expensive (likely to take some time); 
# this is why we will crop it in the next steps. However some adjustments below might 
# help things move more quickly in the interrim. 

rasterOptions()
rasterOptions(maxmemory = 1e+09)

merged <- merge(tile1, tile2)  
merged@crs


# ---------- clip merged raster (2012) ---------- # 

# If the country being analyzed did not require multiple tiles, the single tile raster file would be used 
# here in place of the merged raster. 

# load country shapefile
country_path <- "Data\\GIS\\Shape\\COUNTRY_adm\\COUNTRY_adm0.shp"
if (OS == "unix"){
  country_path <- str_replace_all(country_path, '\\\\', '/\\')     # file path adjustment for Macs
}
adm0 <- readOGR(country_path)  

# check that CRS match in both raster and shape files
merged@crs
st_crs(adm0)

# plot both raster and shape files
plot(adm0)
plot(merged)

# crop raster by extent of shapefile
cropped <- crop(x = merged, y = extent(adm0))
names(cropped)
names(cropped) <- 'nighttime_2012'
plot(cropped)

# mask cropped raster onto administrative division shapefile
masked <- mask(x = cropped, mask = adm2)
plot(masked)

# save cropped raster 
#writeRaster(r, filename="Data\\GIS\\2012\\nighttime_2012.tif", format="GTiff", overwrite=TRUE)   # path for windows
#writeRaster(r, filename="Data/GIS/2012/nighttime_2012.tif", format="GTiff", overwrite=TRUE)   # path for mac



# ---------- extracting luminosity data per administrative division (2012) ---------- # 

# load second administrative division shapefile
adm2_path <- "Data\\GIS\\Shape\\COUNTRY_adm\\COUNTRY_adm2.shp"
if (OS == "unix"){
  adm2_path <- str_replace_all(adm2_path, '\\\\', '/\\')     # file path adjustment for Macs
}
adm2 <- readOGR(adm2_path)   

# simplify object to make plotting quicker
object.size(adm2)
adm2 <- rmapshaper::ms_simplify(adm2)  
object.size(adm2)
plot(adm2)


# polygonize raster file of night lights 
## can rename the existing file, or load the saved file
lights <- masked
#lights <- raster("Data\\GIS\\2012\\nighttime_2012.tif")  # path for windows
#lights <- raster("Data/GIS/2012/nighttime_2012.tif")  # path for mac

plot(lights)  # this may be a very poor visualization, that's OK; we just want to make sure the raster loads
cellStats(lights, 'min')  # note that there are some negative values that will need to be 'rounded up' to zero 


pol <- polygonize(lights, na.rm = TRUE)
hist(pol$nighttime_2012)
min(pol$nighttime_2012)  # negative values that will need to be 'rounded up' to zero 
pol$nighttime_2012[pol$nighttime_2012 < 0] <- 0


ggplot() +
  geom_sf(data = pol)    


# check crs to ensure they match
st_crs(pol)
st_crs(adm2)
crs <- st_crs(adm2)

NDCs <- st_as_sf(adm2, coords = c("longitude", "latitude"), crs = crs)  # make NDC shapefile df format

ggplot() +
  geom_sf(data = NDCs) 

#join the two files
joined <- st_join(NDCs, pol, join = st_intersects)
table(joined$ID_2, exclude = NULL)  # note that if you cropped the raster there should be few to no NAs; 
                                    # if you didn't crop the raster, there may be many NAs

grouped <- joined %>%
  group_by(ID_2) %>%
  mutate(lt_by_adm2 = mean(nighttime_2012, na.rm = TRUE))

table(grouped$ID_2, exclude = NULL)

per_adm <- grouped[!duplicated(grouped$ID_2),]
per_adm <- per_adm[!(is.na(per_adm$ID_2)),]     # drop NA row if necessary 

per_adm$area <- st_area(per_adm$geometry/1000000)   # area in 1000s of kmˆ2

# plot 
ggplot() +
  geom_sf(data = per_adm)

ggplot() +
  geom_sf(data = per_adm, color='white', aes(fill=lt_by_adm2), lwd = 0.3) + 
  theme_void()



# ---------- export tabular data ---------- # 

# rename and drop the geometry tab as it won't hold up in the csv data structure
light_2012 <- per_adm
light_2012$geometry <- NULL

light2012_path <- "Data\\interrim_data_files\\nighttime_2012_per_adm.csv"
if (OS == "unix"){
  light2012_path <- str_replace_all(light2012_path, '\\\\', '/\\')     # file path adjustment for Macs
}
write.csv(light_2012, file = light2012_path, row.names=FALSE)   




############ 2019 satellite imagery ############

# ---------- load satellite images (raster files) (2019) ---------- # 

tile1_path <- "Data\\GIS\\SVDNB_npp_20190401-20190430_75N060W_vcmslcfg_v10_c201905191000.avg_rade9h.tif"
if (OS == "unix"){
  tile1_path <- str_replace_all(tile1_path, '\\\\', '/\\')     # file path adjustment for Macs
}
tile1 <- raster(tile1_path)
tile1@crs
col=hcl.colors(5, palette = "viridis")
image(tile1, col=col)  # it is very hard to perceive the lights in this visualization, this is normal. 

tile2_path <- "Data\\GIS\\SVDNB_npp_20190401-20190430_75N180W_vcmslcfg_v10_c201905191000.avg_rade9h.tif"
if (OS == "unix"){     # file path adjustment for Macs
  tile2_path <- str_replace_all(tile2_path, '\\\\', '/\\')
}
tile2 <- raster(tile2_path)
tile2@crs
image(tile2, col=col)



# ---------- merge raster files (2019) ---------- # 

# working with this file will be computationally expensive (likely to take some time); 
# this is why we will crop it in the next steps. However some adjustments below might 
# help things move more quickly in the interrim. 

rasterOptions()
rasterOptions(maxmemory = 1e+09)

merged <- merge(tile1, tile2)  
merged@crs


# ---------- clip merged raster (2019) ---------- # 

# check that CRS match in both raster and shape files
merged@crs
st_crs(adm0)

# plot both raster and shape files
plot(adm0)
plot(merged)

# crop raster by extent of shapefile
cropped <- crop(x = merged, y = extent(adm0))
names(cropped)
names(cropped) <- 'nighttime_2019'
plot(cropped)

# save cropped raster as .tif
#writeRaster(r, filename="Data\\GIS\\2012\\nighttime_2019.tif", format="GTiff", overwrite=TRUE)   # path for windows
#writeRaster(r, filename="Data/GIS/2012nighttime_2019.tif", format="GTiff", overwrite=TRUE)   # path for mac

# mask cropped raster onto administrative division shapefile
masked <- mask(x = cropped, mask = adm2)
plot(masked)



# ---------- extracting luminosity data per administrative division (2019) ---------- # 

# simplify object to make plotting quicker
object.size(adm2)
adm2 <- rmapshaper::ms_simplify(adm2)  
object.size(adm2)
plot(adm2)

# polygonize raster file of night lights 
## can rename the existing file, or load the saved file
lights <- masked
#lights <- raster("Data\\GIS\\2019\\nighttime_2019.tif")  # path for windows
#lights <- raster("Data/GIS/2019/nighttime_2019.tif")  # path for mac

plot(lights)  # this may be a very poor visualization, that's OK; we just want to make sure the raster loaded correctly
cellStats(lights, 'min')  # note that there are some negative values that will need to be 'rounded up' to zero 


pol <- polygonize(lights, na.rm = TRUE)
hist(pol$nighttime_2019)
min(pol$nighttime_2019)  # negative values that will need to be 'rounded up' to zero 
pol$nighttime_2019[pol$nighttime_2019 < 0] <- 0


ggplot() +
  geom_sf(data = pol)    


# check crs to ensure they match
st_crs(pol)
st_crs(adm2)
crs <- st_crs(adm2)

NDCs <- st_as_sf(adm2, coords = c("longitude", "latitude"), crs = crs)  # make NDC shapefile df format

ggplot() +
  geom_sf(data = NDCs) 

#join the two files
joined <- st_join(NDCs, pol, join = st_intersects)
table(joined$ID_2, exclude = NULL)  # note that if you cropped the raster there should be few to no NAs; 
                                    # if you didn't crop the raster, there may be many NAs

grouped <- joined %>%
  group_by(ID_2) %>%
  mutate(lt_by_adm2 = mean(nighttime_2019, na.rm = TRUE))

table(grouped$ID_2, exclude = NULL)

per_adm <- grouped[!duplicated(grouped$ID_2),]
per_adm <- per_adm[!(is.na(per_adm$ID_2)),]     # drop NA row if necessary 

per_adm$area <- st_area(per_adm$geometry/1000000)   # area in 1000s of kmˆ2

# plot 
ggplot() +
  geom_sf(data = per_adm)

ggplot() +
  geom_sf(data = per_adm, color='white', aes(fill=lt_by_adm2), lwd = 0.3) + 
  theme_void()



# ---------- export tabular data ---------- # 

# rename and drop the geometry tab as it won't hold up in the csv data structure
light_2019 <- per_adm
light_2019$geometry <- NULL

light2019_path <- "Data\\interrim_data_files\\nighttime_2019_per_adm.csv"
if (OS == "unix"){     # file path adjustment for Macs
  light2019_path <- str_replace_all(light2019_path, '\\\\', '/\\')
}
write.csv(light_2019, file = light2019_path, row.names=FALSE)   



print('')
print('finished running script 4: Luminosity')
print('')





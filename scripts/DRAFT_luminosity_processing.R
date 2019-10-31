
### Housing Deficit Estimation: ***DRAFT*** luminosity data processing 

## Author:   Jordan Jasuta Fischer
##           for the Housing and Urban Development Division of the Inter-American Development Bank

# This script should process geotiff files (.tif) that can be downloaded from NASAS's VIIRS website
# (https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html). In this case, tiles 1 and 2 were used
# for each 2012 and 2019. This script will cover the following steps to extract nocturnal luminosity data:
#    1) Merging raster files
#    2) clipping the merged file to the area of study
#    3) Polygonizing (vectorizing) luminosity data within the clipped area
#    4) joining the vectorized data with administrative division data (spatial join)
#    5) exporting the data in tabular form
#
# HOWEVER, this draft script is INCOMPLETE, HAS NOT BEEN VERIFIED, and may contain errors or bugs! 
# For best results, use the QGIS methodology explained in section IV of the Methodology and Guide 
# document. 




# ---------- load satellite images (raster files) ---------- # 

if (!require("raster")) install.packages("raster") 
library(raster)
if (!require("rgdal")) install.packages("rgdal") 
library(rgdal)
library(sp)

# set working directory
setwd("C:\\Users\\jfisher\\Documents\\PROJECTS\\HousingDef")

tile1 <- raster("GIS\\SVDNB_npp_20121101-20121130_75N060W_vcmcfg_v10_c201601270845.avg_rade9h.tif")
tile1@crs
col=hcl.colors(5, palette = "viridis")
image(tile1, col=col)  # note that it is very hard to perceive the lights in this visualization, this is normal. 

tile2 <- raster("GIS\\SVDNB_npp_20121101-20121130_75N180W_vcmcfg_v10_c201601270845.avg_rade9h.tif")
tile2@crs
image(tile2, col=col)


# ---------- merge raster files ---------- # 

# working with this file will be computationally expensive (likely to take some time); 
# this is why we will crop it in the next steps. However some adjustments below might 
# help things move more quickly in the interrim. 

rasterOptions()
rasterOptions(maxmemory = 1e+09)

merged <- merge(tile1, tile2)  
merged@crs


# ---------- clip merged raster ---------- # 

# load country shapefile
adm0 <- readOGR("Data\\GIS\\Shape\\GUY_adm\\GUY_adm0.shp")

# check that CRS match in both raster and shape files
merged@crs
st_crs(adm0)

# plot both raster and shape files
plot(adm0)
plot(merged)

# crop raster by extent of shapefile
cropped <- crop(x = merged, y = extent(adm0))
plot(cropped)

# mask cropped raster onto administrative division shapefile
masked <- mask(x = cropped, mask = adm2)
plot(masked)



# ---------- extracting luminosity data per administrative division ---------- # 

if (!require("maptools")) install.packages("maptools") 
library(maptools)

# load second administrative division shapefile
adm2 <- readOGR("Data\\GIS\\Shape\\GUY_adm\\GUY_adm2.shp")
plot(adm2)

# Extract raster values to list object
GY_light_vals <- raster::extract(test, adm2)   ### HOW TO KNOW THAT THIS KEEPS THE CORRECT ORDER/ETC?


# Use list apply to calculate mean for each administrative division
GY_mean_light <- lapply(GY_light_vals, FUN=mean)


light_by_adm2 <- spCbind(adm2, GY_mean_light)   ### NO CLEAR PATTERN EMERGES - suspicious
plot(light_by_adm2, col=col)


### need to make GY_light_vals a feature of adm2 

adm2$light <- mean(extract(cropped, adm2))
cropped$layer
cropped@crs

test <- raster("Data\\GIS\\2012 Lights\\2012\\nighttime_2012_11_GY.tif")







# ---------- export tabular data ---------- # 






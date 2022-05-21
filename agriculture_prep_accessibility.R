
# ------------- PREPARE WORKSPACE -------------
setwd("C:/Users/grace/GIS/povertyequity/chad")
wd <- getwd()

library(ursa)
library(raster)
library(sp)
library(rgdal)
library(sf)
library(dplyr)
library(rgeos)
library(naniar) # replace_with_na()
library(tidyverse) # ggplot2()
library(tidyr) # complete()
library(units)
# devtools:::install_github("gearslaboratory/gdalUtils")
library(gdalUtils)


# ------------- MOSAIC AND CLIP LANDCOVER -------------
# Tutorial: https://tengkengvang.com/2018/11/12/mosaic-or-merge-rasters-in-r/
landcover <- c('GLCLU_2019_strata_10N_010E.tif', 'GLCLU_2019_strata_10N_020E.tif', 
               'GLCLU_2019_strata_20N_010E.tif', 'GLCLU_2019_strata_20N_020E.tif', 
               'GLCLU_2019_strata_30N_010E.tif', 'GLCLU_2019_strata_30N_020E.tif')
e = extent(13, 25, 7, 24) # Approximate extents of Chad
template = raster(e) # Create empty raster to add actual data onto
proj4string(template) <- CRS('+init=epsg:4326')
writeRaster(template, file="TCD_landcover.tif", format="GTiff", overwrite=TRUE)
mosaic_rasters(gdalfile=landcover, dst_dataset="TCD_landcover.tif", of="GTiff")
gdalinfo("TCD_landcover.tif") # Check contents.

# Load raster and AOI extent as objects for clip.
chad = readOGR("TCD_bbox.shp")
landcover = raster("TCD_landcover.tif")
plot(landcover, main = "Original mosaic")
plot(chad, add=TRUE)
landcover <- crop(landcover, chad)
plot(landcover, main = "Cropped to Chad bounding box")
plot(chad, add=TRUE)

writeRaster(landcover, file="TCD_landcover.tif", format="GTiff", overwrite=TRUE)



# ------------- SUBSET TO JUST CROPLAND VALUE OF LANDCOVER BAND -------------
cropland = clamp(landcover, 17, 17, useValues=FALSE) # Set all non-cropland values to null. (values below and above 17)
cropland = reclassify(cropland, c(16,18,1)) # Reclassify all values between 16-18 to 1.
writeRaster(cropland, file="TCD_cropland.tif", format="GTiff", overwrite=TRUE)


# ------------- GET CROPLAND CELL COUNT AT AGRO'S RESOLUTION (TO GET AGRO VALUE PER CROP CELL) -------------
# Get details of each dataset to:
# Determine by what factor to aggregate cropland.
# Determine which has the larger extent. Use extend() if agro is larger, or crop() if cropland is larger.
gdalinfo("TCD_spam17sum.tif")
gdalinfo("TCD_cropland.tif")
agro = raster("TCD_spam17sum.tif")
cropland = raster("TCD_cropland.tif")

# Add up # cells at same resolution as agro (9km / 30m = 333.33)
cropland_9km = raster::aggregate(cropland, fact=333, fun=sum, na.rm=TRUE)

# Give the same extent by adding NA rows/cols wherever needed (if extending) or removing rows/cols (if cropping)
cropland_9km = raster::extend(cropland_9km, agro) 
plot(cropland_9km)
plot(agro, add=TRUE)

agro_aligned = resample(agro, cropland_9km, method="bilinear")
plot(agro_aligned, add=TRUE)

# The input Raster* objects should have the same extent, origin and resolution. 
# If only the extent differs, the computation will continue for the intersection of the Raster objects.
agro_per_cropcell = agro_aligned / cropland_9km
plot(agro_per_cropcell)



# ------------- GET PROPORTIONAL AGRO AMOUNT AT CROPLAND'S RESOLUTION (TO ASSIGN AGRO VALUE TO CROP CELL) -------------
# Split agro cells to resolution of cropland, keeping same value as its parent cell without arithmetic.
agro_30m = raster::disaggregate(agro_per_cropcell, fact=333, method='', na.rm=TRUE)

agro_30m = raster::extend(agro_30m, cropland)
agro_aligned = resample(agro_30m, cropland, method="bilinear")

val_at_cropcells = agro_aligned * cropland

writeRaster(val_at_cropcells, file="TCD_AgroVal_byCroplandCell.tif", format="GTiff", overwrite=TRUE)


# ------------- CROPLAND VALUES TO POINT FOR USE IN TIME-COST RASTER -------------
crop_pt = rasterToPoints(val_at_cropcells, fun=NULL, spatial=TRUE)

shapefile(crop_pt, "TCD_AgroVal_byCroplandCell.shp", overwrite=TRUE)



# ------------- AGRICULTURAL VALUES TO POLYGON -------------
agro = raster("spam2017v2r1_V_sum1.tif")
agro = crop(agro, chad)
writeRaster(agro, file="TCD_spam17sum.tif", format="GTiff", overwrite=TRUE)

agro_ply = rasterToPolygons(agro, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

plot(agro_ply)

shapefile(agro_ply, "TCD_spam17sum.shp", overwrite=TRUE)



# ------------- COUNT CROP CELLS IN EACH AGRO CELL (NOW POLYGON) -------------
cropland = raster("TCD_cropland.tif")
agro = raster("TCD_spam17sum.tif")
agro_ply = readOGR("TCD_spam17sum.shp")

#agro_ply$crop_sum = raster::extract(cropland, agro_ply, fun=sum, sp=TRUE) # Add raster:: to start to avoid conflict with other packages that have extract() function.

shapefile(agro_ply, "TCD_spam17sum_cropct.shp", overwrite=TRUE)



# ------------- CROPLAND VALUES TO POLYGON -------------
crop_ply = rasterToPolygons(cropland)
#crop_pt = as.data.frame(crop_pt)

#crop_pt$index = 1:nrow(crop_pt)
#crop_pt = rename(crop_pt, crop_ID = index)

#write.csv(crop_pt, file = "TCD_cropland_pt.csv")
shapefile(crop_ply, "TCD_cropland_ply.shp", overwrite=TRUE)






# ------------- JOIN AGRO VALUES and ID'S TO CROPLAND POINTS -------------
# Was having processing trouble, so switched to QGIS to lighten the load a bit.
# In QGIS, re-projected crop_pt and agro to Africa Albers.
chad = readOGR("TCD_ADM0_10km.shp")
crop_pt = readOGR("TCD_cropland_pt_albers.shp")
agro = readOGR("TCD_spam17sum_albers.shp")



agro = st_read("TCD_spam17sum.shp") # Re-read as sf data type to be compatible with st_join
crop_pt = st_read("TCD_cropland_pt.shp")
agro_crop = st_join(crop_pt, agro, join=st_within, left=T)
st_write(agro_crop, "TCD_crop_agrovalues.shp", driver="ESRI Shapefile", append=FALSE)




# ==============================================
# OS Shapefiles 
# add information on lon, lat, volume and area to attributes
# for regressions or subsetting later

setwd("~/Dropbox (Cambridge University)/Cambridge/data/shapefiles/")
# 27700 epsg
# load libraries
library(rgdal)
library(rgeos)
library(raster)

# load shapefile
s <- shapefile("os_cambridge_buildings.shp")

# load raster of building volumes
r <- raster("DEM Cambridge Volumetric Map.tif")

s$volume3D <- NA
s$area2D <- NA

s$centrX <- NA
s$centrY <- NA

print( dim(s) )

# save centroid coordinates for each polygon....
for(i in 1:nrow(s)){
  s[i, c("centrX","centrY")]<- bbox(gCentroid(s[i,]))[,1]
  if(i %% 1000 == 0)  {
    print(i)
  }
}

# loop through each polygon in shape file, calculate area and volume of each building
for(i in 1:nrow(s)){
  # 2D area 
  s$area2D[i] <- gArea(s[i,])
  # 3D, total volume
  s$volume3D[i] <- sum( unlist( extract(r, s[i,]) ), na.rm=TRUE)
  if(i %% 100 == 0)  {
    print(i)
  }
}

# save everything to new shape file
writeOGR(s, "buildings_with_centroids_area_volume.shp", driver="ESRI Shapefile", layer="buildingswithdens")

# Do we have a mugshot from streetview?

pics <- as.data.frame( list.files("../images/cambridge/") )
colnames(pics) <- "filename"
pics$TOID <- gsub("_.*$","",pics$filename, perl=TRUE)
pics$PanoID <- gsub("^[^_]*_(.*)\\....$","\\1",pics$filename, perl=TRUE)

ss <- merge(s, pics, by="TOID", all.x=TRUE)
ss$mugshot <- "N"
ss$mugshot[!is.na(ss$PanoID)] <- "Y"
ss$mugshot[ss$PanoID =="nopanorama"] <- "nopanorama"

# save everything to new shape file
writeOGR(ss, "buildings_with_centroids_area_volume_panoid.shp", driver="ESRI Shapefile", layer="buildingswithdens")


# ss <- shapefile("buildings_with_centroids_area_volume_panoid.shp")

# Add classification scores
scores <- read.csv("https://www.dropbox.com/s/f2m77qexdzhywtk/scores_toid.csv?dl=1", as.is=TRUE, colClasses=  c(rep("numeric",9),rep("character", 2)))

sss <- merge(ss, scores, by="TOID", all.x=TRUE)
writeOGR(sss, "buildings_with_centroids_area_volume_panoid_era.shp", driver="ESRI Shapefile", layer="buildingswithdens")







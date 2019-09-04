# =========================================================
#  Merge sales data with additional data
#   - TOID, to link to OS maps
#   - Volume and footprind area
#   - Estimated style 
#   - Groundtruth style
#   - distance to city centre
# =========================================================

# Load additional libraries
library(stargazer)
library(spdep)
library(raster)
library(wesanderson)
library(sp)
library(rgeos)
library(knitr)
library(xtable)
library(stringr)
library(stringdist)
library(data.table)

source("./settings.R")

sales <- read.csv("https://www.dropbox.com/s/mlybx4io84us0n6/sales_cambridge-2018-10.csv?dl=1", as.is=TRUE, header=FALSE)
colnames(sales) <- c("id","price","date","postcode","propertytype","new","estatetype","paon","saon","street","locality","town","district","county","PPDcategory","recordstatus")
sales$date <- strptime(sales$date,"%Y-%m-%d")
# create time dummy variables, monthly and yearly 
sales$month <- format(sales$date, format="%Y-%m" )
sales$year <- as.factor(format(sales$date, format="%Y"))


l.price <- list()
l.price$upper <- 5000000
l.price$lower <- 10000


# Match addresses to TOID using AddressBase
ab <- read.csv("~/Dropbox (Cambridge University)/Cambridge/data/addresses/OsAddressBase/ID28_DPA_Records.csv", as.is=TRUE)
ab <- subset(ab, select=c("UPRN","POSTCODE","BUILDING_NUMBER","SUB_BUILDING_NAME","BUILDING_NAME","THOROUGHFARE"))
ab$paon <- ab$BUILDING_NUMBER
ab$paon[ is.na( ab$BUILDING_NUMBER) ] <- ab$BUILDING_NAME[ is.na( ab$BUILDING_NUMBER) ]
ab$saon <- ab$SUB_BUILDING_NAME

abxref <- read.csv("~/Dropbox (Cambridge University)/Cambridge/data/addresses/OsAddressBase/ID23_XREF_Records.csv", as.is=TRUE)
abxref <- subset(abxref, grepl('osgb', CROSS_REFERENCE))
abxref <- subset(abxref, select=c("UPRN","CROSS_REFERENCE"))
abxref$TOID <- gsub("osgb","",abxref$CROSS_REFERENCE)
shp <- shapefile("~/Dropbox (Cambridge University)/Cambridge/data/shapefiles/os_cambridge_buildings.shp")
abxref <- subset(abxref, TOID %in% as.character( as.numeric(shp$TOID)))
abx <- merge(ab, abxref, by="UPRN")
abx$postcode <- abx$POSTCODE
abx$street <- abx$THOROUGHFARE
abx$addr_str <- paste(abx$saon, abx$paon, abx$street)
abx$addr_str <- gsub('  ',' ',abx$addr_str, perl=TRUE)
abx$addr_str <- gsub('^ ','',abx$addr_str, perl=TRUE)
abx$addr_str <- gsub(' $','',abx$addr_str, perl=TRUE)
abx <- subset(abx, select=c('addr_str','postcode','TOID'))

sales$addr_str <- paste(sales$saon, sales$paon, sales$street)
sales$addr_str <- gsub('  ',' ',sales$addr_str, perl=TRUE)
sales$addr_str <- gsub('^ ','',sales$addr_str, perl=TRUE)
sales$addr_str <- gsub(' $','',sales$addr_str, perl=TRUE)
s <- merge(sales, abx, by=c("postcode","addr_str"), all.x=TRUE)


# map the new homes in Cambridge
#newTOID <- as.numeric( s$TOID[s$new == 'Y' & !is.na(s$TOID)])
#shpnew <- subset(shp, as.numeric(TOID) %in% newTOID)
#plot(shp, col="grey", border=NA)
#plot(shpnew, col="red", add=TRUE, border="red")


# Merge with classifications
scores <- read.csv("https://www.dropbox.com/s/f2m77qexdzhywtk/scores_gsv_toid.csv?dl=1", as.is=TRUE, colClasses=  c(rep("numeric",10),rep("character", 2)))
scores$TOID <- as.character(as.numeric(scores$TOID))
s <- merge(s, scores, by="TOID", all.x=TRUE)


# merge with area and volume information
volumes <- read.csv("https://www.dropbox.com/s/hwum1l02b6enba8/toid_area_volume.csv?dl=1", as.is=TRUE, colClasses=c("character",rep("numeric",4)))
volumes$TOID <- as.character(as.numeric(volumes$TOID))
s <- merge(s, volumes, by="TOID", all.x=TRUE)


# load true eras from training data
groundtruth <- read.csv("https://www.dropbox.com/s/04wwhdaqtuqzadt/groundtruth.csv?dl=1", as.is=TRUE, colClasses=rep("character",2))
#groundtruth <- read.csv("~/db/Cambridge/data/derived/groundtruth.csv", as.is=TRUE)
groundtruth$TOID <- as.character(as.numeric(groundtruth$TOID))
groundtruth <- subset(groundtruth, !is.na(TOID) & true_era %in% eras)
s <- merge(s, groundtruth, by="TOID", all.x=TRUE)


# calculate distance to city cetre to control for e.g. a time variant preference for central locations
st.marys.coords <- c(544827.9, 258458.1)
s$dist <- apply( s[,c("centrX","centrY")], 1, function(x, center=st.marys.coords){ 
  if( is.na( x[1] ) ){
    return( NA )} 
  else {
    return( sum((x-center)^2)^0.5  )
  }  } )


# # model with yearly time effects
s$era <- relevel(factor(s$era), "b victorian")
s$true_era <- relevel(factor(s$true_era), "b victorian")

## WRITE DATA 
write.csv(s, file="~/Dropbox (Cambridge University)/Cambridge/data/derived/sales_toids_eras.csv", row.names=FALSE)
###


# ======================= WHICH LSOA does the buidling belong to?
# in case a building is on the boundary, pick the one with the bigger overlap
library('RPostgreSQL')
con <- dbConnect(PostgreSQL(), user= "thies", dbname="gis", password='gis', port=5432)
toid_lsoa <- dbGetQuery(con, "SELECT l.lsoa11cd, b.toid, ST_Area(ST_Intersection(l.geom, b.geom))  FROM cambridge_lsoa11 as l, cbg_os_buildings as b WHERE ST_Intersects(l.geom, b.geom)")
toid_lsoa <- toid_lsoa[rev(order(toid_lsoa$toid, toid_lsoa$st_area)) , ]
toid_lsoa$drop <- FALSE
toid_lsoa$drop[2:nrow(toid_lsoa)] <-toid_lsoa$toid[1:(nrow(toid_lsoa)-1)] == toid_lsoa$toid[2:nrow(toid_lsoa)]
toid_lsoa <- subset(toid_lsoa, drop == FALSE)
toid_lsoa$drop <- NULL
toid_lsoa$st_area <- NULL
write.csv(toid_lsoa, file="~/db/Cambridge/data/derived/lsoa_x_toid.csv", row.names=FALSE)
# =============================================================================




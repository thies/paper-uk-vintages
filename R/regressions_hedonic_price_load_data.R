# Hedonic regression of era

library(stargazer)
library(sandwich)
library(lmtest)
library(xtable)
library(xtable)


# load data, in case it needs to be updated: source(augment_sales_data.R)
s <- read.csv("https://www.dropbox.com/s/4sdxm6x5lfhesrp/sales_toids_eras.csv?dl=1")
s$date <- as.Date(s$date)
s$year <- format(s$date, format="%Y")

# load lsoa ids, for spatial controls
lsoa <- read.csv("https://www.dropbox.com/s/8wfw8tdq2sgxza8/lsoa_x_toid.csv?dl=1", as.is=TRUE)
colnames(lsoa) <- c("lsoaid","TOID" )

# merge back to sales
s <- merge(s, lsoa, by="TOID", all.x=TRUE)


# load ground truth
groundtruth <- read.csv("https://www.dropbox.com/s/04wwhdaqtuqzadt/groundtruth.csv?dl=1", as.is=TRUE)
s$true_era <- NULL
s <- merge(s, groundtruth, by="TOID", all.x = TRUE)


#####################################
# add classification based on 
# - Google Streetview
# - Map classification
scores <- read.csv("https://www.dropbox.com/s/f2m77qexdzhywtk/scores_gsv_toid.csv?dl=1", as.is=TRUE)
scores2D <- read.csv("~/db/Cambridge/data/derived/scores2D_toid.csv", as.is=TRUE)
scorecols <- c("a.georgian","b.victorian","c.edwardian.late.victorian","d.interwar","e.postwar","f.contemporary","g.cont.victorian")
colnames(scores2D) <- paste("sc2D", colnames(scores2D), sep="")
scores2D$TOID <- scores2D$sc2DTOID
scores2D$sc2DTOID <- NULL
colnames(scores) <- paste("gsv", colnames(scores), sep="")
scores$TOID <- scores$gsvTOID
scores$gsvTOID <- NULL

sco <- merge(scores, scores2D, by="TOID", all.x=TRUE)
# calculate average scores, 
# classify buildings based on the combined score
p <- 0.5
####
for(c in scorecols){
  sco[ , c ] <- sco[, paste("gsv", c, sep="")]*p + sco[,paste("sc2D", c, sep="")]*(1-p)
  sco[is.na( sco[,paste("sc2D", c, sep="")] ) , c ] <- sco[ is.na( sco[,paste("sc2D", c, sep="")] ) , paste("gsv", c, sep="")]
}
sco$max <- apply( sco[, scorecols], 1, max, na.rm=TRUE)
sco$era <- NA
for(c in scorecols){
  sco$era[ sco$max == sco[,c]] <- c     
}
sco$era <- gsub("\\."," ",sco$era)
for(c in c(scorecols,"max","era")){
 try( s[,c] <- NULL )
}
s <- merge(s, sco, by="TOID", all.x=TRUE)
################################



# exclude 
# - apartments, 
# - leaseholds,
# - buildings that cannot be linked to a polygon via TOID 
regsample <- subset(s, propertytype != "F" & !is.na(TOID) & estatetype =="F")
regsample$era <- relevel(factor( regsample$era), "e postwar")

# interaction terms: era vs. style of neighbours
# load coordinates, addresses and styles of ALL buildings, not just sales
scores <- read.csv("https://www.dropbox.com/s/f2m77qexdzhywtk/scores_gsv_toid.csv?dl=1", as.is=TRUE, colClasses=  c(rep("numeric",10),rep("character", 2)))
scores$TOID <- as.character(as.numeric(scores$TOID))
eras <- scores
coordinates <- read.csv("https://www.dropbox.com/s/hwum1l02b6enba8/toid_area_volume.csv?dl=1", as.is=TRUE)
environ <- merge(coordinates, eras, by="TOID")
streetnames <- read.csv("https://www.dropbox.com/s/ibrsntc5je43pbe/shape.address.csv?dl=1", as.is=TRUE)[, c("TOID","POSTCODE","THOROUGHFARE")]
environ <- merge(environ, streetnames, by="TOID")

NeighEra <- function(toid, neighdist=100, environ, coordinates, tmp_eras){
  x <- coordinates$centrX[coordinates$TOID == toid]
  y <- coordinates$centrY[coordinates$TOID == toid]
  street <- regsample$street[regsample$TOID == toid]
  # select all buildings at same street (exclude the building at hand)
  tmp <- subset(environ, THOROUGHFARE == street & TOID != toid)
  # select all within cutoff dist
  tmp <- unique( subset(tmp, ((centrX - x)^2 + (centrY - y)^2)^0.5 <= neighdist ))
  tmp_n <- nrow(tmp)
  names(tmp_n) <- "n"
  if( tmp_n == 0 ){
    return(NA)
  } else {
    tmp_means <- colMeans(tmp[, tmp_eras], na.rm=TRUE)
    tmp_max <- max(tmp_means)
    names(tmp_max) <- "max"
    tmp_max_name <- tmp_eras[ tmp_means == tmp_max] 
    names(tmp_max_name) <- "eraneigh"
    tmp_herf <- sum( tmp_means^2, na.rm=TRUE)
    names(tmp_herf) <- "herf"
    
    # distance in scores vectors 
    # (in case there are scores for both the building and the neigh.)
    dist <- NA
    bldgScores <- unique(  regsample[regsample$TOID == toid, era_cols] )
    if( !is.na(bldgScores[1])){
      dist <- sum( (as.numeric( tmp_means) -  bldgScores) ^2)^0.5
    }
    names(dist) <- "scoredist"
    # return
    return( c(tmp_means, tmp_max, tmp_max_name, tmp_herf, tmp_n, dist) ) 
  }
}
era_cols <- c("a.georgian","b.victorian","c.edwardian.late.victorian","d.interwar","e.postwar","f.contemporary","g.cont.victorian")
neigh <- lapply(regsample$TOID, NeighEra, environ=environ, coordinates=coordinates, tmp_eras = era_cols)
neigh <- as.data.frame( do.call(rbind, neigh), stringsAsFactors=FALSE)

regsample$scoredist <- as.numeric( neigh$scoredist )
regsample$neigh_herf <- as.numeric(neigh$herf)
regsample$eraneigh <- neigh$eraneigh
regsample$eraneigh <- gsub("\\."," ", regsample$eraneigh)
regsample$eraneigh <- relevel(factor(regsample$eraneigh), "e postwar")

nscores <- subset( neigh, select=era_cols)
colnames(nscores) <- paste("n_", era_cols)
nscores <- apply(nscores, 2, as.numeric)

regsample <- cbind(regsample, nscores)






# =====================================================
# Some ad-hoc cleansing 
# exclude extreme sales prices
# - 50,000 was low in 1995
# - 2,000,000 is high, even in 2017
regsample <- subset(regsample, price < 2000000 & price > 50000)
# exclude extreme footprints and volumes
regsample <- subset(regsample, area2D < 500)
regsample <- subset(regsample, volum3D < 1000)
# exclude trees and walls
regsample <- subset(regsample, ! era %in% c("x greenery","w walls") )
regsample$era <- levels(regsample$era)[regsample$era]




# ------- exclude those observations where the classification was not really confident
secondscore <- function( x, cols, n ){
  tmpscores <- x[ cols ]
  tmpscores <- tmpscores[ rev( order( tmpscores ) ) ]
  return( tmpscores[2] )   
}
scorecols <- c("a.georgian","b.victorian","c.edwardian.late.victorian","d.interwar","e.postwar","f.contemporary","g.cont.victorian")
scores$secondscore <- as.numeric( apply(scores, 1, secondscore, cols=scorecols) )
scores$scoreratio <- scores$max/scores$secondscore
scores$score_confident <- 0
scores$score_confident[ scores$scoreratio > quantile(scores$scoreratio, 0.05)] <- 1
try(regsample$score_confident <- NULL)
try(regsample$secondscore <- NULL)
try(regsample$scoreratio <- NULL)
sc <- subset( scores, select=c("secondscore", "scoreratio", "score_confident","TOID" ))
regsample <- merge(regsample, sc, by="TOID")
#regsample <- subset(regsample, score_confident==1)
try(regsample$score_confident <- NULL)
#===================



write.csv(regsample, "~/db/Cambridge/data/derived/regsamples.csv", row.names=FALSE)

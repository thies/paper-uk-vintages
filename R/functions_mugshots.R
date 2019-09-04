#============ Some Helpers
# Create a set of 360 points, evenly distributed around the origin
# We can shift them later to the position of any panorama
createEndpoints <- function( radius=50, numpoints=360){
  endpoints <- list()
  # first point, just go north
  endpoints[[1]] <- c(0, radius)
  for(a in 1:(numpoints-1)){
    ang <- (2* pi) * (a / numpoints)
    endpoints[[ a+1 ]] <- c( endpoints[[1]][1] *cos(ang) - endpoints[[1]][2] *sin(ang),  endpoints[[1]][2] *cos(ang) + endpoints[[1]][1] *sin(ang), ang )
  }
  # 
  ep <- rbindlist(lapply(endpoints, function(x) data.table(lon=x[1], lat=x[2], rad=x[3])))
  return(ep)
}
# find the first polygon that intersects with a direct line from pano
firstHit <- function(line, pano, poly){
  hits <- list()
  for(p in 1:nrow(poly)){
    if(gIntersects(line, poly[p,])){
      chopped <- gIntersection(line, poly[p,])
   #   plot(chopped, add=TRUE, col="green", lwd=3)
      poly.id <- as.character( as.data.frame(poly[p,])$TOID )
      hits[[ poly.id ]] <- c( poly.id, gDistance(pano, chopped))
    }
  }
  if(length(hits) > 0){
    h <- as.data.frame( do.call(rbind, hits), stringsAsFactors=FALSE)
    colnames(h)<- c("id","dist")
    h$dist <- as.numeric(h$dist)
    rownames(h) <- NULL
    h <- h[order(h$dist), ]
    return(h[1, ])
  } else {
    return(c(NA, NA))
  }
}
firstHitRecursive <- function( d, pano, poly ){
  fh[[ d ]] <- firstHit(los[[d]], pano, poly)
  if(!is.na( fh[[ d ]][1] )){
    if(d>1){
      fh[[ (d-1) ]] <- fhfirstHitRecursive( (d-1), pano, poly )
    }
    if(d<360){
      fh[[ d+1 ]] <- firstHitRecursive( (d+1), pano, poly )
    }
   return(TRUE)
  }
  return(FALSE)
}
coords2points <- function(DT, l.p4string){
  # Input file:
  ## DT: data.table with lat, long
  # Output file:
  ## spatialPointsDataFrame
  coords <- cbind(Longitude = as.numeric(as.character(DT$location.lng)),
                  Latitude = as.numeric(as.character(DT$location.lat)))
  
  Location.pts <- SpatialPointsDataFrame(coords, dplyr::select(DT, -location.lat, -location.lng),
                                         proj4string = CRS(l.p4string$WGS84))
  shp.pts <- spTransform(Location.pts, CRS(l.p4string$UK))
  return(shp.pts)
}
getPanorama <- function( x, api.key, savePano=TRUE ){
  # find panorama that is closest to this centroid
  api.url <- paste("https://maps.googleapis.com/maps/api/streetview/metadata?size=600x300&location=", paste(rev(coordinates( x )), collapse=",") ,"&key=",api.key, sep="")
  panorama <- fromJSON(f=api.url)
  if(panorama$status  == 'OK'){
    pano.wgs84 <- readWKT(paste("POINT(",panorama$location$lng, panorama$location$lat,")"), p4s=l.p4string$WGS84)
    if( savePano ){
      exportJson <- toJSON( panorama )
      write(exportJson, paste(pano.dir,panorama$pano_id,".json", sep=""))
    }
  return( list( pano.wgs84, panorama ))
  } else {
    return(list(NA,panorama))
  }
}

# ================ End Helpers

# This is where stuff really happens 
getMugShot <- function(toid, s, plot=FALSE, fov.ratio=1, subset.radius=70, endpoints=NA, fov=NA,
                       fDest=NA, savePano=TRUE, api.key, cores=1, shp.panoIds=NULL, l.p4string){
  
  h <- subset(s, TOID == toid)
  if(is.na(endpoints[1,1])){
    endpoints <- createEndpoints(50, 360)
  }
  # find centroid of home
  centr <- gCentroid( h )
  centr.wgs84 <- spTransform(centr, CRS(l.p4string$WGS84))
  
  # subset polygons from close surroundings
  # this is just to speed up things...
  s_sub <- subset(s, abs(centrX-s$centrX[ s$TOID== toid]) < subset.radius & abs(centrY-s$centrY[ s$TOID== toid ]) < subset.radius ) 
  # Reproject s_sub to match the pano projection below
  s_sub <- spTransform(s_sub, CRS("+init=epsg:27700"))
  
  if(plot){
    plot(s_sub)
    plot(h, col="red", add=TRUE)
    points(centr, col="blue")
  }
  
  # find panorama that is closest to this centroid
  # Do this either based on explicit query of panoId query
  if (!(is.null(shp.panoIds))){
    shp.panoIds <- spTransform(shp.panoIds, CRS(l.))
    dt.dists <- data.table(pano_id = shp.panoIds@data$pano_id, dists = distGeo(centr.wgs84@coords, shp.panoIds@coords))
    dt.dists <- dt.dists[order(dists)]
    panoIds.near <- dt.dists$pano_id
    pano.id <- panoIds.near[1]
    coords.pano <- shp.panoIds[shp.panoIds@data$pano_id == pano.id,]@coords
    coords.target <- centr.wgs84@coords
    heading <- bearing(coords.pano, coords.target)
    # See if there is actually a picture
    api.metadata <- paste0("https://maps.googleapis.com/maps/api/streetview/metadata?size=640x600&pano=", pano.id, "&key=",api.key)
    panorama <- unlist(fromJSON(file=api.metadata))
    panorama <- data.table(t(panorama), TOID = toid)
    pano.loc <- paste0('~/Dropbox/panorama.db/',toid,'.alt.', pano.i, ".rds")
    saveRDS(panorama, file=pano.loc)
    if(panorama$status != 'OK') {
      # exit here if no panorama was found
      s.status <- paste0('_', str_replace_all(str_to_lower(panorama$status), ' ', ''), '.txt')
      fDest <- paste0(photo.dir, toid, s.status)
      # cat(panorama$status , file=fDest)
      return(fDest)
    }
    # Make consistent with the location based search
    pano.wgs84 <- readWKT(paste("POINT(",panorama$location.lng, panorama$location.lat,")"), p4s=l.p4string$WGS84)
    location <- list(lat=panorama$location.lat, lng=panorama$location.lng)
    panorama <- list(copyright = panorama$copyright, date=panorama$date, location=location, pano_id=panorama$pano_id, status=panorama$status)
    pano <- spTransform(pano.wgs84, CRS("+init=epsg:27700"))
  } else {
    # Use the centroid and find the nearest pano.id
    pan <- getPanorama( centr.wgs84, api.key, savePano )
    if( pan[[2]]$status != 'OK') {
      # exit here if no panorama was found
      s.status <- paste0('_', str_replace_all(str_to_lower(panorama$status), ' ', ''), '.txt')
      fDest <- paste0(photo.dir, toid, s.status)
      cat( pan[[2]]$status , file=fDest)
      return(fDest)
    }
    pano.wgs84 <- pan[[1]]
    panorama <- pan[[2]]
    pano <- spTransform(pano.wgs84, CRS("+init=epsg:27700"))
  }

  # Find out, which direction the camera should aim
  # How much of the building is visible from the pano position?
  # look in each of the 360 degree directions, and see if one can see the house unobstructed
  linesofsight <- endpoints
  linesofsight$lon <- linesofsight$lon+coordinates(pano)[1]
  linesofsight$lat <- linesofsight$lat+coordinates(pano)[2]
  losCoords <- paste(linesofsight$lon, linesofsight$lat)
  
  cores <- detectCores() - 1
  if (Sys.info()[which(names(Sys.info())=='user')]=='ejohnso4') cores <- 1
  #start.time <- Sys.time()
  los <- mclapply(losCoords, function(x,y,z){ readWKT(paste("LINESTRING(", paste( y, collapse=" ")," , ", x,")"), p4s=z) },
                  coordinates(pano), l.p4string$UK, mc.cores=cores)
  los <- mclapply(los, spTransform, CRS("+init=epsg:27700"), mc.cores=cores)
  
  # ==================== 
  # Trying to pre-scan which LOS might hit the home
  # - then move from there
  # - this should speed up things
  deg <- 1:360
  dseq <- unique( c( deg[deg %% 180 == 0], deg[deg %% 90 == 0], deg[deg %% 30 == 0], deg[deg %% 10 == 0], deg[deg %% 3 == 0], deg))
  fh <- as.list(rep(NA, length( los )))
  # implementing a primitive queue
  queue <- list()
  # and search for the first hit
  for( d in dseq){
    fh[[d]] <- firstHit( los[[ d ]], pano, s_sub )
    if(  ! is.na(fh[[d]][1] )){
      if( fh[[d]][1] == toid ){
        if(d>1){
          queue[[ length(queue)+1 ]] <- (d-1)
        } 
        if(d < 360){
          queue[[ length(queue)+1 ]] <- (d+1)
        } 
        done <- d
        break
      }
    }
  }
  # work through the queue
  while(length( queue ) >= 1){
    # take first element of queue
    qe <- queue[[1]]
    fh[[ qe ]] <- firstHit( los[[ qe ]], pano, s_sub )
    if( ! is.na(fh[[ qe ]][1] )){
      if( fh[[ qe ]][1] == toid ){
        if( qe > 1 & ( ! (qe-1) %in% done ) ){
          queue[[ length(queue)+1 ]] <- (qe -1 )
        } 
        if( qe < 360 & ( ! (qe+1) %in% done) ){
          queue[[ length(queue)+1 ]] <- (qe+1)
          } 
      }  
    }
    # remove from queue when done
    done <- c(done, qe)
    queue[[1]] <- NULL
  }
  
  # ============================
  
  # Former brute force solution
  #fh <- mclapply( los, firstHit, pano, s_sub, mc.cores = cores)
  

  fhdf <- as.data.frame( do.call(rbind, fh), stringsAsFactors=FALSE)
  fhdf$lon <- linesofsight$lon
  fhdf$lat <- linesofsight$lat
  #end.time <- Sys.time()
  #cat(end.time - start.time)
  
  # focus on lines of sight to the building
  goodangles <- subset(fhdf, id == toid)
  if(nrow(goodangles) > 0){
    # (crudely) assuming that all lines of sight are in one streak,
    # and each line represents one degree
    # then the overall fan should have approx. a max angle of nrow(goodangles) degrees
    # fov.ratio allows some scaling of the fan
    if(is.na(fov)){
      fov <- nrow(goodangles) * fov.ratio 
    } 
    # and the best bearing is the one in the middle (fingers crossed)
    goodangles.index <- median(1:nrow(goodangles))
    midLos <- readWKT( paste("POINT(",paste(goodangles[ goodangles.index , c("lon","lat")], collapse=" "),")", sep=""), p4s = l.p4string$UK)
    if(plot){
      midLosLine <- readWKT(paste("LINESTRING(", paste( coordinates(pano), collapse=" ")," , ",
                                  paste( coordinates(midLos), collapse=" ") ,")"), p4s=l.p4string$UK)
      plot(midLosLine, col="green", lwd=3, add=TRUE)
    }
    midLos.wgs84 <- spTransform(midLos, CRS("+init=epsg:4326"))
    #midLos.string <- paste(rev(coordinates(midLos.wgs84)), collapse=",")
    heading <- bearing(pano.wgs84, midLos.wgs84)
    shotLoc <- paste0('https://maps.googleapis.com/maps/api/streetview?size=640x640&pano=',panorama$pano_id,
                      '&heading=', heading, 
                      '&fov=',fov,
                      '&pitch=-0.76&key=', api.key)
    
    if(is.na(fDest)){
      fDest <- paste(photo.dir, toid, "_", panorama$pano_id,".jpg", sep="")
    }
    # download the picture from Streetview API
    download.file(shotLoc, fDest)
    return(fDest)
  } else {
    # Cat to empty file so only run onc
    fDest <- paste(photo.dir, toid, "_nolineofsight.txt", sep="")
    if (is.null(shp.panoIds)){
      cat("no direct line of sight", file=fDest)
    }
    return(fDest)
    }
}
funOsm <- function(){
    # Load Buildings
    if (!file.exists(osm.location)){
        osm <- list()
        osm$build <- readOGR(dsn=osm.dir, layer = 'gis.osm_buildings_a_free_1', verbose = FALSE)
        osm$road <- readOGR(dsn=osm.dir, layer = 'gis.osm_roads_free_1', verbose = FALSE)
        road.fclass.exclude <- c('footway', 'service',
                                 'cycleway', 'living_street', 'path', 'pedestrian')
        osm$road <- osm$road[!(osm$road$fclass %in% road.fclass.exclude),]
        osm <- lapply(osm, spTransform, CRS("+init=epsg:27700"))
        save(osm, file=osm.location)
    } else {
        load(osm.location)
    }
    return(osm)
}
getPanoIds <- function(s, plot=FALSE, savePano = TRUE, api.key){
  # Returns 1) panoIds (data.table: full set of pano_id and TOID (nearest match)) 
  #         2) shp.panoIds (spatialPointsDatqframe, panoid and location)
  panoIds.location <- '~/Dropbox/panorama.db/panoIds.rdata'
  if (file.exists(panoIds.location)){
    load(panoIds.location)
    check <- getPanoIds.check(s, panoIds)
    panoIds.toProcess <- unique(c(check$missingIds, check$errorsIds.OTHER, check$errorsIds.OTHER, check$statusIds.OVER_QUERY_LIMIT))
  } else {
    panoIds.toProcess <- s@data$TOID
    panoIds <- data.table(copyright = as.character(),
                          date = as.character(),
                          location.lat = as.character(),
                          location.lng = as.character(),
                          pano_id = as.character(),
                          status = as.character(),
                          TOID = as.character(),
                          error.message = as.character())
    check <- getPanoIds.check(s, panoIds)
  }
  while(length(unlist(panoIds.toProcess))>0){
    n.cores <- max(1, detectCores()-4)
    n.cores <- min(length(panoIds.toProcess),n.cores)
    panoIds <- panoIds[!(TOID %in% panoIds.toProcess)]
    cat('Processing', length(panoIds.toProcess), 'addresses')
    panoIds.new <- mclapply(panoIds.toProcess, function(x) getPanoIds.run(x, s, api.key), mc.cores=n.cores)
    panoIds.new.dt.ids <- which(unlist(sapply(panoIds.new, function(x) class(x)[1]!='try-error', simplify=TRUE)))
    panoIds.new <- rbindlist(panoIds.new[panoIds.new.dt.ids], use.names = TRUE, fill=TRUE)
    check.new <- getPanoIds.check(s, panoIds.new)
    panoIds.new.good <- panoIds.new[(TOID %in% c(check.new$statusIds.OK, check.new$statusIds.ZERO_RESULTS))]
    panoIds <- rbindlist(list(panoIds, panoIds.new.good), use.names = TRUE, fill=TRUE)
    panoIds <- unique(panoIds[, names(panoIds), with=FALSE])
    check<- getPanoIds.check(s, panoIds)
    cat(str(check))
    panoIds.toProcess <- unique(unlist(check)[!(unlist(check) %in% c(check$statusIds.OK, check$statusIds.ZERO_RESULTS))])
  }
  # Create shapes
  panoIds.unique <- unique(panoIds[status=='OK', .(location.lat, location.lng, pano_id)])
  shp.panoIds <- coords2points(panoIds.unique, l.p4string)
  l.panoIds <- list(panoIds=panoIds, shp.panoIds=shp.panoIds)
  #str(l.panoIds)
  return(l.panoIds)
}
getPanoIds.check <-function(s, panoIds){
  # Error check panoIds
  check <- list()
  check$missingIds <- s$TOID[!(s$TOID %in% panoIds$TOID)]
  error.ids <- FALSE
  # Error message
  if (length(which(names(panoIds)=='error_message')>0)) {
    errors <- panoIds[!is.na(error_message)]
    if (nrow(errors)>0){
      check$errorsIds.QUOTA <- unique(errors$TOID[which(str_detect(errors$error_message, 'quota'))])
      check$errorsIds.OTHER <- unique(errors$TOID[which(!str_detect(errors$error_message, 'quota'))])
      error.ids <- TRUE
    }
  } 
  if (error.ids==FALSE){
    check$errorsIds.QUOTA <- NULL
    check$errorsIds.OTHER <- NULL
  }
  # Bad Status
  status.names <- unique(panoIds$status)
  if (length(status.names)>1){
    status.ids <- sapply(status.names, function(x) unique(panoIds[status==x]$TOID))
    names(status.ids) <- paste0('statusIds.', names(status.ids))
  } else {
    status.ids <- unique(panoIds[status==status.names]$TOID)
    names(status.ids) <- paste0('statusIds.',  status.names)
  }
  check <- c(check, status.ids)
  return(check)
}
getPanoIds.run <-function(toid, s, api.key){
  s.i <- s[s@data$TOID==toid,]
  pano.loc <- paste0('~/Dropbox/panorama.db/',toid,".rds")
  centr <- gCentroid(s.i)
  centr.wgs84 <- spTransform(centr, CRS("+init=epsg:4326"))
  api.url <- paste("https://maps.googleapis.com/maps/api/streetview/metadata?size=600x300&location=",
                   paste(rev(coordinates(centr.wgs84)), collapse=",") ,"&key=",api.key, sep="")
  panorama <- unlist(fromJSON(file=api.url))
  panorama <- data.table(t(panorama), TOID = toid)
  saveRDS(panorama, file=pano.loc)
  return(panorama)
}
funPhotos.check <- function(photo.dir, tmp.dir){
  # After photo batch is run, examine the quality of the pictures
  photos <- list.files(photo.dir)
  l.check <- list()
  l.check$greenery <- fread(paste0(tmp.dir, 'greeneryIds.csv')) # From tensorflow
  l.check$greenery$error.type <- 'greenery'
  l.check$greenery <- l.check$greenery[, fDest:=paste0(TOID, '_', panoid, '.jpg')]
  # Labels (return query info from streetview)
  labels.index <- which(str_detect(photos, regex('(?<=\\_)[A-z]{1,}\\.txt')))
  labels.names <- unique(str_extract(photos[labels.index], regex('(?<=\\_)[A-z]{1,}(?=\\.txt)')))
  photos.labels <- photos[labels.index]
  # Find 
  if (length(labels.names)>1){
    labels.ids <- sapply(labels.names,
                         function(x) na.omit(str_extract(photos.labels, regex(paste0('(?<=^)[0-9]{1,}(?=\\_', x,')'), perl=TRUE))))
    fDests <- sapply(labels.names,
                     function(x) photos.labels[str_detect(photos.labels, regex(paste0('(?<=^)[0-9]{1,}(?=\\_', x,')'), perl=TRUE))])
    labels.dt <- lapply(labels.ids, function(x) data.table(TOID=unlist(x), panoid=NA))
    labels.dt <- mapply(function(x,y) data.table(x, error.type=y), labels.dt, labels.names, SIMPLIFY=FALSE)
    labels.dt <- mapply(function(x,y) data.table(x, fDest = unlist(y)), labels.dt, fDests, SIMPLIFY=FALSE)
    }
  if (length(labels.names)==1){
    labels.dt <- data.table(TOID=na.omit(str_extract(photos.labels, regex(paste0('(?<=^)[0-9]{1,}(?=\\_', labels.names,')'), perl=TRUE))),
                            panoid = NA,
                            error.type=labels.names,
                            fDest = photos.labels[str_detect(photos.labels, regex(paste0('(?<=^)[0-9]{1,}(?=\\_', labels.names,')'), perl=TRUE))])
  }
  l.check <- c(l.check, labels.dt)
  str(l.check)
 return(l.check)
}

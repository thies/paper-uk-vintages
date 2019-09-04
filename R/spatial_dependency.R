# analysis of spatial dependency in groundtruth and predicted style values

library(spdep)
library(keras)
library(dplyr)
colour <- c("#230356","#076391","#2fd1d1","#b51676","#e90d3a","#f98e19","#ffc217")


# load stacked Inception vectors 
# 2048 from obs (Variables "V123"), 2048 from neares neighbour ("VV123")
v <- read.csv("~/research/paper-cambridge-vintages/data/groundtruth_image_vectors_1nn.csv", as.is=TRUE)

# Exclude observations with bad images (those not showing home)
# There is no point in training with those
vv <- subset(v, true_era != "x greenery" & true_era != "w walls" )
vv <- subset(vv, !is.na(true_era))

# rescale inception vectors
vector.columns <- grepl('^V+\\d+$', colnames(vv), perl=TRUE)
vv[,vector.columns] <- vv[,vector.columns]/max(vv[,vector.columns])
vv$true_era <- factor(vv$true_era)


if(FALSE){
  # stratified training sample
  train <- vv %>%
    group_by(true_era) %>%
    sample_n(330)
  # there are not that many Georgians in the sample, unfortunately
  # trainng sample will be not fully balanced...
  tmp <- subset(vv, ! toid %in% unique(train$toid))
  tmp <- subset(tmp, true_era != "a georgian")
  train.tmp <- tmp %>%
    group_by(true_era) %>%
    sample_n(270)
  train <- rbind(train, train.tmp)
  
  # test with the rest, true out of sample
  test <- subset(vv, ! toid %in% unique(train$toid))
  
  ###### TRAIN 2 MODELS
  # - based on one picture only
  # - based on the nearest neighbour as well
  # ----------
  model1cols <- colnames(train)[grepl('^V\\d+$', colnames(train), perl=TRUE)]
  model2cols <- colnames(train)[grepl('^V+\\d+$', colnames(train), perl=TRUE)]
  
  pred <- list()
  for(vector.columns in list(model2cols, model1cols ) ){
    veclength <- length(vector.columns)
    # define the train and test data
    x_train <- as.matrix( vv[,vector.columns] )
    x_test <- as.matrix( vv[,vector.columns] )
    y_train <- to_categorical( as.numeric(vv$true_era) -1 )
    # define model
    model <- keras_model_sequential() 
    model %>% 
      layer_dense(units = ncol(x_train)/2, activation = 'relu', input_shape = c(ncol(x_train))) %>% 
      layer_dropout(rate = 0.5) %>% 
      layer_dense(units = ncol(y_train), activation = 'softmax')
    # model summary
    summary(model)
    # compile model
    model %>% compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_rmsprop(),
      metrics = c('accuracy')
    )
    # train model and plot loss functions/accuracy
    history <- model %>% fit(
      x_train, y_train, 
      epochs = 50, batch_size = 128, 
      validation_split = 0.2
    )
    plot(history)
    pred[[ as.character(veclength) ]] <- list(
      hist = history, 
      pred = model %>% predict_classes(x_test),
      true = true_class <- as.numeric(test$true_era)-1)
  }


  fullpred <- list()
  lev <- levels(vv$true_era)
  for(n in c("2048","4096")){
    fullpred[[paste("pred_era_",n, sep="")]] <- unlist(pred[[n]]$pred)
    fullpred[[paste("pred_era_",n, sep="")]] <- sapply( fullpred[[paste("pred_era_",n, sep="")]], function(x){ return( lev[x+1])}) 
    
  }  
  fullpred <- as.data.frame( do.call(cbind, fullpred))
  fullpred$toid <- vv$toid
  coords <- read.csv("~/research/paper-uk-vintages/data/toid_coords.csv", as.is=TRUE)
  fullpred <- merge(fullpred, coords, by="toid", all.x=TRUE)
  vvv <- unique( subset(vv, select=c("toid","true_era")))
  fullpred <- merge(fullpred, vvv, by="toid", all.x=TRUE)
  write.csv(fullpred, "~/research/paper-uk-vintages/data/pred_full_sample.csv", row.names=FALSE)
}

fullpred <- read.csv("~/research/paper-uk-vintages/data/pred_full_sample.csv", as.is=TRUE)
coordinates(fullpred) <- ~ x + y

k5.nb <- knn2nb(knearneigh(fullpred, 5))
jcm.4096 <- joincount.multi(factor(fullpred$pred_era_4096), nb2listw(k5.nb, style="B"))
jcm.2048 <- joincount.multi(factor(fullpred$pred_era_2048), nb2listw(k5.nb, style="B"))
cbind(jcm.4096[,1],jcm.2048[,1])


jct.4096 <- joincount.test(factor(fullpred$pred_era_4096), nb2listw(k5.nb, style="B"))
jct.2048 <- joincount.test(factor(fullpred$pred_era_2048), nb2listw(k5.nb, style="B"))
jct.true <- joincount.test(factor(fullpred$true_era), nb2listw(k5.nb, style="B"))

jct <- list()
for(i in 1:7){
  jct[[i]] <- c(jct.true[[i]][[3]][1],jct.4096[[i]][[3]][1], jct.2048[[i]][[3]][1]) 
}
same.colour.stat <- as.data.frame( do.call(rbind, jct))
colnames(same.colour.stat) <- c("true","4096","2048")
same.colour.stat$ratio.4096 <- same.colour.stat[,2]/same.colour.stat[,1]*100-100 
same.colour.stat$ratio.2048 <- same.colour.stat[,3]/same.colour.stat[,1]*100-100 
barplot(t(as.matrix( same.colour.stat[,c("ratio.4096","ratio.2048")])), beside = TRUE, col=colour[c(2,5)], ylab="% of groundtruth stat.")
box()

library(raster)
shp <- shapefile("~/Dropbox (Cambridge University)/Cambridge/data/shapefiles/buildings_with_centroids_area_volume_panoid_era.shp")
shp$toid <- as.numeric(shp$TOID)
fullpred <- read.csv("~/research/paper-uk-vintages/data/pred_full_sample.csv", as.is=TRUE)
shp <- merge(shp, unique(fullpred), by="toid", all.x=TRUE)
shp <- shapefile(shp,filename="~/Dropbox (Cambridge University)/Cambridge/data/shapefiles/buildings_with_centroids_area_volume_panoid_era_full.shp")


f1 <- list()
for(n in c("2048","4096")){
  # tabulate predictons vs true era
  tab <- table(as.data.frame(cbind(pred[[n]]$pred, vv$true_era )))
  rownames(tab) <- levels(train$true_era)  
  colnames(tab) <- levels(train$true_era)  
  # classification accuracy statistics
  recall <- diag(tab)/colSums(tab)
  precision <- diag(tab)/rowSums(tab)
  f1scores <- 2*(precision*recall)/(precision+recall)
  f1[[n]] <- round(f1scores, 2)
}
do.call(rbind, f1)





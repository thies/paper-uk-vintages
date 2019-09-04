# load additional libraries
library(spdep)
library(keras)
library(dplyr)


source("~/research/paper-uk-vintages/R/helper.R")


#==================================
# Create 4096 vectors, by stacking 2048 from nearest neighbour
# Most of the time, this won't be necessary....
if(FALSE){
  v <- read.csv("~/research/paper-cambridge-vintages/data/groundtruth_image_vectors.csv", as.is=TRUE)
  v <- unique(v)
  #vv <- subset(v, !is.na(x) & true_era != "x greenery" & true_era != "w walls" )
  v$true_era <- factor(v$true_era)
  
  vector.columns <- grepl('^V\\d+$', colnames(v), perl=TRUE)
  
  # find nearest neighbours
  k <- 1
  col.knn <- knearneigh(as.matrix(v[,c("x","y")]), k=30)
  # calculate average vector for n nearest neighbours 
  indices <- as.matrix(col.knn[[1]])
  # only keep neighbours that offer clean view on building,
  # exclude greenery and walls
  walls.greenery.index <- which(v$true %in% c("x greenery","w walls"))
  indices[indices %in% walls.greenery.index] <- NA
  
  nnvec <- list()
  
  for(i in 1:nrow(v)){
    if(i %% 200 == 0){ print(i) }
    toid.tmp <- as.character( v$toid[i] )
    ind.tmp <- indices[i,]
    ind.tmp <- ind.tmp[!is.na(ind.tmp)][1]
    nnvec[[ toid.tmp ]] <- as.list( v[ ind.tmp , vector.columns])
  }  
  nnvecs <- as.data.frame( do.call( rbind, nnvec ))
  toids.tmp <- rownames(nnvecs)
  nnvecs <- matrix(unlist(nnvecs), ncol=2048 )
  colnames(nnvecs) <- paste("VV", 1:ncol(nnvecs), sep="")
  nnvecs <- as.data.frame(nnvecs)
  nnvecs$toid <- as.numeric(unlist(toids.tmp))
  rm(nnvec)
  vvv <- merge(v, nnvecs, by="toid")
  rm(nnvecs)
  write.csv(vvv, "~/research/paper-cambridge-vintages/data/groundtruth_image_vectors_1nn.csv", row.names = FALSE)
}



#############################################
# train the model(s)

library(spdep)
library(keras)
library(dplyr)

# load stacked Inception vectors 
# 2048 from obs (Variables "V123"), 2048 from neares neighbour ("VV123")
v <- read.csv("~/research/paper-cambridge-vintages/data/groundtruth_image_vectors_1nn.csv", as.is=TRUE)
vv <- v
# rescale inception vectors
vector.columns <- grepl('^V+\\d+$', colnames(vv), perl=TRUE)
vv[,vector.columns] <- vv[,vector.columns]/max(vv[,vector.columns])

for(iteration in 1:100){
  cat(paste("\n\n\n\n\n\n\nIteration: ", iteration, "\n\n"))
  # stratified training sample
  # Exclude observations with bad images (those not showing home)
  # There is no point in training with those
  train.pre <- subset(vv, true_era != "x greenery" & true_era != "w walls" )
  train.pre <- subset(train.pre, !is.na(true_era))
  train <- train.pre %>%
    group_by(true_era) %>%
    sample_n(330)
  # there are not that many Georgians in the sample, unfortunately
  # trainng sample will be not fully balanced...
  tmp <- subset(train.pre, ! toid %in% unique(train$toid))
  tmp <- subset(tmp, true_era != "a georgian")
  train.tmp <- tmp %>%
    group_by(true_era) %>%
    sample_n(270)
  train <- rbind(train, train.tmp)
  train$true_era <- factor(train$true_era)
  rm(train.pre)
  # test with the rest, true out of sample
  test <- subset(vv, ! toid %in% unique(train$toid))
  
  ###### TRAIN 2 MODELS
  # - based on one picture only
  # - based on the nearest neighbour as well
  # ----------
  model1cols <- colnames(train)[grepl('^V\\d+$', colnames(train), perl=TRUE)]
  model2cols <- colnames(train)[grepl('^V+\\d+$', colnames(train), perl=TRUE)]
  
  for(vector.columns in list(model1cols, model2cols ) ){
    veclength <- length(vector.columns)
    # define the train and test data
    x_train <- as.matrix( train[,vector.columns] )
    x_test <- as.matrix( test[,vector.columns] )
    y_train <- to_categorical( as.numeric(train$true_era) -1 )
    # define model
    model <- keras_model_sequential() 
    model %>% 
      layer_dense(units = ncol(x_train)/2, activation = 'relu', input_shape = c(ncol(x_train))) %>% 
      layer_dropout(rate = 0.5) %>% 
      layer_dense(units = ncol(x_train)/4, activation = 'relu') %>% 
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
    jpeg(filename=paste("~/research/paper-cambridge-vintages/data/iterations/training_history",iteration,"_", veclength,".jpg", sep=""), height=400, width=700)
    plot(history)
    dev.off()
    
    # predict, out of sample
    pred <- model %>% predict_classes(x_test)
    predictions.tmp <- as.data.frame( cbind(test$toid, pred, veclength)) 
    colnames(predictions.tmp) <- c("toid","pred","model")
    write.csv(predictions.tmp, file = paste( "~/research/paper-uk-vintages/data/derived/k-fold-preds", iteration, "_", veclength,".csv", sep=""), row.names=FALSE)
    rm(predictions.tmp)
    }
}



# find best classification
bestPrediction <- function( model = "4096", num=100){
  pred <- list()
  for(n in 1:num){
    try( pred[[as.character(n)]] <- read.csv(file = paste("~/research/paper-uk-vintages/data/derived/k-fold-preds",n,"_",model,".csv", sep=""), as.is=TRUE)[,c("toid","pred")] )
  }
  pred <- as.data.frame( do.call(rbind, pred))
  tab <- table(pred$toid, pred$pred)
  tab.max <- apply(tab, 1, max)
  pred.max <- rep(NA, nrow(tab))
  for(i in 1:ncol(tab)){
    pred.max[ tab[,i] == tab.max] <- i-1
  }
  tmp <- as.data.frame( cbind(row.names(tab),pred.max))
  colnames(tmp) <- c("toid",paste("pred",model, sep=""))
  return( tmp )
}

distPrediction <- function( model = "4096", num=100){
  pred <- list()
  for(n in 1:num){
    try( pred[[as.character(n)]] <- read.csv(file = paste("~/research/paper-uk-vintages/data/derived/k-fold-preds",n,"_",model,".csv", sep=""), as.is=TRUE)[,c("toid","pred")] )
  }
  pred <- as.data.frame( do.call(rbind, pred))
  return( pred )
}
pred.dist.4096 <- distPrediction(model="4096")
pred.dist.4096.tab <- table(pred.dist.4096$toid, pred.dist.4096$pred)
write.csv(pred.dist.4096.tab, paste("~/research/paper-uk-vintages/data/derived/k-fold-preds_tabulated_4096.csv", sep=""))


pred.4096 <- bestPrediction(model="4096")
pred.2048 <- bestPrediction(model="2048")
preds <- merge(pred.2048, pred.4096, by="toid")  
preds <- merge(preds, v[,c("toid","true_era")], all.x=TRUE)
preds$true_era <- factor(preds$true_era)
lev <- levels(preds$true_era)
preds$pred4096 <- sapply(as.numeric(preds$pred4096), function(x){ return(lev[x])} )
preds$pred2048 <- sapply(as.numeric(preds$pred2048), function(x){ return(lev[x])} )
write.csv(preds, file="~/research/paper-uk-vintages/data/derived/predicted_styles.csv", row.names=FALSE)

#============================================
# create confusion matrix tables  
tab <- table(preds[, c("pred4096","true_era")])[,1:7]
colnames(tab) <- unlist(custom.labels)
rownames(tab) <- unlist(custom.labels)
# classification accuracy statistics
recall <- diag(tab)/colSums(tab)
precision <- diag(tab)/rowSums(tab)
f1scores <- 2*(precision*recall)/(precision+recall)
f1scores.4096 <- f1scores
tab.mean <- tab
for(i in 1:ncol(tab)){
  tab.mean[,i] <- (tab.mean[,i]/sum(tab.mean[,i]))*100
}
tab.4096 <- rbind(tab, tab.mean, recall, precision, f1scores)
# base model
tab <- table(preds[, c("pred2048","true_era")])
colnames(tab) <- unlist(custom.labels)
rownames(tab) <- unlist(custom.labels)
# classification accuracy statistics
recall <- diag(tab)/colSums(tab)
precision <- diag(tab)/rowSums(tab)
f1scores <- 2*(precision*recall)/(precision+recall)
f1scores.2048 <- f1scores
tab.mean <- tab
for(i in 1:ncol(tab)){
  tab.mean[,i] <- (tab.mean[,i]/sum(tab.mean[,i]))*100
}
tab.2048 <- rbind(tab, tab.mean, recall, precision, f1scores)
library(xtable)
confusion.matrices.mean.digits <- matrix(0,nrow=nrow(tab.4096), ncol=ncol(tab.4096)+1)
confusion.matrices.mean.digits[15:17,]<-2

print.xtable(xtable(tab.4096,digits=confusion.matrices.mean.digits),
             booktabs = TRUE,
             sanitize.rownames.function = function(x){
               x <- gsub(".\\d+$","", x, perl=TRUE)
               x <- gsub("Contemporary","Cont.", x, perl=TRUE)
               return( x )
             },
             size="footnotesize",
             type="latex",
             file="~/research/paper-uk-vintages/text/confusion_matrix_raw_4096.tex"
)
print.xtable(xtable(tab.2048, digits=confusion.matrices.mean.digits),
             booktabs = TRUE,
             sanitize.rownames.function = function(x){
               x <- gsub(".\\d+$","", x, perl=TRUE)
               x <- gsub("Contemporary","Cont.", x, perl=TRUE)
               return( x )
             },
             size="footnotesize",
             type="latex",
             file="~/research/paper-uk-vintages/text/confusion_matrix_raw_2048.tex"
)
jpeg(filename="~/research/paper-uk-vintages/text/figures/barplot_f1scores.jpg", width=700, height=350)
par(mar=c(3.1,3.1,1,1.1))
barplot(t(cbind(f1scores.2048, f1scores.4096))[,1:7], beside=TRUE, col=colour[2:3], names.arg=c("Georg.","Early V.", "Late V./E.","Interw.","Postw.","Cont.","Revival"), legend.text=c("base classifier","spatial classifier"))
box()
dev.off()

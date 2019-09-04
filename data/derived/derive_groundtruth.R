setwd("~/Dropbox (Cambridge University)/COMBINED_0926/")
folders <- list.files()

gt <- list()
for (f in folders){
  gt[[f]] <-  as.data.frame( list.files(f) )
}
gt <- do.call(rbind, gt) 
colnames(gt) <- "TOID"
gt$true_era <- rownames(gt)
gt$true_era <- gsub("_"," ",gt$true_era)
gt$true_era <- tolower(gt$true_era)
gt$true_era <- gsub(".(\\d+)$","",gt$true_era, perl=TRUE)
gt$true_era <- gsub("\\."," ",gt$true_era, perl=TRUE)
gt$true_era <- gsub("  "," ",gt$true_era, perl=TRUE)

gt$TOID <- gsub("^(\\d+)_.+$","\\1",gt$TOID, perl=TRUE)

write.csv(gt, file="~/Dropbox (Cambridge University)/Cambridge/data/derived/groundtruth.csv", row.names=FALSE)
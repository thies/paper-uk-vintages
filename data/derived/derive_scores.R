library(rjson)

setwd("~/tf_files/scores/")
scorefiles <- list.files(".")

js <- list()
for(f in scorefiles){
  js[[f]] <- unlist(fromJSON(file=f))
}

js.df <- as.data.frame( do.call(rbind, js) )
eras <- colnames(js.df)
js.df$max <- apply(js.df, 1, max)
js.df$TOID <- rownames(js.df)
rownames(js.df) <- NULL
js.df$TOID <- gsub("^(\\d+)_.+$","\\1",js.df$TOID, perl=TRUE)
js.df$era <- NULL
for(e in eras){
  js.df$era[ js.df[,e] == js.df$max ] <- e
}
write.csv(js.df, "~/Dropbox (Cambridge University)/Cambridge/data/derived/scores_toid.csv", row.names=FALSE)






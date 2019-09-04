# Table with pictures of estimate vintages 

plot_top_pictures <- function( regsam=NA, n=10, labels_short=NA, figpath = "/home/thies/db/Cambridge/data/images/cambridge/", img.width="50px"){
  library(xtable)
  options(xtable.sanitize.text.function=identity)
  
  # load data
  if( is.na(regsam) ){
    require(RCurl)
    remote.file <-  "https://www.dropbox.com/s/he3hfquk8claaa1/regsamples.csv?dl=1"  
    tmp.file <- tempfile()
    download.file(remote.file, tmp.file)
    regsam <- read.csv(tmp.file)
    file.remove(tmp.file)
  }
  if( is.na(labels_short) ){
    labels_short <- c("Georg.","Early Vic.","Late V./Edw.","Interwar","Postwar","Contemp.","Revival")
  }
  eras <- unique(regsam$era)
  eras <- eras[grepl("^[a-z]", eras)]
  
  eras <- eras[ order(eras) ]
  names(labels_short) <- eras
  
  files <- list.files( figpath )
  files <- files[grepl(".jpg", files)]
  
  tab <- matrix("", n , length(eras))
  colnames(tab) <- eras
  
  for(e in eras){
    tmp <- unique( subset(regsam, era == e, select=c("TOID","max","era")))
    tmp <- tmp[rev(order(tmp$max)),]
    for(i in 1:n){
      tab[i, e ] <- files[ grepl( as.character( tmp$TOID[i] ), files )]
      tab[ i, e] <- paste("\\includegraphics[width=",img.width,"]{", figpath ,tab[i,e],"}", sep="")
    }
  }
  colnames(tab) <- paste("\\emph{",labels_short,"}", sep="")
  xtab <- xtable( tab, 
                  booktabs=TRUE,
                  comment=FALSE,
                  row.names=FALSE)
  print(xtab, 
        comment=FALSE,
        include.rownames=FALSE,
        floating=FALSE)
}
#plot_top_pictures(regsam=regsample)

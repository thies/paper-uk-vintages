### Hedonic regression analysis

library(xtable)
library(stargazer)
library(spdep)

# 
setwd("~/research/paper-uk-vintages/R/") 


# Paths to datasets
paths <- list()
local.paths <- TRUE
if(local.paths){
  paths[["full_samples"]] <- "~/dropbox/paper-uk-vintages/pred_full_sample.csv"
  paths[["preds"]] <- "~/dropbox/paper-uk-vintages/predicted_styles.csv"
  paths[["sales"]] <- "~/dropbox/Cambridge/data/derived/sales_2018-10.csv"
  paths[["lsoa"]] <- "~/dropbox/Cambridge/data/derived/lsoa_x_toid.csv"
} else {
  paths[["full_samples"]] <- "https://www.dropbox.com/s/v26ofbxg2zmzbrp/pred_full_sample.csv?dl=1"
  paths[["preds"]] <- "https://www.dropbox.com/s/3ked11qizv37lpj/predicted_styles.csv?dl=1"
  paths[["sales"]] <- "https://www.dropbox.com/s/hu3mlar1n155w7m/sales_2018-10.csv?dl=1"
  paths[["lsoa"]] <- "https://www.dropbox.com/s/8wfw8tdq2sgxza8/lsoa_x_toid.csv?dl=1"
}


source("./helper.R")


# load sales data
v <- read.csv(paths[['full_samples']], as.is=TRUE)
v$pred_era_4096 <- NULL
v$pred_era_2048 <- NULL
v$true_era <- NULL
preds <- read.csv(paths[["preds"]], as.is=TRUE)
v <- unique( merge(v, preds, by="toid", all=TRUE ))

v$pred_era <- v$pred4096
v$true_era[v$true_era %in% c("x greenery", "w walls")] <- NA 
v$true_era_aug <- v$pred_era
v$true_era_aug[! is.na(v$true_era)] <- v$true_era[!is.na(v$true_era)] 


s <- read.csv(paths[["sales"]], as.is=TRUE)
s$toid <- s$TOID
s$TOID <- NULL
s$true_era <- NULL
s$pred_era <- NULL
sales <- merge( s, v,  by="toid", all=TRUE)
# remove flats or "other"
sales <- subset(sales, propertytype %in% c("D","S","T"))
sales$date <- as.Date(sales$date)
# remove sales with prices above 1m
sales <- subset(sales, price <= 1000000)


# drop leaseholds
sales <- subset(sales, estatetype =="F")

# add lsoaid 
lsoa <- read.csv(paths[["lsoa"]], as.is=TRUE)

sales <- merge(sales, lsoa, by="toid", all.x=TRUE)
sales$true_era <- factor(sales$true_era)



### load Herf scores
herfs <- read.csv("~/research/paper-uk-vintages/data/derived/toid_pred_herf.csv", as.is=TRUE)
sales <- merge(sales, herfs, by="toid", all.x=TRUE)
herf.threshold <- quantile(sales$herf, 0.33, na.rm=TRUE)


###### Reverse regressions, AIC comparison
reverseRegressions <- function(){
  lev <- levels(sales$true_era)
  rev.reg.tab <- list()
  for(l in lev){
    print(l)
    sales$tmp_true <- 0
    sales$tmp_true[ sales$true_era == l] <- 1
    sales$tmp_pred <- 0
    sales$tmp_pred[ sales$pred4096 == l] <- 1
    rev.reg.tab[[ l ]] <- list()
    glm.tmp <- glm(tmp_true ~lsoa11cd + factor(year) + log(area2D +1) + log(volum3D +1) + tmp_pred,  family=binomial(), data=sales)
    glm.tmp.sum <- summary(glm.tmp)
    rev.reg.tab[[ l ]][["aic4096"]] <- glm.tmp$aic
    rev.reg.tab[[ l ]][["coeff4096"]] <- glm.tmp$coefficients["tmp_pred"]
    rev.reg.tab[[ l ]][["pval4096"]] <- glm.tmp.sum$coefficients["tmp_pred", 4]
    glm.tmp <- glm(tmp_true ~lsoa11cd + factor(year) + log(area2D +1) + log(volum3D +1) + tmp_pred,  family=binomial(), data=subset(sales, herf > herf.threshold))
    glm.tmp.sum <- summary(glm.tmp)
    rev.reg.tab[[ l ]][["aic4096.herf"]] <- glm.tmp$aic
    rev.reg.tab[[ l ]][["coeff4096.herf"]] <- glm.tmp$coefficients["tmp_pred"]
    rev.reg.tab[[ l ]][["pval4096.herf"]] <- glm.tmp.sum$coefficients["tmp_pred", 4]
    sales$tmp_pred <- 0
    sales$tmp_pred[ sales$pred2048 == l] <- 1
    glm.tmp <- glm(tmp_true ~lsoa11cd + factor(year) + log(area2D +1) + log(volum3D +1) + tmp_pred,  family=binomial(), data=sales)
    glm.tmp.sum <- summary(glm.tmp)
    rev.reg.tab[[ l ]][["aic2048"]] <- glm.tmp$aic
    rev.reg.tab[[ l ]][["coeff2048"]] <- glm.tmp$coefficients["tmp_pred"]
    rev.reg.tab[[ l ]][["pval2048"]] <- glm.tmp.sum$coefficients["tmp_pred", 4]
    glm.tmp <- glm(tmp_true ~lsoa11cd + factor(year) + log(area2D +1) + log(volum3D +1),  family=binomial(), data=sales)
    glm.tmp.sum <- summary(glm.tmp)
    rev.reg.tab[[ l ]][["aic.base"]] <- glm.tmp$aic
  }
  rev.reg.tab.comb <- cbind( do.call(rbind, rev.reg.tab))
  rev.reg.tab.comb[,c("aic.base","aic2048","aic4096","aic4096.herf")]
  return( xtable(rev.reg.tab.comb[,c("aic.base","aic2048","aic4096","aic4096.herf")], digits = 0, booktabs=TRUE) )
}
#revRegTable <- reverseRegressions()
#================================






#========= 
# find dominant style of homes on same street, within 100m  
# (not very elegant code, admittedly)
sales <- subset(sales, !is.na(centrX))
dnn <- dnearneigh(as.matrix(sales[,c("centrX","centrY")]), 0, 100)
sales$neigh_era <- NA
sales$neigh_era_aug <- NA
for(i in 1:length(dnn)){
  print(i)
  tmp <- unique(sales[ dnn[[i]], c("street", "pred_era","true_era_aug","toid","herf") ])
  tmp <- subset(tmp, street==sales$street[i] & herf > herf.threshold)
  tmp2 <- table( tmp$pred_era )
  tmp2 <- tmp2[rev(order(tmp2))]
  try(sales$neigh_era[i] <- names(tmp2)[1])
  tmp3 <- table( tmp$true_era_aug )
  tmp3 <- tmp3[rev(order(tmp3))]
  try(sales$neigh_era_aug[i] <- names(tmp3)[1])
}
sales$neigh_era <- relevel(factor(sales$neigh_era), "f contemporary")
sales$neigh_era_aug <- relevel(factor(sales$neigh_era_aug), "f contemporary")
sales$true_era_aug <- relevel(factor(sales$true_era_aug), "f contemporary")


############### HEDONIC MODEL
rs_reg <- list()
sales$pred_era <- relevel( factor(sales$pred_era), "f contemporary" )
sales$true_era <- relevel( factor(sales$true_era), "f contemporary" )

sales$true_era[sales$true_era =="w walls"] <- NA
sales$true_era[sales$true_era =="x greenery"] <- NA



regsample <- subset(sales, !is.na(pred_era) & ! is.na(true_era) & ! is.na(neigh_era))
regsample.new <- subset(regsample, new=="Y" & ( true_era == "f contemporary" | true_era == "g cont victorian") & ( pred_era == "f contemporary" | pred_era == "g cont victorian")   )
regsample$misclassified <- 0
regsample$misclassified[regsample$pred_era != regsample$true_era] <- 1


regsample$era <- regsample$true_era
rs_reg[["hed.base"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new, data=regsample )
summary(rs_reg[["hed.base"]])

rs_reg[["hed.base.true"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+era, data=regsample)
summary(rs_reg[["hed.base.true"]])

rs_reg[["hed.base.true.nolsoa"]] <- lm(log(price)~factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+era, data=regsample)
summary(rs_reg[["hed.base.true.nolsoa"]])


rs_reg[["hed.base.mis"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+era, data=subset( regsample, misclassified == 1))
summary(rs_reg[["hed.base.mis"]])

regsample$era <- regsample$pred_era
rs_reg[["hed.base.mis.pred"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+era, data=subset( regsample, misclassified == 1))
summary(rs_reg[["hed.base.mis.pred"]])


regsample$era <- regsample$pred_era
rs_reg[["hed.base.pred"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+era, data=regsample)
summary(rs_reg[["hed.base.pred"]])

# high herf observations only
regsample$era <- regsample$pred_era
rs_reg[["hed.base.herf"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+era, data=subset(regsample, !is.na(herf) & herf > herf.threshold))
summary(rs_reg[["hed.base.herf"]])


regsample.new$era <- regsample.new$true_era
rs_reg[["hed.new.true"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist) + propertytype+log(area2D+1)+log(volum3D+1)+ era, data=regsample.new)
summary(rs_reg[["hed.new.true"]])

regsample.new$era <- regsample.new$pred_era
rs_reg[["hed.new.pred"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist) + propertytype+log(area2D+1)+log(volum3D+1)+era, data=regsample.new)
summary(rs_reg[["hed.new.pred"]])

# Neighbourhood effects
regsample$era <- regsample$pred_era
regsample$nera <- regsample$neigh_era
rs_reg[["hed.neigh"]]<- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+ era+nera, data=subset(regsample, !is.na(herf) & herf > herf.threshold))
summary(rs_reg[["hed.neigh"]])

# Interaction terms, one base
rs_reg[["int"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+ era*nera, data=subset(regsample, !is.na(herf) & herf > herf.threshold))
summary(rs_reg[["int"]])



#sales$era <- sales$true_era_aug
#sales$nera <- sales$neigh_era_aug
#rs_reg[["hed.neigh.aug"]]<- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+ era+nera, data=sales)
#summary(rs_reg[["hed.neigh.aug"]])

# Interaction terms, one base
#rs_reg[["int.aug"]] <- lm(log(price)~lsoa11cd+factor(year)+log(dist)+propertytype+log(area2D+1)+log(volum3D+1)+new+ era*nera, data=sales)
#summary(rs_reg[["int.aug"]])




# Robust SE's
rs_robustse <- list()
rmse <- list()
library(sandwich)
for(n in names(rs_reg)){
  rs_robustse[[n]] <- as.numeric( sqrt(diag( vcovHC(rs_reg[[ n ]]  , type = "HC1")  )))
  rmse[[n]] <- sqrt(mean(rs_reg[[ n ]]$residuals^2))
}

n <- table(sales$neigh_era, sales$era)
n <- n[order(rownames(n)),order(colnames(n))]
rownames(n) <- paste("neigh", row.names(n))


rs_reg.set1 <- list()
rs_robustse.set1 <- list()
rmse.set1 <- list()
for(r in c("hed.base", "hed.base.true","hed.base.true.nolsoa","hed.base.pred","hed.base.herf","hed.base.mis","hed.new.true", "hed.neigh","int") ){
  rs_reg.set1[[r]] <-rs_reg[[r]] 
  rs_robustse.set1[[r]] <-rs_robustse[[r]] 
  rmse.set1[[r]] <- round( rmse[[r]], 3)
}

rs_reg.set2 <- list()
rs_robustse.set2 <- list()
rmse.set2 <- list()
for(r in c("hed.base.true","hed.base.true.nolsoa") ){
  rs_reg.set2[[r]] <-rs_reg[[r]] 
  rs_robustse.set2[[r]] <-rs_robustse[[r]] 
  rmse.set2[[r]] <- round( rmse[[r]], 3)
}



stargazer(rs_reg.set1,
          se=rs_robustse.set1,
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent variable: ln(price)",
          omit="year|lsoa|pred_era_neigh|:neigh_era|:nera",
          type="text",
          #type="latex",
          #out = "/home/thies/research/paper-uk-vintages/text/hed_reg_table_raw.set1.tex",
          intercept.bottom=FALSE,
          keep.stat=c("adj.rsq", "n"),
          digits=2,
          title="Hedonic Regression Estimates",
          font.size="footnotesize",
          label="tab:hedreg",
          no.space=TRUE,
          covariate.labels = c("Constant", 
                               "ln(dist. city center)",
                               "Type: semi-detached", "Type: terraced",
                               "ln(area)", "ln(volume)",
                               "New",
                               "Georgian","Early Vic.","Late Vic./Edw.","Interwar","Postwar","Revival",
                               "Neigh: Georgian","Neigh: Early Vic.","Neigh: Late V./Edw.","Neigh: Interwar","Neigh: Postwar","Neigh: Revival"
          ),
          add.lines = list(
            c("Year dummies", rep("Yes",9)),
            c("Neigh. dummies", rep("Yes",9)),
            c("Interaction terms", rep("No",8),"Next Table") ,
            c("RMSE", as.vector(unlist(rmse.set1)) ))
          )

# for referee
stargazer(rs_reg.set2,
          se=rs_robustse.set2,
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent variable: ln(price)",
          omit="year|lsoa|pred_era_neigh|:neigh_era|:nera",
          #type="text",
          type="latex",
          #out = "/home/thies/research/paper-uk-vintages/text/hed_reg_table_raw.set1.tex",
          intercept.bottom=FALSE,
          keep.stat=c("adj.rsq", "n"),
          digits=2,
          title="Hedonic Regression Estimates",
          font.size="footnotesize",
          label="tab:hedreg",
          no.space=TRUE,
          covariate.labels = c("Constant", 
                               "ln(dist. city center)",
                               "Type: semi-detached", "Type: terraced",
                               "ln(area)", "ln(volume)",
                               "New",
                               "Georgian","Early Vic.","Late Vic./Edw.","Interwar","Postwar","Revival",
                               "Neigh: Georgian","Neigh: Early Vic.","Neigh: Late V./Edw.","Neigh: Interwar","Neigh: Postwar","Neigh: Revival"
          ),
          add.lines = list(
            c("Year dummies", rep("Yes",2)),
            c("Neigh. dummies","Yes","No"),
            c("Interaction terms", rep("No",2)) ,
            c("RMSE", as.vector(unlist(rmse.set2)) ))
)
# herf concentration of style per lsoa

lsoaXstyle <- table(regsample$lsoa11cd, regsample$true_era)
lsoa.herfs <-  rowSums((lsoaXstyle/rowSums(lsoaXstyle))^2)
summary(lsoa.herfs )
hist(lsoa.herfs, breaks=50, col="lightblue", xlab="Herfindahl score of style at LSOA level")
box()
rs_reg.set2 <- list()
rs_robustse.set2 <- list()
for(r in c("hed.neigh.aug","hed.neigh","int.aug","int") ){
  rs_reg.set2[[r]] <-rs_reg[[r]] 
  rs_robustse.set2[[r]] <-rs_robustse[[r]] 
}

library(raster)
shp <- shapefile("../text/maps/shp/Lower_Super_Output_Areas_December_2001_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
for(s in unique(regsample$true_era)){
  pdf(paste(file="../text/maps/",s,".pdf", sep=""), width = 2, height=2)
  par(mar = c(0.2,0.2,0.2,0.2))
  plot(range(regsample$x), range(regsample$y),type="n", xaxt='n', yaxt='n', xlab=NA, ylab=NA)
  plot(shp, add=TRUE, lwd=0.5)
  points(regsample$x[regsample$true_era == s], regsample$y[regsample$true_era == s], pch=19, cex=0.4)
  dev.off()
}







stargazer(rs_reg.set2,
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent variable: ln(price)",
          se=rs_robustse.set2,
          omit="year|lsoa|pred_era_neigh|:nera",
          #type="latex",
          type="text",
          out = "/home/thies/research/paper-uk-vintages/text/hed_reg_table_raw.set2.tex",
          intercept.bottom=FALSE,
          keep.stat=c("adj.rsq", "n"),
          digits=2,
          title="Hedonic Regression Estimates",
          font.size="footnotesize",
          label="tab:hedreg",
          no.space=TRUE,
          covariate.labels = c("Constant", 
                               "ln(dist. city center)",
                               "Type: semi-detached", "Type: terraced",
                               "ln(area)", "ln(volume)",
                               "New",
                               "Georgian","Early Vic.","Late Vic./Edw.","Interwar","Postwar","Revival",
                               "Neigh: Georgian","Neigh: Early Vic.","Neigh: Late V./Edw.","Neigh: Interwar","Neigh: Postwar","Neigh: Revival"
          ),
          add.lines = list(
            c("Neigh. interact.", rep("No",2), rep("See next table",2)),
            c("Year dummies", "Yes","Yes", "Yes","Yes","Yes","Yes"),
            c("Neigh. dummies", "Yes", "Yes","Yes","Yes","Yes","Yes"))
)




xtab <- list()
for(model in c("int")){
  tmp <- stargazer(rs_reg[[model]], type="text",
                   title="Summary statistics residential property transactions",
                   digits=2,
                   se=list(rs_robustse[["full_int"]]),
                   single.row = TRUE
  ) 
  intcoeffs.tmp <- list()
  for(i in 1:length(tmp)){
    line <- tmp[i]
    if(grepl(":nera", line)){
      line <- gsub("\\s+$","",line, perl=TRUE)
      key <- gsub("\\s\\s.+$","" ,line, perl=TRUE)
      val <- gsub("^.*\\s\\s","" ,line, perl=TRUE)
      intcoeffs.tmp[[key]] <- val
    }
  }
  eras  <- levels(sales$pred_era)
  intcoeffs <- matrix("--", length(eras), length(eras))
  colnames(intcoeffs) <- eras
  rownames(intcoeffs) <- eras
  
  for(nn in names(intcoeffs.tmp)){
    b.era <- gsub("^era","",nn, perl=TRUE) 
    b.era <- gsub(":.*$","",b.era, perl=TRUE) 
    n.era <- gsub("^.*:","",nn, perl=TRUE) 
    n.era <- gsub("^nera","",n.era, perl=TRUE) 
    if(!grepl(":",intcoeffs.tmp[[ nn ]])){
      intcoeffs[n.era,b.era] <- intcoeffs.tmp[[ nn ]]
    }
  }
  intcoeffs <- intcoeffs[order(rownames(intcoeffs)),]
  rownames(intcoeffs) <- paste("Neigh:",replace.all.labels(rownames(intcoeffs), custom.labels) )
  intcoeffs <- intcoeffs[, order(colnames(intcoeffs))]
  xtab[[model]] <- intcoeffs
}
xtab <- do.call(rbind, xtab)
xtab.twolines <- list()
for(i in 1:nrow(xtab)){
  xtab.twolines[[ paste("a",i) ]] <- gsub(" \\(.*?\\)", "",xtab[i,])
  xtab.twolines[[ paste("b",i) ]] <- gsub("^.*? ", "",xtab[i,])
}
xtab.twolines <- do.call(rbind, xtab.twolines)
colnames(xtab.twolines) <- replace.all.labels(colnames(xtab.twolines), custom.labels)
print(xtable(xtab.twolines))


########### combined era effect: direct + neigh + interaction
combined.coeffs <- function(eras, coeffs){
  comb <- matrix(0, length(eras), length(eras))
  colnames(comb) <- eras
  rownames(comb) <- eras
  for(e in eras){
    # building effect, direct
    if(!is.na(coeffs[paste("era",e, sep="")])){
      comb[,e] <- comb[,e] + coeffs[paste("era",e, sep="")]
    }
    # neighbourhood effect
    if(!is.na(coeffs[paste("nera",e, sep="")])){
       comb[e,] <- comb[e,] + coeffs[paste("nera",e, sep="")]
    }
    # interaction terms
    for(ee in eras){
      if(!is.na(coeffs[paste("era",e, sep="")]) & !is.na(coeffs[paste("nera",ee, sep="")])){
        comb[ee,e] <- comb[ee,e] + coeffs[paste("era",e,":nera",ee, sep="")]
      }    
    }
  }
  return(comb)
}  
coeffs <- rs_reg.set1[['int']]$coefficients
comb <- combined.coeffs(eras[order(eras)], coeffs)
#coeffs <- rs_reg.set1[['int.aug']]$coefficients
#comb.aug <- combined.coeffs(eras[order(eras)], coeffs)
#comb <- round(rbind(comb.aug, comb.int),3)
colnames(comb) <- c("Georgian","Early Vic.","Late Vic./Edw.","Interwar","Postwar","Contemporary","Revival")
rownames(comb) <- c("Georgian","Early Vic.","Late Vic./Edw.","Interwar","Postwar","Contemporary","Revival")
xtable(comb, digits=2)




tmp <- table(sales$neigh_era_aug, sales$true_era_aug)
tmp <- tmp[order(rownames(tmp)), order(rownames(tmp))]
colnames(tmp) <- replace.all.labels(colnames(tmp), custom.labels)
rownames(tmp) <- paste("Neigh:",replace.all.labels(rownames(tmp), custom.labels))
xtable(tmp)



######### SUMSTATS TABLE

sales.sumstat <- subset(sales, !is.na(pred_era))
sales.sumstat$x <- NULL
sales.sumstat$y <- NULL
sales.sumstat$centrX <- NULL
sales.sumstat$centrY <- NULL
sales.sumstat$toid <- NULL
sales.sumstat$toid_num <- NULL

sales.sumstat$newnum <- 0
sales.sumstat$newnum[sales.sumstat$new == "Y"] <- 1
type.f <- factor(sales.sumstat$propertytype)
dummies <- model.matrix(~type.f-1)
dummies <- dummies[, order(colnames(dummies))]
sales.sumstat <- cbind(sales.sumstat, dummies)


era.f <- factor(sales.sumstat$pred_era)
dummies <- model.matrix(~era.f-1)
dummies <- dummies[, order(colnames(dummies))]
sales.sumstat <- cbind(sales.sumstat, dummies)

nera.f <- factor(sales.sumstat$neigh_era)
dummies <- model.matrix(~nera.f-1)
dummies <- dummies[, order(colnames(dummies))]
sales.sumstat <- cbind(sales.sumstat, dummies)



stargazer(sales.sumstat, type="latex",
          title="Summary statistics residential property transactions",
          covariate.labels = c("Price","Year", 
                               "ln(volume)", "ln(area)", 
                               "ln(dist. city center)",
                               "New",
                               "Type: detached", "Type: semi-detached", "Type: terraced",
                               "Georgian","Early Vic.","Late Vic./Edw.","Interwar","Postwar","Contemporary","Faux Vic.",
                               "Neigh: Georgian","Neigh: Early Vic.","Neigh: Late V./Edw.","Neigh: Interwar","Neigh: Postwar","Neigh: Contemporary","Neigh: Faux Vic."),
          digits=2,
          out="~/research/paper-uk-vintages/text/sumstats.raw.tex"
)  


## confusion matrix
# tabulate predictons vs true era

tab.sales <- subset(sales, herf > herf.threshold, select=c("true_era","pred_era"))
tab <- table(tab.sales$pred_era, tab.sales$true_era)
rownames(tab) <- levels(sales$true_era)
colnames(tab) <- levels(sales$true_era)
tab <- tab[order(rownames(tab)), order(colnames(tab))]
# classification accuracy statistics
recall <- diag(tab)/colSums(tab)
precision <- diag(tab)/rowSums(tab)
f1scores <- 2*(precision*recall)/(precision+recall)
# percentages
tabpct <- tab
for(i in 1:ncol(tab)){
  tabpct[,i] <- round(tab[,i]/sum(tab[,i])*100, 1)
}
tab.tabpct <- rbind(tab, tabpct, recall, precision, f1scores)
xtable(tab.tabpct)


tab.sales <- subset(sales, herf > herf.threshold, select=c("true_era","pred_era","nera"))
tab <- table(tab.sales$nera, tab.sales$pred_era)
tab <- tab[order(rownames(tab)), order(colnames(tab))]
rownames(tab) <- c("Georgian","Early Vic.","Late Vic./Edw.","Interwar","Postwar","Contemporary","Revival")
xtable(tab)










### Hedonic regression analysis

library(xtable)
library(stargazer)

# load sales data
v <- read.csv("~/research/paper-uk-vintages/data/derived/predicted_styles.csv", as.is=TRUE)
v$pred_era <- v$pred4096
#s <- read.csv("https://www.dropbox.com/s/hu3mlar1n155w7m/sales_2018-10.csv?dl=1", as.is=TRUE)
s <- read.csv("~/Dropbox (Cambridge University)/Cambridge/data/derived/sales_2018-10.csv", as.is=TRUE)

s$toid <- s$TOID
s$TOID <- NULL
s$true_era <- NULL
sales <- merge( s, v,  by="toid", all.x=TRUE)
# remove flats or "other"
sales <- subset(sales, propertytype %in% c("D","S","T"))
sales$date <- as.Date(sales$date)
# remove sales with prices above 1m
sales <- subset(sales, price <= 1000000)


# define a unique ID for each property
sales$id <- apply(sales, 1, function(x){ paste(x['paon'], x['saon'], x['street'], x['postcode'] ) }) 

# age of building (new at the very first sale)
firstsaledate <- subset(sales, new=="Y", select=c("id","date"))
colnames(firstsaledate) <- c("id","dob")

# drop leaseholds
sales <- subset(sales, estatetype =="F")

# add lsoaid 
#lsoa <- read.csv("https://www.dropbox.com/s/8wfw8tdq2sgxza8/lsoa_x_toid.csv?dl=1", as.is=TRUE)
lsoa <- read.csv("~/Dropbox (Cambridge University)/Cambridge/data/derived/lsoa_x_toid.csv", as.is=TRUE)
sales <- merge(sales, lsoa, by="toid", all.x=TRUE)

# only keep sales with era predictions
sales <- subset(sales, !is.na(pred_era) )


############## REPEAT SALES
sales$md5 <- ""
sales <- subset(sales, select=c("id","price","date","pred_era","centrX", "centrY","toid","md5","lsoa11cd"))
sales <- sales[order( sales$id, sales$date),]

# create pairs
pairs <- cbind( sales[2:nrow(sales),], sales[1:(nrow(sales)-1),])
colnames(pairs) <- c("id","price2","date2","pred_era","centrX","centrY","toid","md5","lsoa","id1","price1","date1","pred_era1","centrX1","centrY1","toid1","md51","lsoa1")
pairs <- subset(pairs, id==id1)
pairs$id1 <- NULL
pairs$centrX1 <- NULL
pairs$centrY1 <- NULL
pairs$pred_era1 <- NULL
pairs$toid1 <- NULL
pairs$md51 <- NULL
pairs$lsoa1 <- NULL


# calculate return
pairs$ret <- log(pairs$price2/pairs$price1)
# calculate time between sales
pairs$days <- as.integer(pairs$date2-pairs$date1)
pairs$years <- round(pairs$days/365)

# create year dummies
pairs$year2 <- as.integer(format(pairs$date2, format="%Y"))
pairs$year1 <- as.integer(format(pairs$date1, format="%Y"))
pairs <- subset(pairs, year2 > year1)
year.range <- range(pairs$year1, pairs$year2)
years <- seq(from=year.range[1], to=year.range[2])
dummies <- matrix(0, nrow(pairs), length(years))
colnames(dummies) <- paste("y", years, sep="")
for(y in years){
  col <- paste("y", y, sep="")
  dummies[ pairs$year1 == y,col] <- -1  
  dummies[ pairs$year2 == y,col] <- 1
  dummies[ pairs$year1 == y & pairs$year2 == y,col] <- 0
}
pairs <- cbind(pairs, dummies)


# flag pairs of buildings older than the start of sample
pairs$nevernew <- 1
pairs$nevernew[pairs$id %in% firstsaledate$id] <- 0
pairs$new <- (pairs$nevernew-1)^2
pairs$newcontemp <- 0
pairs$newcontemp[ pairs$pred_era == "f contemporary" & pairs$new == 1] <- 1


# calculate age at sales
p <- merge(pairs, firstsaledate, by="id", all.x=TRUE)
p$agesale1 <- round(as.integer( p$date1 - p$dob)/365) 
p$agesale2 <- round(as.integer( p$date2 - p$dob)/365) 
p <- subset(p, agesale1 >= 0 | is.na (agesale1))
p <- subset(p, agesale2 >= 0 | is.na (agesale1))
p$agediff <- p$agesale2 - p$agesale1

#dummies.cols <- colnames(p)
#dummies.cols <- dummies.cols[grepl("^[y]\\d+$",dummies.cols, perl=TRUE)]
#dummies.cols.str <- paste(dummies.cols, collapse=" + ")
#reg.age.exp <- lm(as.formula( paste("ret~-1+",dummies.cols.str,"+factor(agediff)", sep="")), data=p)
#summary(reg.age.exp)




age.range <- range(0,p$agesale2, na.rm=TRUE)
ages <- seq(from=age.range[1], to=age.range[2])
dummies <- matrix(0, nrow(p), length(ages))
colnames(dummies) <- paste("a", ages, sep="")
for(a in ages){
  col <- paste("a", a, sep="")
  dummies[ p$agesale1 == a, col] <- -1  
  dummies[ p$agesale2 == a, col] <- 1
  dummies[ p$agesale1 == a & p$agesale2 == a,col] <- 0
}
pa <- as.data.frame( cbind(p, dummies))
dummies.cols <- colnames(pa)
dummies.cols <- dummies.cols[grepl("^[ya]\\d+$",dummies.cols, perl=TRUE)]
dummies.cols.str <- paste(dummies.cols, collapse=" + ")
reg.age <- lm(as.formula( paste("ret~-1+",dummies.cols.str, sep="")), data=pa)
summary(reg.age)
age.coeffs <- reg.age$coefficients[grepl('^a\\d+$', names(reg.age$coefficients), perl=TRUE)]
year.coeffs <- reg.age$coefficients[grepl('^y\\d+$', names(reg.age$coefficients), perl=TRUE)]
plot(exp(year.coeffs), type="l")
plot(exp(age.coeffs), type="l")

# in 5 year bands
ages <- c(5,10,15,20,25)
dummies <- matrix(0, nrow=nrow(pa), ncol=length(ages))
colnames(dummies) <- paste("aa", ages, sep="")
for(a in ages){
  col <- paste("aa", a, sep="")
  dummies[ p$agesale1 < a & p$agesale1 >= (a-5) , col] <- -1  
  dummies[ p$agesale2 < a & p$agesale2 >= (a-5), col] <- 1
  dummies[ p$agesale1 < a & p$agesale1 >= (a-5) & p$agesale2 < a & p$agesale2 >= (a-5) ] <- 0
}
pa <- as.data.frame( cbind(p, dummies))
# drop base dummies
pa$y1995 <- NULL
pa$aa5 <- NULL
pa$era <- pa$pred_era2
pa$era[! pa$pred_era2 %in% c("g cont victorian","f contemporary")] <- "historic"
dummies.cols <- colnames(pa)
dummies.cols.years <- dummies.cols[grepl("^y\\d+$",dummies.cols, perl=TRUE)]
dummies.cols.str.years <- paste(dummies.cols.years, collapse=" + ")
dummies.cols.age <- dummies.cols[grepl("^aa\\d+$",dummies.cols, perl=TRUE)]
dummies.cols.str.age <- paste(dummies.cols.age, collapse=" + ")
reg.age <- lm(as.formula( paste("ret~-1+",dummies.cols.str.years,"+",dummies.cols.str.age, sep="")), data=pa)
summary(reg.age)
age.coeffs <- reg.age$coefficients[grepl('^aa\\d+$', names(reg.age$coefficients), perl=TRUE)]
year.coeffs <- reg.age$coefficients[grepl('^y\\d+$', names(reg.age$coefficients), perl=TRUE)]
# -------
dummies.cols.str.age.int <- paste(dummies.cols.age, collapse="*newcontemp + ")
dummies.cols.str.age <- paste(dummies.cols.age, collapse=" + ")
reg.age.cont <- lm(as.formula( paste("ret~-1+lsoa+",dummies.cols.str.years,"+",dummies.cols.str.age,"+new+",dummies.cols.str.age.int,"*newcontemp", sep="")), data=pa)
summary(reg.age.cont)
age.coeffs.base <- reg.age.cont$coefficients[grepl('^aa\\d+$', names(reg.age.cont$coefficients), perl=TRUE)]
age.coeffs.int <- age.coeffs.base 
for(a in ages){
  col <- paste("aa", a, sep="")
  try( age.coeffs.int[col] <- reg.age.cont$coefficients[col] +reg.age.cont$coefficients["newcontemp"]  + reg.age.cont$coefficients[ grepl(col, names(reg.age.cont$coefficients)) & grepl("newcontemp", names(reg.age.cont$coefficients)) ])
}
age.coeffs.base.scaled <- exp(c(0, age.coeffs.base))
age.coeffs.int.scaled <- exp(c(0,age.coeffs.int))


# base repeat sales regression
dummies.cols.str <- paste(years[2:length(years)], collapse="+y")
reg.base <- lm(as.formula( paste("ret~-1+lsoa+y",dummies.cols.str, sep="")), data=pa)
summary(reg.base)
# repeat sales regression, with era
reg.base.era <- lm(as.formula( paste("ret~-1+lsoa+y",dummies.cols.str,"+pred_era", sep="")), data=pa)
summary(reg.base.era)




colour <- c("#230356","#076391","#2fd1d1","#b51676","#e90d3a","#f98e19","#ffc217")

# general price index
ind <- reg.age.cont$coefficients[grepl("^y", names(reg.age.cont$coefficients), perl=TRUE) ]
ind <- exp(c(0, ind))*100
x<-1995:2018
jpeg("~/research/paper-cambridge-vintages/text/figures/repsales.index.jpg", width=700, height=250, quality=100)
  par(mar=c(5.1,4.1,1,2.1))
  plot(x,ind, type="l", ylab="1995=100", xlab="Year", col=colour[5], lwd=3)
dev.off()

# plot decay of new buildings
jpeg("~/research/paper-cambridge-vintages/text/figures/repsales.newvsold.jpg", width=700, height=250, quality=100)
  par(mar=c(5.1,4.1,1,2.1))
  plot(c(0,4)*5,c(-25,0), type="n", xaxt = "n", ylab="Return new vs. second hand (%)", xlab="Age of building")
  axis(1, at=(0:4)*5, labels=paste((0:4)*5,(1:5)*5-1, sep="-"))
  lines((0:4)*5, age.coeffs.base.scaled*100-100, col=colour[2], lwd=3)
  lines((0:4)*5, age.coeffs.int.scaled*100-100, col=colour[3], lty=3, lwd=3 )
  legend("bottomleft",c("Contemporary","All other styles"), lwd=5, col=colour[3:2], lty=c(3,1), bty="n")
dev.off()

# regression table output
stargazer(reg.base.era, reg.age.cont,
          omit="y1|y2|lsoa",
          type="latex",
          #type="text",
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent variable: $ln(price_{t2}) - ln(price_{t1})$",
          se=list( sqrt(diag( vcovHC(reg.base.era  , type = "HC1"))),
                   sqrt(diag( vcovHC(reg.age.cont  , type = "HC1")))),
          out = "/home/thies/research/paper-cambridge-vintages/text/repsales_raw.tex",
          intercept.bottom=FALSE,
          keep.stat=c("adj.rsq", "n"),
          digits=2,
          title="Repeat Sales Regression Estimates",
          font.size="footnotesize",
          label="tab:repreg",
          add.lines = list(c("Year dummies", "Yes","Yes"), c("Neigh. dummies", "Yes","Yes")),
          covariate.labels = c("Early Vic.","Late Vic./Edw.","Interwar","Postwar","Contemporary","Faux Vic.",
                              "Age 5-9",
                               "Age 10-14",
                               "Age 15-19",
                               "Age 20-24",
                              "new",
                               "New contemporary",
                               "New cont. age 5-9",
                               "New cont. age 10-14",
                               "New cont. age 15-19",
                               "New cont. age 20-24")
)




# Load OS AddressBase data (stored on Dropbox)
library(data.table)
library(stringr)
# Postal Addressess
ab <- fread("https://www.dropbox.com/s/4328cqcd3j8auam/ID28_DPA_Records.csv?dl=1")
ab <- readRDS('~/Dropbox/pkg.data/cambridge/Raw/ID28_DPA_Records.rds')
setkey(ab, UPRN)
# TOID
xref <- fread("https://www.dropbox.com/s/ng6k8ud90wehoyf/ID23_XREF_Records.csv?dl=1")
xref <- readRDS('~/Dropbox/pkg.data/cambridge/Raw/ID23_XREF_Records.rds')
xref <- xref[grepl('osgb', CROSS_REFERENCE), ][,.(UPRN, CROSS_REFERENCE)]
setkey(xref, UPRN)
a <- copy(xref[ab])
a <- as.data.table(a)
a$TOID <- gsub('osgb','', a$CROSS_REFERENCE)
a <- a[nchar(TOID)==13, TOID:=str_pad(TOID,width = 16,side = 'left',pad = '0')]
table(as.character(nchar(a$TOID)))
table(str_sub(a[nchar(TOID)==13]$TOID,1,4))
table(str_sub(a[nchar(TOID)==16]$TOID,1,4))
table(str_sub(a$TOID, 1, 4))
# Subset a to only inludde leading digits same as building.data
a <- a[str_sub(a$TOID, 1, 4) != '4000']

# Check the actual ids in the map
building.data <- as.data.table(read.dbf(file = '~/Dropbox/pkg.data/cambridge/Raw/os_cambridge_buildings.dbf', as.is=TRUE))
setkey(building.data, TOID)

shape.address <- a[building.data]
shape.address <- shape.address[!is.na(CROSS_REFERENCE)]
write.csv(shape.address, file='data/shape.address.csv')

# Check first four digits of TOID in building data
table(nchar(building.data$TOID))
table(str_sub(building.data$TOID, 1, 4))

# Check how many TOIDs from a are not in building.data
cat('There are ', length(a$TOID[a$TOID %in% building.data$TOID]), 'a$TOID in building.data$TOID', 
    'and', length(a$TOID[!(a$TOID %in% building.data$TOID)]), 'that are unmatched')

# Examine the unmatched
a.TOID.noMatch <- a$TOID[!(a$TOID %in% building.data$TOID)]
a.TOID.match <- a$TOID[a$TOID %in% building.data$TOID]
table(str_sub(a$TOID,1, 4))
table(str_sub(a.TOID.noMatch, 1, 4))
table(str_sub(a.TOID.match, 1, 4))
mapply(function(x, y) x/y, x=table(str_sub(a.TOID.match, 1, 4)), y=table(str_sub(a$TOID,1, 4)))
# a <- subset(a, nchar(TOID) == 13)

# read sales data (Dropbox again)A
# sales <- fread("https://www.dropbox.com/s/vsrtks68vwxcd24/sales_cambridge.csv?dl=1")
sales <- readRDS('~/Dropbox/pkg.data/cambridge/Raw/sales_cambridge.rds')
colnames(sales) <- c("sales_id","price","date","postcode","propertytype","new","estatetype","paon","saon","street","locality","town","district","county","PPDcategory","recordstatus")
sales <- sales[, date:=strptime(date,"%Y-%m-%d")]
sales$year <- factor( as.numeric(format(sales$date, "%Y")))
sales$houseno <- gsub("[^\\d]","", sales$paon, perl=TRUE)
sales <- as.data.table(sales)

l.mapped <- list()
l.sales <- list()
l.sales$base <- copy(sales)
l.a <- list()
l.mapped.ids <- list()

# Clean up the 'a' lookup table a bit
# First if building number is na
# This is basically the join that Thies originally did
l.a$first <- copy(a[!is.na(BUILDING_NUMBER), a_key := paste(BUILDING_NUMBER, POSTCODE)])
l.a$first <- l.a$first[ , a_key_n := str_length(a_key)]
setkey(l.a$first, a_key)

l.sales$first <- copy(l.sales$base[postcode!='' & paon !=''])
l.sales$first <- l.sales$first[, sales_key := paste(paon, postcode)]
l.sales$first <- l.sales$first[, sales_key_n := str_length(sales_key)]
setkey(l.sales$first, sales_key)

l.mapped$first <- l.a$first[l.sales$first]
l.mapped$first <- l.mapped$first[!(is.na(TOID)), .(sales_id, TOID)]
setkey(l.mapped$first, sales_id, TOID)
l.mapped$first <- unique(l.mapped$first)
# Check for multiple matches
# This one had 30 TOID matches (Reduced to 2 with unique call)
l.mapped$first[sales_id=='{350DA8AF-A815-491D-942F-28BF2778E56B}']
sales[sales_id=='{350DA8AF-A815-491D-942F-28BF2778E56B}']
which(l.mapped$first[sales_id=='{350DA8AF-A815-491D-942F-28BF2778E56B}']$TOID %in% building.data$TOID)

l.mapped$first <- l.mapped$first[, n_sales_matches := .N, by=sales_id]
l.mapped$first <- l.mapped$first[TOID %in% building.data$TOID, n_building_matches :=.N, by=sales_id]
table(l.mapped$first$n_sales_matches)
table(l.mapped$first$n_building_matches)
l.mapped$first <- l.mapped$first[is.na(n_building_matches), n_building_matches := 0]
#l.mapped$first <- l.mapped$first[n_building_matches > 0]
mapped.ids <- unique(unlist(sapply(l.mapped, function(x) x$sales_id, simplify = TRUE)))
cat('First Join \n', length(mapped.ids), 'matches',
    length(unique(sales[!(sales_id %in% mapped.ids)]$sales_id)), 'misses')

# Next, Try and match the following:
# Easier to restrict a to the set that have building matches

l.sales$second <- l.sales$first[!(sales_id %in% mapped.ids)]
# Example missing match
l.sales$second[sales_id=='{2A289EA0-E74A-CDC8-E050-A8C063054829}']
l.a$first[POSTCODE=='CB1 2EZ']

# Has sales$paon = '1 - 2'
# Has l.a$second$BUILDING_NAME = '1-2'
# Has l.a$second$BUILDING_NUMBER = NA
# First, remove all spaces from paon
l.sales$second <- l.sales$second[, paon:=gsub(' - ', '-',x = paon)]
# Second, how many building numbers are NA?
cat('There are ', nrow(l.a$first[is.na(BUILDING_NUMBER)]), ' NA Building Numbers in a')
# If this is the case, start by using BUILDING_NAME in a
l.a$first <- l.a$first[is.na(BUILDING_NUMBER), a_key := paste(BUILDING_NAME, POSTCODE)]
setkey(l.a$first, a_key)
l.sales$second <- l.sales$second[, sales_key := paste(paon, postcode)]
setkey(l.sales$second, sales_key)
l.mapped$second <- l.a$first[l.sales$second]
l.mapped$second <- l.mapped$second[!(is.na(TOID)), .(sales_id, TOID)]
setkey(l.mapped$second, sales_id, TOID)
l.mapped$second <- l.mapped$second[!(is.na(TOID))]
l.mapped$second <- unique(l.mapped$second)

mapped.ids <- unique(unlist(sapply(l.mapped, function(x) x$sales_id, simplify = TRUE)))
cat('Second Join \n', length(unique(mapped.ids)), 'matches',
    length(unique(sales[!(sales_id %in% mapped.ids)]$sales_id)), 'misses')

# Third match
# Next, Try and match the following:
# Easier to restrict a to the set that have building matches
l.sales$third <- l.sales$second[!(sales_id %in% mapped_ids)]
l.sales$third[sales_id=='{8AAEC3FB-4FC1-46EE-9444-BE310B60012E}']
# BUILDING_NAME=1 PARKSIDE PLACE; need tgo extract the 1
l.a$first <- l.a$first[, BUILDING_NAME_num := str_extract(BUILDING_NAME, regex('(?<=^)[1-9]{1,}(?= )', perl=TRUE))]
l.a$first[POSTCODE=='CB1 1HQ']
l.a$first <- l.a$first[, a_key := paste(BUILDING_NAME_num, POSTCODE)]
setkey(l.a$first, a_key)
setkey(l.sales$third, sales_key)
l.mapped$third <- l.a$first[l.sales$third]
l.mapped$third <- l.mapped$third[!(is.na(TOID)), .(sales_id, TOID)]
setkey(l.mapped$third, sales_id, TOID)
l.mapped$third <- l.mapped$third[!(is.na(TOID))]
l.mapped$third <- unique(l.mapped$third)

mapped.ids <- unique(unlist(sapply(l.mapped, function(x) x$sales_id, simplify = TRUE)))
cat('Third Join \n', length(unique(mapped.ids)), 'matches',
    length(unique(sales[!(sales_id %in% mapped.ids)]$sales_id)), 'misses')

# Fourth match (explorator does not match)
# Appears that new==Y may not have shapes in data yet
l.sales$fourth <- l.sales$third[!(sales_id %in% mapped.ids)]
l.sales$fourth[sales_id=='{4E95D757-2922-EDA1-E050-A8C0630539E2}']
l.a$first[POSTCODE=='CB1 3FD']
# Appear to be new construcion
# https://idox.cambridge.gov.uk/online-applications/propertyDetails.do?activeTab=relatedCases&keyVal=NVULI0DX05I00
table(sales[sales_id %in% mapped.ids]$new)
table(sales[!(sales_id %in% mapped.ids)]$new)
table(ab$DEPENDENT_LOCALITY)

# Dump some sales for 'good reasons'
# Focus on sales with new==N
l.sales$fifth <- l.sales$fourth[new=='N']
#l.sales$fifth[sales_id=='{17E1EA0E-F28F-4F02-8620-97E85778318E}']
# Find all post codes not in ab but in the sales
sales_postcode <- unique(sales$postcode)
sales_postcode_no_match <- sales_postcode[!(sales_postcode %in% a$POSTCODE)]
sales_postcode_no_match <- unique(sales_postcode_no_match)
l.mapped.ids$bad <- copy(l.sales$fifth[(postcode %in% sales_postcode_no_match)])
l.sales$fifth <- l.sales$fifth[!(postcode %in% sales_postcode_no_match)]

mapped.ids <-c(mapped.ids, l.mapped.ids$bad)
# Dump not in geographically relevant
l.sales$sixth <- l.sales$fifth[!(sales_id %in% mapped.ids)]
# Remove more 'bad' and not geographically relevant paon
l.mapped.ids$bad <- c(l.mapped.ids$bad$sales_id, l.sales$sixth[str_detect(paon, '^[0-9]{1,}$')]$sales_id)
mapped.ids <-c(mapped.ids, l.mapped.ids$bad)
l.sales$seventh <- l.sales$fifth[!(sales_id %in% mapped.ids)]
l.sales$seventh <- l.sales$seventh[, sales_key:=paste(paon, street, postcode)]

l.sales$seventh[sales_id=='{40112D4A-E56F-4005-B2DB-EE4CDD283DDB}']
l.a$first[POSTCODE == 'CB3 0AH']
l.a$first <- l.a$first[, a_key:=paste(str_replace(BUILDING_NAME, '\\.', ''), POSTCODE)]
setkey(l.a$first, a_key)
setkey(l.sales$seventh, sales_key)

l.mapped$fourth <- l.a$first[l.sales$seventh]
l.mapped$fourth <- l.mapped$fourth[!(is.na(TOID)), .(sales_id, TOID)]
setkey(l.mapped$fourth, sales_id, TOID)
l.mapped$fourth <- l.mapped$fourth[!(is.na(TOID))]
l.mapped$fourth <- unique(l.mapped$fourth)

mapped.ids <- unique(unlist(sapply(l.mapped, function(x) x$sales_id, simplify = TRUE)))
cat('Join \n', length(unique(mapped.ids)), 'matches')

# Fifth clean
dt.mapped <- rbindlist(l.mapped, use.names = TRUE, fill = TRUE)
dt.mapped <- dt.mapped[!is.na(n_sales_matches)]
save(dt.mapped, file='~/Dropbox/pkg.data/cambridge/Clean/dt.mapped.rdata')

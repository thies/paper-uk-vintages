# load data, in case it needs to be updated: source(augment_sales_data.R)
s <- read.csv("https://www.dropbox.com/s/4sdxm6x5lfhesrp/sales_toids_eras.csv?dl=1")
s$date <- as.Date(s$date)
s$year <- format(s$date, format="%Y")


newTOIDs <- unique( s$TOID[s$new=="Y" & !is.na(s$TOID)] )



# ======================= WHICH LSOA does the buidling belong to?
# in case a building is on the boundary, pick the one with the bigger overlap
library('RPostgreSQL')
con <- dbConnect(PostgreSQL(), user= "thies", dbname="gis", password='gis', port=5432)
for( toid in newTOIDs){
  sql <- paste("UPDATE cbg_os_buildings SET new = TRUE where cast(toid as numeric) = ",toid, sep="")
  print(sql)
  dbGetQuery(con, sql)
}

# ALTER TABLE cbg_os_buildings ADD COLUMN geom_buffer_15m geometry(Geometry,27700)
# UDPATE cbg_os_buildings SET geom_buffer_15m = ST_BUFFER( geom, 15);
# CREATE INDEX cbg_os_buildings_geom_buffer_15m ON cbg_os_buildings USING GIST (geom_buffer_15m);



sql <- "SELECT
          a.toid AS neigh_toid,
          n.toid AS infill_toid
        FROM
          cbg_os_buildings  as a,
          cbg_os_buildings as n
        WHERE 
          n.new = TRUE
          AND n.toid != a.TOID
          AND ST_Intersects(n.geom_buffer_15m, a.geom)"

infill_neighbours <- dbGetQuery(con, sql)

write.csv(infill_neighbours, file="~/db/Cambridge/data/derived/infill_15m.csv", row.names=FALSE)
# =============================================================================








## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, fig.align = 'center')
knitr::opts_chunk$set(cache.extra = R.version.string)


## ----pack---------------------------------------------------------------------------------------------------------------------------------------
library(sf)
library(rgdal)
library(terra) 
library(tidyverse)


## ----drive--------------------------------------------------------------------------------------------------------------------------------------
ogr.d <- ogrDrivers()
ogr.d[ix <- which(ogr.d$name=="OpenFileGDB"), ]


## -----------------------------------------------------------------------------------------------------------------------------------------------
base.dir.gsm05 <- "/Users/rossiter/ds/GSM_USA/"
base.dir.gsm05.import <- "/Volumes/Pythagoras/ds/GSM_USA/"


## ----find.db------------------------------------------------------------------------------------------------------------------------------------
gdb.name <- paste0(base.dir.gsm05.import, "GSM_34LRH_20180606094754.gdb")
file.info(gdb.name)[c("size", "mtime")]
(db.layers <- ogrListLayers(gdb.name))


## ----read.db------------------------------------------------------------------------------------------------------------------------------------
if (!exists("db")) {
  cat("Loading attribute database...")
  system.time(db <- sf::st_read(gdb.name))
} else {
  cat("Using previously-loaded attribute database.")
}


## ----info.db------------------------------------------------------------------------------------------------------------------------------------
class(db)
dim(db)
names(db)[1:13]
table(db$mukind)  # some have no map unit kind!
head(db$muname)
head(db$mukey)


## ----prop.names---------------------------------------------------------------------------------------------------------------------------------
ix <- grep("clay", names(db))
sort(names(db)[ix])


## ----prop.props---------------------------------------------------------------------------------------------------------------------------------
ix <- grep("mu_sum_comppct_r_fix_claytotal_[r,l,h]", names(db))
names(db)[ix]


## ----comppct------------------------------------------------------------------------------------------------------------------------------------
db$mu_sum_comppct_r_fix_claytotal_r[16:20]
db$mu_sum_comppct_r_fix_claytotal_l[16:20]
db$mu_sum_comppct_r_fix_claytotal_h[16:20]


## ----voi.list-----------------------------------------------------------------------------------------------------------------------------------
voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
voi.list.gsm <- c("claytotal_r_g_kg", "silttotal_r_g_kg", "sandtotal_r_g_kg",
                  "ph1to1h2o_r_ions_pHx10", "ecec_r_cmolc_kg", "soc_r_mr_g_gF", 
                  "dbthirdbar_lt2mm_r_g_cm3", "gravel_r_vol_ratio_m3_m3")


## ----voi----------------------------------------------------------------------------------------------------------------------------------------
voi <- 1
(voi.name <- paste0("mu_", voi.list.gsm[voi], "_"))


## -----------------------------------------------------------------------------------------------------------------------------------------------
dim(db)
head(names(db))
db.attr <- dplyr::select(db, matches(c("mukeyint", voi.name)))
names(db.attr)
dim(db.attr)
# the 2nd attribute is typical of all depths
# set the NoData values
na.val <- ifelse(is.integer(db.attr[,2]), 2^16-1, 
                 ifelse(is.double(db.attr[,2]), min(db.attr[,2]),NA))
db.attr <- db.attr %>%
  na_if(., na.val)
# summary(db.attr)


## ----depth.list---------------------------------------------------------------------------------------------------------------------------------
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
depth.list.gsm <- c("000_005", "005_015", "015_030", "030_060", "060_100", "100_200")


## ----depth--------------------------------------------------------------------------------------------------------------------------------------
depth <- 1
(voi.depth.name <- paste0(voi.name, depth.list.gsm[depth]))


## ----tile.1-------------------------------------------------------------------------------------------------------------------------------------
## Lower-right corner, in integer degrees
#
tile.lrc <- c(-77, 35) # lower-right corner
#
## Tile size, in integer degrees
# 
size.long <- 1; size.lat <- 1


## ----tile.2-------------------------------------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-size.long, tile.lrc[2]+size.lat) # upper-left corner
m <- matrix(c(tile.ulc[1],tile.lrc[1],  #ulc
              tile.ulc[2], tile.lrc[2]  #lrc
              ),
            nrow=2)
bb.ll <- st_sfc(st_multipoint(m))
st_crs(bb.ll) <- 4326


## -----------------------------------------------------------------------------------------------------------------------------------------------
grid.name <- paste0(base.dir.gsm05.import, "mukeyint_SaS_3as/mukeyint_SaS_3as.tif")
file.info(grid.name)[c("size", "mtime")]


## ----read.grid----------------------------------------------------------------------------------------------------------------------------------
r <- rast(grid.name)
ext(r)
crs(r)
dim(r)
res(r)
res(r)*60*60


## -----------------------------------------------------------------------------------------------------------------------------------------------
res(r)[1]*(10000/90)*1000  # at the equator
res(r)[1]*(10000/90)*1000*cos((pi/180)*30)


## ----name.grid----------------------------------------------------------------------------------------------------------------------------------
names(r) <- "mukeyint"


## ----clip.grid----------------------------------------------------------------------------------------------------------------------------------
bb.vect <- as.vector(matrix(st_bbox(bb.ll), nrow=2, byrow=T))
r.crop <- terra::crop(r, bb.vect)
dim(r.crop)
length(unique(r.crop))
head(r.crop, rows=8, cols=8)


## ----link---------------------------------------------------------------------------------------------------------------------------------------
m.reclass <- data.frame(from=db.attr[,1],
                    to=db.attr[, match(voi.depth.name, names(db.attr))])
dim(m.reclass)
m.reclass[1:6,]
system.time(
  r.attr <- classify(r.crop, m.reclass)
)
plot(r.attr)
summary(r.attr)


## -----------------------------------------------------------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## ----save---------------------------------------------------------------------------------------------------------------------------------------
dest.dir <-  paste0(base.dir.gsm05, AOI.dir.prefix)
if (!dir.exists(dest.dir)) {
   dir.create(dest.dir, recursive = TRUE)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------
f <- terra::writeRaster(r.attr, file=paste0(dest.dir, "/GSM_",
                                              voi.depth.name, ".tif"),
                        overwrite=TRUE, datatype="INT2U", options=c("TFW=YES"),
                        filetype="GTIFF")
print(f)


## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      fig.align = 'center')
knitr::opts_chunk$set(cache.extra = R.version.string)


## -----------------------------------------------------------------------------------------------------------------------------------------------
options("rgdal_show_exportToProj4_warnings"="none") 
library(zen4R)          # access Zenodo
library(sf)             # spatial data types 
library(terra)          # raster data, replaces `raster`


## -----------------------------------------------------------------------------------------------------------------------------------------------
base.dir <- "/Users/rossiter/ds/"
base.dir.landgis <- paste0(base.dir, "LandGIS")
base.dir.import <- "/Volumes/Pythagoras/ds/"
base.dir.landgis.import <- paste0(base.dir.import, "LandGIS")


## -----------------------------------------------------------------------------------------------------------------------------------------------
voi.list.landgis <- c("clay.wfraction_usda.3a1a1a",
                      "silt.wfraction_usda.3a1a1a",
                      "sand.wfraction_usda.3a1a1a",
                      "ph.h2o_usda.4c1a2a",
                      "organic.carbon_usda.6a1c",
                      "bulkdens.fineearth_usda.4a1h",
                      "coarsefrag.vfraction_usda_3b1")
voi.list.landgis.code <- c(1476854, 2525675, 1476851,
                           1475459, 1475457, 1475970,
                           2525681)
voi.list.sg <- c("clay", "silt", "sand", "phh2o", "soc",  "bdod", "cfvo")


## -----------------------------------------------------------------------------------------------------------------------------------------------
voi.n <- 1
voi.sg <- voi.list.sg[voi.n]
voi.landgis <- voi.list.landgis[voi.n]
voi.landgis.code <- voi.list.landgis.code[voi.n]


## -----------------------------------------------------------------------------------------------------------------------------------------------
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
depth.list.landgis <- c("0", "10", "30", "60", "100", "200")


## -----------------------------------------------------------------------------------------------------------------------------------------------
depth <- 1
depth.sg <- depth.list.sg[depth]
if (depth == 1) {
  top <- 1; bottom <- 2; weights <- c(2/3, 1/3)  
} else if (depth == 2) { # both the same
  top <- bottom <- 2; weights <- c(1/2, 1/2)
} else if (depth == 3) {
  top <- 2; bottom <-3; weights <- c(1/3, 2/3) 
} else if (depth == 4) {
  top <- 3; bottom <- 4; weights <- c(1/2, 1/2)
} else if (depth == 5) {
  top <- 4; bottom <- 5; weights <- c(1/2, 1/2)
} else if (depth == 6) {
  top <- 5; bottom <- 6; weights <- c(1/2, 1/2)
}


## ----slice.file.names---------------------------------------------------------------------------------------------------------------------------
depth.landgis.top <- depth.list.landgis[top]
depth.landgis.bottom <- depth.list.landgis[bottom]
depth.landgis.suffix.top <- paste0("b", depth.landgis.top, "..", depth.list.landgis[top], "cm")
depth.landgis.suffix.bottom <- paste0("b", depth.landgis.bottom, "..", depth.list.landgis[bottom], "cm")


## ----lrc----------------------------------------------------------------------------------------------------------------------------------------
tile.lrc <- c(-77, 35)   # lower-right corner
tile.size <- 1


## ----ulc----------------------------------------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-tile.size, tile.lrc[2]+tile.size) # upper-left corner


## ----zenodo.new---------------------------------------------------------------------------------------------------------------------------------
(filename.in.top <- paste0(base.dir.landgis.import, 
                          "/sol_", voi.landgis, 
                          "_m_250m_", depth.landgis.suffix.top,
                          "_1950..2017_v0.2.tif"))
if (!file.exists(filename.in.top)) {
  system.time(
  download_zenodo(paste0("10.5281/zenodo.",voi.landgis.code),
                  path=base.dir.landgis.import,
                  logger="INFO")
  ) } else print("File previously downloaded")
(filename.in.bottom <- paste0(base.dir.landgis.import, 
                          "/sol_", voi.landgis, 
                          "_m_250m_", depth.landgis.suffix.bottom,
                          "_1950..2017_v0.2.tif"))
if (!file.exists(filename.in.bottom)) {
  system.time(
  download_zenodo(paste0("10.5281/zenodo.",voi.landgis.code),
                  path=base.dir.landgis.import,
                  logger="INFO")
  ) } else print("File previously downloaded")


## -----------------------------------------------------------------------------------------------------------------------------------------------
tools::md5sum(filename.in.top)
tools::md5sum(filename.in.bottom)


## ----in.slice1----------------------------------------------------------------------------------------------------------------------------------
r.top <- terra::rast(filename.in.top)
r.bottom <- terra::rast(filename.in.bottom)
print(r.top)


## -----------------------------------------------------------------------------------------------------------------------------------------------
m <- matrix(c(tile.ulc[1],tile.lrc[1],  #ulc
              tile.ulc[2], tile.lrc[2]),  #lrc
            nrow=2)
bb.ll <- st_sfc(st_multipoint(m))
st_crs(bb.ll) <- 4326
bb.vect <- as.vector(matrix(st_bbox(bb.ll), nrow=2, byrow=T))
bb.ll.ext <- ext(bb.vect)


## ----crop---------------------------------------------------------------------------------------------------------------------------------------
r.top.crop <- terra::crop(r.top, bb.ll.ext)
r.bottom.crop <- terra::crop(r.bottom, bb.ll.ext)


## ----average------------------------------------------------------------------------------------------------------------------------------------
r.landgis.crop <- r.top.crop*weights[1] + r.bottom.crop*weights[2]


## ----plot.properties----------------------------------------------------------------------------------------------------------------------------
print(r.landgis.crop)
summary(r.landgis.crop)
terra::plot(r.landgis.crop,
            main=paste0("LandGIS, ", voi.landgis, ", depth ", depth.sg))


## -----------------------------------------------------------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## -----------------------------------------------------------------------------------------------------------------------------------------------
(dest.dir.landgis <-  paste0(base.dir.landgis, "/", AOI.dir.prefix))
if (!dir.exists(dest.dir.landgis)) {
  dir.create(dest.dir.landgis, recursive = TRUE)}


## -----------------------------------------------------------------------------------------------------------------------------------------------
(file.name <- paste0(voi.landgis, "_", depth.sg))
f <- writeRaster(r.landgis.crop, file=paste0(dest.dir.landgis,"/",
                                             file.name, ".tif"),
                 overwrite=TRUE, wopt=list(gdal=c("TFW=YES")),
                 filetype="GTiff")


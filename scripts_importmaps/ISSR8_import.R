## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, fig.align = 'center')
knitr::opts_chunk$set(cache.extra = R.version.string)


## -----------------------------------------------------------------------------------------------------------------------------------------------
library(sf)     # Simple Features representation of spatial data
library(terra)  # for raster import and display


## -----------------------------------------------------------------------------------------------------------------------------------------------
base.dir.import <- "/Volumes/Pythagoras/ds/"
base.dir.issr8.import <- paste0(base.dir.import, "ISSR8")
base.dir <- "/Users/rossiter/ds/"
base.dir.issr8 <- paste0(base.dir, "ISSR8")


## -----------------------------------------------------------------------------------------------------------------------------------------------
if (!dir.exists(base.dir.issr8.import)) {
   dir.create(dbase.dir.issr8.import,recursive = TRUE)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------
# clay, silt, sand, pH, CEC, SOC, bulk density, coarse fragments

voi.list.issr8 <- c("clay", "silt","sand","ph", "cec", "ec", "rf", "paws", "sar")


## -----------------------------------------------------------------------------------------------------------------------------------------------
voi.issr8 <- "clay"


## -----------------------------------------------------------------------------------------------------------------------------------------------
depth.list.issr8 <- c("05", "025", "2550", "3060")


## -----------------------------------------------------------------------------------------------------------------------------------------------
depth.issr8 <- "05"


## ----base.url-----------------------------------------------------------------------------------------------------------------------------------
base.url <- "http://soilmap2-1.lawr.ucdavis.edu/800m_grids/rasters/"


## -----------------------------------------------------------------------------------------------------------------------------------------------
issr8.map <- paste0(voi.issr8, "_", depth.issr8, ".tif")


## ----download.conus-----------------------------------------------------------------------------------------------------------------------------
(dest.file <- paste0(base.dir.issr8.import, "/", issr8.map))
if (!file.exists(dest.file)) {
   download.file(
      url = 
         paste0(base.url, issr8.map), 
      destfile = dest.file,
      extra = "--max-redirect=0",
      method = "auto")
} else {
   print("Local copy of file already exists")
}


## ----plot.properties.issr8.import---------------------------------------------------------------------------------------------------------------
r.issr8 <- terra::rast(dest.file)
print(r.issr8)
terra::plot(r.issr8, main=paste("ISSR-800;",  main=voi.issr8))


## ----lrc----------------------------------------------------------------------------------------------------------------------------------------
tile.lrc <- c(-77, 35) # lower-right corner
size <- 1                # tile dimensions


## ----ulc----------------------------------------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-size, tile.lrc[2]+size) 


## ----dir.prefix---------------------------------------------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## -----------------------------------------------------------------------------------------------------------------------------------------------
(dest.dir.issr8 <-  file.path(base.dir.issr8,
                       AOI.dir.prefix))
if (!dir.exists(dest.dir.issr8)) {
   dir.create(dest.dir.issr8,recursive = TRUE)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------
m <- matrix(c(tile.ulc[1], tile.lrc[1], tile.lrc[1], tile.ulc[1],  
              tile.ulc[2], tile.ulc[2], tile.lrc[2], tile.lrc[2]),  
            nrow=4)
bb.ll <- st_sfc(st_multipoint(m))
st_crs(bb.ll) <- 4326   # ESPG code for WGS84 long/lat


## -----------------------------------------------------------------------------------------------------------------------------------------------
crs.aea <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
(bb.aea <- st_transform(bb.ll, crs.aea))


## ----plot.properties.issr8.crop-----------------------------------------------------------------------------------------------------------------
st_bbox(bb.aea)
bb.vect <- as.vector(matrix(st_bbox(bb.aea), nrow=2, byrow=T))
bb.aea.ext <- ext(bb.vect)
r.issr8.crop <- terra::crop(r.issr8, bb.aea.ext)
plot(r.issr8.crop, main=paste("ISSR-800;",  main=voi.issr8))
print(r.issr8.crop)
summary(r.issr8.crop)


## ----save.tile----------------------------------------------------------------------------------------------------------------------------------
f <- writeRaster(r.issr8.crop, file=paste0(dest.dir.issr8,"/",
                              voi.issr8, "_", depth.issr8, ".tif"),
            overwrite=TRUE, wopt=list(gdal=c("TFW=YES")),
            filetype="GTiff")


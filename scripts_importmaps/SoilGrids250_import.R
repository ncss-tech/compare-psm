## ----set.dirs-----------------------------------------------------------------------------------------------------------------------------------
base.dir <- "/Users/rossiter/ds/"
base.dir.sg <- paste0(base.dir, "SoilGrids250/")
base.dir.import <- "/Volumes/Pythagoras/ds/"
base.dir.sg.import <- paste0(base.dir.import, "SoilGrids250/")


## ----load.pkgs----------------------------------------------------------------------------------------------------------------------------------
options("rgdal_show_exportToProj4_warnings"="none") 
library(rgdal)          # GDAL access from R
library(gdalUtils)      # wrappers for GDAL utility programs that could be
                        #  called from the command line
library(sf)             # spatial data types 
library(terra)          # raster data, replaces `raster`




## ----crs.igh------------------------------------------------------------------------------------------------------------------------------------
crs.igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'


## ----q.list-------------------------------------------------------------------------------------------------------------------------------------
quantile.list <- c("Q0.05", "Q0.5", "Q0.95", "mean")


## ----voi.list-----------------------------------------------------------------------------------------------------------------------------------
voi.list <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc", "ocd")


## ----d.list-------------------------------------------------------------------------------------------------------------------------------------
depth.list <- paste0(c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"),"cm")


## ----select.prop.d.q----------------------------------------------------------------------------------------------------------------------------
voi.n <- 4
voi <- voi.list[voi.n]  
depth.n <- 1
depth <- depth.list[depth.n]
quantile.n <- 4
quantile.sg <- quantile.list[quantile.n]
(voi_layer <- paste(voi, depth, quantile.sg, sep="_")) 


## ----lrc----------------------------------------------------------------------------------------------------------------------------------------
tile.lrc <- c(-77, 35) # lower-right corner
size <- 1                # tile dimensions


## ----ulc----------------------------------------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-size, tile.lrc[2]+size)   # lower-right corner
m <- matrix(c(tile.lrc[1]-size, tile.lrc[2]+size,  #ulc
              tile.lrc[1], tile.lrc[2]+size,  #urc
              tile.lrc[1], tile.lrc[2],       #lrc
              tile.lrc[1]-size, tile.lrc[2]  #lcc
              ),
            nrow=4, byrow = TRUE)
m <- rbind(m, m[1,]) # close the polygon
bb.ll <- st_sfc(st_polygon(list(m)))
st_crs(bb.ll) <- 4326
print(bb.ll)
st_boundary(bb.ll)


## ----dir.prefix---------------------------------------------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## ----dest.dirs----------------------------------------------------------------------------------------------------------------------------------
dest.dir.sg.import <-  paste0(base.dir.sg.import, 
                       AOI.dir.prefix, "/",
                       voi, "/", 
                       quantile.sg, "/", 
                       depth)
if (!dir.exists(dest.dir.sg.import)) {
   dir.create(dest.dir.sg.import, recursive = TRUE)
}
dest.dir.sg <-  paste0(base.dir.sg, 
                       AOI.dir.prefix, "/",
                       voi, "/", 
                       quantile.sg, "/", 
                       depth)
if (!dir.exists(dest.dir.sg)) {
  dir.create(dest.dir.sg, recursive = TRUE)
}


## ----bbox---------------------------------------------------------------------------------------------------------------------------------------
(bb.igh <- st_transform(bb.ll, crs.igh))         # reproject the polygon
(bb.igh.coords <-  st_coordinates(bb.igh)[,1:2]) # convert to coordinates, we only need 2D
#  convert to a bounding box, must order these as c(ulx, uly, lrx, lry)
(bb.sg <- as.vector(c(min(bb.igh.coords[,"X"]), 
                     max(bb.igh.coords[,"Y"]), 
                     max(bb.igh.coords[,"X"]), 
                     min(bb.igh.coords[,"Y"]))))


## ----read.vrt.1---------------------------------------------------------------------------------------------------------------------------------
sg_url <- "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"


## ----read.vrt.2---------------------------------------------------------------------------------------------------------------------------------
(file.out.vrt <- paste0(dest.dir.sg.import, "/", voi_layer, '.vrt'))


## ----read.vrt.3---------------------------------------------------------------------------------------------------------------------------------
gdal_translate(src_dataset=paste0(sg_url, voi, '/', voi_layer, '.vrt'),
    dst_dataset=file.out.vrt,
    tr=c(250,250),
    projwin=bb.sg,
    projwin_srs = crs.igh, 
    of="VRT",
    overwrite=TRUE,
    verbose=TRUE)


## ----vrt.to.tiff.igh----------------------------------------------------------------------------------------------------------------------------
(file.out <- paste0(dest.dir.sg, "/", voi_layer, '.tif'))
gdal_translate(src_dataset = file.out.vrt,
               dst_dataset = file.out,
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               overwrite=TRUE,
               of="GTiff")


## ----get.sg.igh, fig.width=8, fig.height=8------------------------------------------------------------------------------------------------------
r.sg <- terra::rast(file.out)
print(r.sg)
summary(r.sg)
terra::plot(r.sg)


## ----project.vrt--------------------------------------------------------------------------------------------------------------------------------
file.out.vrt.4326 <- paste0(dest.dir.sg.import, "/", voi_layer, '_4326.vrt')
gdalwarp(srcfile=file.out.vrt,
    dstfile=file.out.vrt.4326,
    s_src=igh, 
    t_srs="EPSG:4326", 
    of="VRT",
    overwrite=TRUE)


## ----vrt.to.tiff.4326---------------------------------------------------------------------------------------------------------------------------
(file.out.4326 <- paste0(dest.dir.sg, "/", voi_layer, '_4326.tif'))
gdal_translate(src_dataset = file.out.vrt.4326,
               dst_dataset = file.out.4326,
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               overwrite=TRUE,
               of="GTiff")


## ----get.sg.4326, fig.width=8, fig.height=8-----------------------------------------------------------------------------------------------------
r.sg.4326 <- terra::rast(file.out.4326)
print(r.sg.4326)
summary(r.sg.4326)
terra::plot(r.sg.4326)


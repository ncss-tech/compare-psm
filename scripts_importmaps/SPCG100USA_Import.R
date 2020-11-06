## ------------------------------------------------------------------------------------------------------------------------------------------------
library(sf)     # Simple Features representation of spatial data
library(terra)  # for raster import and display


## ------------------------------------------------------------------------------------------------------------------------------------------------
base.dir.import <- "/Volumes/Pythagoras/ds/"
base.dir.psu.import <- paste0(base.dir.import, "SPCG100USA")
base.dir <- "/Users/rossiter/ds/"
base.dir.psu <- paste0(base.dir, "SPCG100USA")


## ------------------------------------------------------------------------------------------------------------------------------------------------
if (!dir.exists(base.dir.psu.import)) {
   dir.create(dbase.dir.psu.import, recursive = TRUE)
}


## ------------------------------------------------------------------------------------------------------------------------------------------------
voi.list.psu <- c("soc", "n", "bd", "sand", "clay", "ph_h2o", "ec", "k", "mg")


## ------------------------------------------------------------------------------------------------------------------------------------------------
voi.n <- 6
voi.psu <- voi.list.psu[voi.n]


## ------------------------------------------------------------------------------------------------------------------------------------------------
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
depth.list.psu <- c("0", "5", "15", "30", "60", "100", "200")


## ------------------------------------------------------------------------------------------------------------------------------------------------
depth.n<- 1
depth.sg <- depth.list.sg[depth.n]
depth.psu.top <- paste0("sl", depth.n)  # slice at top of layer
depth.psu.bottom <- paste0("sl", depth.n+1)  # slice at bottom of layer


## ----lrc-----------------------------------------------------------------------------------------------------------------------------------------
tile.lrc <- c(-76, 42)   # lower-right corner
tile.size <- 1


## ----ulc-----------------------------------------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-tile.size, tile.lrc[2]+tile.size) # upper-left corner


## ------------------------------------------------------------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## ----in.slice1-----------------------------------------------------------------------------------------------------------------------------------
filename.in <- paste0(base.dir.psu.import, "/", voi.psu, "_M_", depth.psu.top ,"_100m.tif")
r.psu.top <- terra::rast(filename.in)
print(r.psu.top)


## ----in.slice2-----------------------------------------------------------------------------------------------------------------------------------
filename.in <- paste0(base.dir.psu.import, "/", voi.psu, "_M_", depth.psu.bottom ,"_100m.tif")
r.psu.bottom <- terra::rast(filename.in)
print(r.psu.bottom)


## ----plot.properties.crop, fig.width=10, fig.height=5--------------------------------------------------------------------------------------------
m <- matrix(c(tile.ulc[1],tile.lrc[1],  #ulc
              tile.ulc[2], tile.lrc[2]),  #lrc
            nrow=2)
bb.ll <- st_sfc(st_multipoint(m)); st_crs(bb.ll) <- 4326
# convert the bounding box to the AEA CRS used in SPCG100USA
bb.aea <- st_transform(bb.ll, crs(r.psu.top))
bb.vect <- as.vector(matrix(st_bbox(bb.aea), nrow=2, byrow=T))
(bb.aea.ext <- ext(bb.vect))


## ----crop.both-----------------------------------------------------------------------------------------------------------------------------------
r.psu.crop.top <- terra::crop(r.psu.top, bb.aea.ext)
r.psu.crop.bottom <- terra::crop(r.psu.bottom, bb.aea.ext)


## ----crop.plot-----------------------------------------------------------------------------------------------------------------------------------
zlim = c(floor(min(values(r.psu.crop.top), values(r.psu.crop.bottom), na.rm=TRUE)),
         ceiling(max(values(r.psu.crop.top), values(r.psu.crop.bottom), na.rm=TRUE)))
par(mfrow=c(1,2))
terra::plot(r.psu.crop.top, range=zlim,
            main=paste0(voi.psu, ", depth ", depth.list.psu[depth.n], " cm"))
terra::plot(r.psu.crop.bottom, range=zlim,
            main=paste0(voi.psu, ", depth ", depth.list.psu[depth.n+1], " cm"))
par(mfrow=c(1,1))
print(r.psu.crop.top)
print(r.psu.crop.bottom)
summary(r.psu.crop.top)
summary(r.psu.crop.bottom)


## ------------------------------------------------------------------------------------------------------------------------------------------------
rm(r.psu.top, r.psu.bottom)


## ------------------------------------------------------------------------------------------------------------------------------------------------
r.psu.crop <- ((r.psu.crop.bottom + r.psu.crop.top)/2)
summary(r.psu.crop)
plot(r.psu.crop)
rm(r.psu.crop.top, r.psu.crop.bottom)


## ------------------------------------------------------------------------------------------------------------------------------------------------
(dest.dir.psu <-  paste0(base.dir.psu, "/", AOI.dir.prefix))
if (!dir.exists(dest.dir.psu)) {
  dir.create(dest.dir.psu, recursive = TRUE)}


## ------------------------------------------------------------------------------------------------------------------------------------------------
(file.name <- paste0(voi.psu, "_", depth.sg))
f <- writeRaster(r.psu.crop, file=paste0(dest.dir.psu,"/",
                                        file.name, ".tiff"),
                 overwrite=TRUE, wopt=list(gdal=c("TFW=YES")),
                 filetype="GTiff")


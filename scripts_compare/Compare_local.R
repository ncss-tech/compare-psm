params <-
list(lrc_long = -76L, lrc_lat = 42L, size = 1L, voi.n = 4L, quantile.n = "NA", 
    depth.n = 1L, test.tile.size = 0.15, test.tile.x.offset = 0.3, 
    test.tile.y.offset = 0.45)

## ----figure.setup----------------------------------------------------------------------------
n.products <- 3
n.figs.row <- 1
n.figs.col <- 3
map.fig.width <- n.figs.col*5
map.fig.height <- n.figs.row*5
n.figs.row.diff <- 1
n.figs.col.diff <- 2
map.fig.width.diff <- n.figs.col.diff*5
map.fig.height.diff <- n.figs.row.diff*5


## --------------------------------------------------------------------------------------------
library(rgdal)      # R interface to GDAL
library(terra)      # for raster maps
library(sf)         # Simple Features spatial data
# library(gridExtra)  # arrange multiple plots
library(knitr)      # for fancy tables
library(xtable)     # (same)


## ----base.dir--------------------------------------------------------------------------------
base.dir <- "/Volumes/Pythagoras/ds/DSM_export"
base.dir.gssurgo <- paste0(base.dir, "/gSSURGO")
base.dir.polaris <- paste0(base.dir, "/POLARIS")
base.dir.sg <- paste0(base.dir, "/SoilGrids250")


## ----base.dir.import-------------------------------------------------------------------------
base.dir.import <- "/Volumes/Pythagoras/ds/"
base.dir.polaris.import <- paste0(base.dir.import, "POLARIS")


## ----base.dir.export-------------------------------------------------------------------------
base.dir.export <- paste0(base.dir.import, "Compare_PSM_local")


## --------------------------------------------------------------------------------------------
print(paste("lrc_long:", params$lrc_long, "; lrc_lat:", params$lrc_lat, "; size:", params$size))
print(paste("voi.n:", params$voi.n, "; depth.n:", params$depth.n))
print(paste("test.tile.size:", params$test.tile.size, 
            "test.tile.x.offset:", params$test.tile.x.offset,
            "test.tile.y.offset:", params$test.tile.y.offset))


## --------------------------------------------------------------------------------------------
voi.list.gssurgo <- c("claytotal_r", "silttotal_r", "sandtotal_r",
                  "ph1to1h2o_r", "cec7_r", "om_r",   # note SOM not SOC
                  "dbthirdbar_r", "sieveno10_r") # passing 2.0 mm sieve, complement is coarse fragments
voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
voi.list.polaris <- c("clay", "silt", "sand", "ph", "", "om", "bd", "") 


## --------------------------------------------------------------------------------------------
voi.n <- params$voi.n   # variable of interest, SoilGrids name
voi.gssurgo <- voi.list.gssurgo[voi.n]
voi.sg <- voi.list.sg[voi.n]
voi.polaris <- voi.list.polaris[voi.n]


## --------------------------------------------------------------------------------------------
depth.list.gssurgo <- c("05", "515", "1530", "3060", "60100", "100200")
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
depth.list.polaris <- gsub("-", "_", depth.list.sg)


## --------------------------------------------------------------------------------------------
depth <- params$depth.n


## ----lrc-------------------------------------------------------------------------------------
tile.lrc <- c(params$lrc_long, params$lrc_lat) # lower-right corner
tile.size <- params$size                # tile dimensions


## ----ulc-------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-tile.size, tile.lrc[2]+tile.size) # upper-left corner


## ----aoi.dir.prefix--------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## ----adjust.fig.path-------------------------------------------------------------------------
knitr::opts_chunk$set(fig.path = paste0(knitr::opts_chunk$get("fig.path"), 
                                        AOI.dir.prefix, "/",
                                        voi.sg, "_", depth.list.sg[depth], "_"))


## ----bbox.4326-------------------------------------------------------------------------------
m <- matrix(c(tile.ulc[1],tile.lrc[1],  #ulc
              tile.ulc[2], tile.lrc[2]), nrow=2) #lrc
bb.ll <- st_sfc(st_multipoint(m))
st_crs(bb.ll) <- 4326   # ESPG code for WGS84 long/lat


## ----bbox.igh--------------------------------------------------------------------------------
# convert to Homolosine. Note epsg=152160 is not in PROJ4 database
crs.igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
(bb.igh <- st_transform(bb.ll, crs.igh))
(bb.igh <- st_coordinates(bb.igh)[,1:2])
(bb <- as.vector(t(bb.igh)))


## ----bbox.aea--------------------------------------------------------------------------------
crs.aea <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
(bb.aea <- st_transform(bb.ll, crs.aea))


## --------------------------------------------------------------------------------------------
tile.lrc <- c(params$lrc_long, params$lrc_lat) # lower-right corner
tile.size <- params$size     
test.tile.size <- params$test.tile.size  # degrees
test.tile.x.offset <- params$test.tile.x.offset  # west from right edge
test.tile.y.offset <- params$test.tile.y.offset  # north from bottom edge
## CNY
# test.tile.size <- 0.15  # degrees
# test.tile.x.offset <- 0.30  # west from right edge
# test.tile.y.offset <- 0.45  # north from bottom edge
## NC
# test.tile.size <- 0.15  # degrees
# test.tile.x.offset <- 0.32  # west from right edge
# test.tile.y.offset <- 0.68  # north from bottom edge
## IN
# test.tile.size <- 0.15  # degrees
# test.tile.x.offset <- 0.61  # west from right edge
# test.tile.y.offset <- 0.44  # north from bottom edge


## ----get.tiles.gssurgo-----------------------------------------------------------------------
src.dir <-  paste0(base.dir.gssurgo ,"/", 
                   AOI.dir.prefix)
(voi.depth.name <- paste0(voi.gssurgo, "_", depth.list.gssurgo[depth]))
(file.name <- paste0(src.dir, "/", voi.depth.name, '.tif'))
if (file.exists(file.name)) {
  r.gssurgo <- terra::rast(file.name)
  names(r.gssurgo) <- "gssurgo"
  print(r.gssurgo)
} else { stop("No gSSURGO tile, stopping") }


## ----get.tiles.sg----------------------------------------------------------------------------
# SoilGrids250 -- only the mean prediction in this script
# Use the EPSG:4326 version
src.dir <-  paste0(base.dir.sg ,"/", 
                   AOI.dir.prefix, "/", 
                   voi.sg, "/mean/",
                   depth.list.sg[depth], "cm")
(voi.depth.name <- paste0(voi.sg, "_", depth.list.sg[depth], "cm_mean_4326"))
(file.name <- paste0(src.dir, "/", voi.depth.name, '.tif'))
if (file.exists(file.name)) {
  r.sg <- terra::rast(file.name)
  names(r.sg) <- "SoilGrids250"
  print(r.sg)
} else { stop("No SoilGrids250 tile, stopping") }


## ----get.tiles.polaris-----------------------------------------------------------------------
# POLARIS -- only the mean prediction in this script
(file.name <- paste0(base.dir.polaris.import, "/",
                     AOI.dir.prefix, "/",
                     voi.list.polaris[params$voi.n], "/mean/",
                     depth.list.polaris[depth], "/",
                     AOI.dir.prefix, ".tif"))
if (file.exists(file.name)) {
  r.p <- terra::rast(file.name)
  names(r.p) <- "polaris"
  print(r.p)
}  else { stop("No POLARIS tile, stopping") }


## ----show.conversions------------------------------------------------------------------------
df <- data.frame(property=voi.list.sg, 
                 #"clay"  "silt"  "sand"  "phh2o" "cec"   "soc"   "bdod"  "cfvo" 
                 sg=c("%%","%%","%%","pHx10","mmol(c)/kg","dg/kg","cg/cm3", "cm3/dm3"),  #SG
                 ## metadata for gSSURGO:
                 ## https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053631
                 gssurgo=c("%","%","%","pH","meq/Cg","1.724 * %","g/cm3", "100-%"),  # gSSURGO
                  # sieveno10_r is complement of cfvo; som not soc
                 p=c("%","%", "%","pH","", "log10(1.724 * %)", "g/cm3", ""))  # POLARIS
 knitr::kable(
  df, caption = 'Properties and units of measure',
  col.names=c("Property", "SG2", "gSSURGO",  "PSP"),
  booktabs = TRUE)


## ----make.conversion.matrix------------------------------------------------------------------
som.to.soc <- 1/1.724138 # this was used in the lab, I know it has been heavily criticized
conversions <- data.frame(property=voi.list.sg, 
                 # sg=c("%%","%%","%%","pHx10","mmol(c)/kg","dg/kg","cg/cm3", "cm3/dm3"), #SG
                 gssurgo=c(10, 10, 10, 10 , 10, 1000*som.to.soc, 100, NA),  # gSSURGO
                 p=c(10,  10, 10,  10, NA, NA, 100, NA)  # POLARIS -- SOM is special case
                 )
conversions <- format(conversions, digits=4, nsmall=0, scientific=FALSE)
knitr::kable(
  conversions, caption = 'Conversion factors, multiply by these to match SG2',
  col.names=c("Property", "gSSURGO", "PSP"),
  booktabs = TRUE,
  align = "r")


## ----polaris.soc-----------------------------------------------------------------------------
if (exists("r.p") && (voi.sg=="soc")) {
    r.p <- ((10^r.p)*som.to.soc*1000) 
}


## ----gssurgo.cfvo----------------------------------------------------------------------------
if (voi.sg == "cfvo") {
  r.gssurgo <- (100 - r.ssurgo)*0.1
}


## ----convert---------------------------------------------------------------------------------
# this property's factors
(factors <- conversions[match(voi.sg, conversions$property),])

# gSSURGO
fact <- as.numeric(factors["gssurgo"])
if (!is.na(fact) && (fact != 1)) { r.gssurgo <- r.gssurgo*fact }

fact <- as.numeric(factors["p"])
if (!is.na(fact) && (fact != 1)) { r.p <- r.p*fact }


## ----crs.show.1------------------------------------------------------------------------------
rgdal::showP4(crs(r.sg))
data.frame(sg=res(r.sg)[1], 
           polaris=res(r.p)[1])


## ----crs.show.2------------------------------------------------------------------------------
rgdal::showP4(crs(r.gssurgo))


## ----crs.resample----------------------------------------------------------------------------
r.sg.p <- terra::resample(r.sg, r.p, method="cubic")
# plot(r.sg.p)


## ----crs.project-----------------------------------------------------------------------------
r.gssurgo.p <- terra::project(r.gssurgo, r.p, method="near") 
# plot(r.gssurgo.p)


## ----make.polygon----------------------------------------------------------------------------
m <- matrix(c(tile.ulc[1],tile.ulc[2],  #ulc
              tile.lrc[1],tile.ulc[2],  #urc
              tile.lrc[1],tile.lrc[2],  #lrc
              tile.ulc[1],tile.lrc[2],  #llc
              tile.ulc[1], tile.ulc[2]), nrow=5, byrow = TRUE) # ulc again, closes polygon
bb.poly <- st_sfc(st_linestring(m))
st_crs(bb.poly) <- 4326 
bb.poly <- st_polygonize(bb.poly)
bb.poly <- as_Spatial(bb.poly)
ext(bb.poly)


## --------------------------------------------------------------------------------------------
r.gssurgo.p <- crop(r.gssurgo.p, bb.poly)
r.p <- crop(r.p, bb.poly)
r.sg.p <- crop(r.sg.p, bb.poly)


## ----mask.with.polaris-----------------------------------------------------------------------
r.gssurgo.p <- mask(r.gssurgo.p, r.sg.p)
r.p <- mask(r.p, r.sg.p)


## ----mask.with.gssurgo-----------------------------------------------------------------------
r.p <- mask(r.p, r.gssurgo.p)
r.sg.p <- mask(r.sg.p, r.gssurgo.p)


## ----crop.test.area--------------------------------------------------------------------------
(tmp <- as.vector(ext(r.sg.p)))
tmp["xmax"] <- tmp["xmax"] - test.tile.x.offset
tmp["xmin"] <- tmp["xmax"] - test.tile.size
tmp["ymin"] <- tmp["ymin"] + test.tile.y.offset
tmp["ymax"] <- tmp["ymin"] + test.tile.size
ext(tmp)
r.gssurgo.p <- crop(r.gssurgo.p, ext(tmp))
r.sg.p <- crop(r.sg.p, ext(tmp))
r.p <- crop(r.p, ext(tmp))


## ----compare.zlim----------------------------------------------------------------------------
zlim <- c(min(values(r.sg)*10, na.rm = TRUE),
          max(values(r.sg)*10, na.rm = TRUE))/10
zlim <- c(floor(min(zlim[1]*10, values(r.gssurgo.p)*10, na.rm=TRUE)),
          ceiling(max(zlim[2]*10, values(r.gssurgo.p)*10, na.rm=TRUE)))/10
zlim <- c(floor(min(zlim[1]*10, values(r.p)*10, na.rm=TRUE)),
          ceiling(max(zlim[2]*10, values(r.p)*10, na.rm=TRUE)))/10
print(zlim)


## ----hist.densities--------------------------------------------------------------------------
max.dens <- function(r.map) {  # argument: the raster map
  h <- hist(r.map,  breaks=24, plot = FALSE)
  max(h$counts/(diff(h$breaks[1:2]))/sum(h$counts))
}
yl <- c(0, max(max.dens(r.gssurgo.p), max.dens(r.sg.p), max.dens(r.p)))


## ----hist.sg.props, fig.width=map.fig.width, fig.height=map.fig.height-----------------------
par(mfrow=c(n.figs.row, n.figs.col))
hist(r.gssurgo.p, breaks=24, main="gSSURGO",
     xlim=zlim, xlab="", freq = FALSE, ylim=yl)
hist(r.p, breaks=24, main="PSP",
     xlim=zlim, xlab="", freq = FALSE, ylim=yl)
hist(r.sg, breaks=24, main="SG2",
     xlim=zlim, xlab="", freq = FALSE, ylim=yl)
par(mfrow=c(1,1))


## ----map.sg.props, fig.width=map.fig.width, fig.height=map.fig.height------------------------
par(mfrow=c(n.figs.row, n.figs.col))
terra::plot(r.gssurgo.p, main="gSSURGO", range=zlim)
terra::plot(r.p, main="PSP", range=zlim)
terra::plot(r.sg.p, main="SG2", range=zlim)
par(mfrow=c(1,1))


## ----pairwise--------------------------------------------------------------------------------
v.all <- data.frame(gssurgo=values(r.gssurgo.p),
                    sg=values(r.sg.p),
                    polaris=values(r.p))
names(v.all) <- c("gSSURGO", "SG2", "PSP")
summary(v.all)
cor.all <- cor(v.all, use="pairwise.complete.obs")
cor.upper <- cor.all; cor.upper[lower.tri(cor.upper)] <- NA
print(round(cor.upper, 3))


## ----corrplot, fig.width=(n.figs.col*1.5), fig.height=n.figs.col*1.5-------------------------
library(corrplot)
corrplot.mixed(cor.all, upper="ellipse", lower="number", diag="n",
               lower.col = "black")


## --------------------------------------------------------------------------------------------
diff.gssurgo.sg <- r.gssurgo.p - r.sg.p
diff.gssurgo.p <-  r.gssurgo.p - r.p


## ----stats.compare.sg, warning=FALSE---------------------------------------------------------
stats.compare <- data.frame(DSM_product = "", MD = 0, RMSD = 0, RMSD.Adjusted = 0)
rmse <- function(v1, v2) {
  round(sqrt(mean((v1-v2)^2, na.rm=TRUE)),3)
}
me <- function(v1, v2) { 
  round(mean(v1-v2, na.rm=TRUE), 3)
}
rmse.adj <- function(v1, v2) {   # RMSE adjusted for ME (bias)
  me <- mean(v1-v2, na.rm=TRUE)
  v2.adj <- v2 + me
  round(sqrt(mean((v1-v2.adj)^2, na.rm=TRUE)),3)
}
stats.compare[1, ] <- c("SG2",
                        me(values(r.gssurgo.p),values(r.sg.p)),
                        rmse(values(r.gssurgo.p),values(r.sg.p)),
                        rmse.adj(values(r.gssurgo.p),values(r.sg.p))
)
stats.compare[2, ] <- c("PSP",
                        me(values(r.gssurgo.p),values(r.p)),
                        rmse(values(r.gssurgo.p),values(r.p)),
                        rmse.adj(values(r.gssurgo.p),values(r.p))
)


## ----save.stats.compare.sg-------------------------------------------------------------------
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
names(stats.compare)[1] <- "DSM\_product"
x <- xtable(stats.compare, row.names=FALSE, digits=3)
autoformat(x)
capture.output(print(x, include.rownames=FALSE), 
               file=paste0("../LaTeX_tables/gSSURGO_compare_statistics_",
                           AOI.dir.prefix, "_", voi.sg, "_", depth.list.sg[depth], ".tex"))


## ----zlim.diff.sg----------------------------------------------------------------------------
zlim <- c(NA, NA)
zlim <- c(floor(min(zlim[1]*10, values(diff.gssurgo.sg)*10, na.rm=TRUE)),
          ceiling(max(zlim[2]*10, values(diff.gssurgo.sg)*10, na.rm=TRUE)))/10
zlim <- c(floor(min(zlim[1]*10, values(diff.gssurgo.p)*10, na.rm=TRUE)),
          ceiling(max(zlim[2]*10, values(diff.gssurgo.p)*10, na.rm=TRUE)))/10
print(zlim)


## ----diff.hist.densities---------------------------------------------------------------------
yl <- c(0, max(max.dens(diff.gssurgo.p), max.dens(diff.gssurgo.sg)))


## ----hist.diff.sg, fig.width=map.fig.width.diff, fig.height=map.fig.height.diff--------------
par(mfrow=c(n.figs.row.diff, n.figs.col.diff))
hist(diff.gssurgo.p, main="gSSURGO - PSP", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
hist(diff.gssurgo.sg, main="gSSURGO - SG2", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
par(mfrow=c(1,1))


## ----plot.diff.sg, fig.width=map.fig.width.diff, fig.height=map.fig.height.diff--------------
par(mfrow=c(n.figs.row.diff, n.figs.col.diff))
terra::plot(diff.gssurgo.p, main="Difference gSSURGO - PSP",
            range=zlim, col=bpy.colors(64))
terra::plot(diff.gssurgo.sg, main="Difference gSSURGO - SG2",
            range=zlim, col=bpy.colors(64))
par(mfrow=c(1,1))


## ----save.dir--------------------------------------------------------------------------------
dest.dir.save <-  file.path(base.dir.export,
                       AOI.dir.prefix)
if (!dir.exists(dest.dir.save)) {
   dir.create(dest.dir.save, recursive = TRUE)
}


## ----save.tiles------------------------------------------------------------------------------
# gSSURGO
voi.depth.sg <- paste0(voi.sg, "_", depth.list.sg[depth])
dest.name <- paste0(dest.dir.save,"/gssurgo_tile_30_",  voi.depth.sg, ".tif")
f <- terra::writeRaster(r.gssurgo.p, file=dest.name,
                        overwrite=TRUE,
                        wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
                        filetype="GTiff")
# GDALinfo(dest.name)
print(paste("Wrote ", dest.name))
# POLARIS
dest.name <- paste0(dest.dir.save,"/polaris_tile_30_",  voi.depth.sg, ".tif")
f <- terra::writeRaster(r.p, file=dest.name,
                        overwrite=TRUE,
                        wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
                        filetype="GTiff")
#  GDALinfo(dest.name)
print(paste("Wrote ", dest.name))
# SoilGrids250
dest.name <- paste0(dest.dir.save,"/sg_tile_30_",  voi.depth.sg, ".tif")
f <- terra::writeRaster(r.sg.p, file=dest.name,
                        overwrite=TRUE,
                        wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
                        filetype="GTiff")
# GDALinfo(dest.name)
print(paste("Wrote ", dest.name))


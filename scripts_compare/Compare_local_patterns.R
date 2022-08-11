params <-
list(lrc_long = -76L, lrc_lat = 42L, size = 1L, voi.n = 4L, quantile.n = "NA", 
    depth.n = 1L, test.tile.size = 0.15, test.tile.x.offset = 0.3, 
    test.tile.y.offset = 0.45)

## ----------------------------------------------------------------------------------------------------------------
n.products <- 3
n.figs.row <- 1
n.figs.col <- 3
map.fig.width <- n.figs.col*5
map.fig.height <- n.figs.row*5


## ----------------------------------------------------------------------------------------------------------------
library(raster, warn.conflicts=FALSE)      # previous version of raster classes now in `terra`
                     #   needed for landscape metrics
library(terra, warn.conflicts=FALSE)       # Robert Hijmans raster and vector data
library(rgdal)       # CRS
library(sp)          # `sp` spatial classes
library(sf)          # Simple Features
require(xtable)      # format tables for LaTeX
library(dplyr)       # data wrangling
library(ggplot2)     # ggplot graphics
library(gridExtra)  # arrange multiple plots
library(RColorBrewer) # colour pallettes
library(sabre)       # compare polygon map spatial structure
library(landscapemetrics)   # FRAGSTATS metrics
library(landscapetools)
library(gstat)      # variogram modelling


## ----------------------------------------------------------------------------------------------------------------
base.dir <- "/Volumes/Pythagoras/ds/Compare_PSM_local/"


## ----------------------------------------------------------------------------------------------------------------
print(paste("lrc_long:", params$lrc_long, "; lrc_lat:", params$lrc_lat, "; size:", params$size))
print(paste("voi.n:", params$voi.n, "; depth.n:", params$depth.n))


## ----lrc---------------------------------------------------------------------------------------------------------
tile.lrc <- c(params$lrc_long, params$lrc_lat) # lower-right corner
tile.size <- params$size                # tile dimensions


## ----ulc---------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-tile.size, tile.lrc[2]+tile.size) # upper-left corner


## ----aoi.set.dir.prefix------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## ----voi---------------------------------------------------------------------------------------------------------
voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
voi.sg <- voi.list.sg[params$voi.n]


## ----depth-------------------------------------------------------------------------------------------------------
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
voi.depth <- paste0(voi.sg, "_", depth.list.sg[params$depth.n])


## ----adjust.fig.path---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.path = paste0(knitr::opts_chunk$get("fig.path"), 
                                        AOI.dir.prefix, "/",
                                        voi.depth, "_"))


## ----------------------------------------------------------------------------------------------------------------
(gssurgo <- rast(paste0(base.dir, AOI.dir.prefix, "/gssurgo_tile_30_", voi.depth, ".tif")))
names(gssurgo) <- "voi"
(sg <- rast(paste0(base.dir, AOI.dir.prefix, "/sg_tile_30_", voi.depth, ".tif")))
names(sg) <- "voi"
(polaris <- rast(paste0(base.dir, AOI.dir.prefix, "/polaris_tile_30_", voi.depth, ".tif")))
names(polaris) <- "voi" 


## ----show.crs----------------------------------------------------------------------------------------------------
rgdal::showP4(crs(sg))


## ----get.utm-----------------------------------------------------------------------------------------------------
long2UTM <- function(long) { (floor((long + 180)/6) %% 60) + 1 }
utm.zone <- long2UTM(params$lrc_long+0.5)
epsg.db <- rgdal::make_EPSG()
ix <- grep(paste0("WGS 84 / UTM zone ", utm.zone, "N"), epsg.db$note)
epsg.db[ix,]
epsg.code <- epsg.db[ix, "code"]


## ----------------------------------------------------------------------------------------------------------------
crs.utm <- paste0("+init=epsg:", epsg.code)
gssurgo <- terra::project(gssurgo, crs.utm)
sg <- terra::project(sg, crs.utm)
polaris <- terra::project(polaris, crs.utm)


## ----zlim--------------------------------------------------------------------------------------------------------
values.all <- c(values(gssurgo),
                values(sg),
                values(polaris))
(zlim <- c(min(values.all, na.rm = TRUE),
                max(values.all, na.rm=TRUE)))


## ----side.by.side, fig.width=map.fig.width, fig.height=map.fig.height--------------------------------------------
par(mfrow=c(n.figs.row, n.figs.col))
plot(gssurgo, main="gSSURGO", range=zlim)
plot(polaris, main="PSP", range=zlim)
plot(sg, main="SG2", range=zlim)
par(mfrow=c(1,1))


## ----------------------------------------------------------------------------------------------------------------
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


## ----compare.stats-----------------------------------------------------------------------------------------------
stats.compare <- data.frame(DSM_product = "", MD = 0, RMSD = 0, RMSD.Adjusted = 0)
stats.compare[1, ] <- c("SG2",
                        me(values(gssurgo),values(sg)),
                        rmse(values(gssurgo),values(sg)),
                        rmse.adj(values(gssurgo),values(sg))
                        )
stats.compare[2, ] <- c("PSP",
                        me(values(gssurgo),values(polaris)),
                        rmse(values(gssurgo),values(polaris)),
                        rmse.adj(values(gssurgo),values(polaris))
)
print(stats.compare)


## ----side.by.side.unbiased---------------------------------------------------------------------------------------
sg.adj <- sg + me(values(gssurgo),values(sg))
polaris.adj <- polaris + me(values(gssurgo),values(polaris))


## ----zlim.adj----------------------------------------------------------------------------------------------------
values.all.adj <- c(values(gssurgo), values(sg.adj), values(polaris.adj))
(zlim.adj <- c(min(values.all.adj, na.rm = TRUE),
                max(values.all.adj, na.rm=TRUE)))


## ----compare.stats.adj-------------------------------------------------------------------------------------------
stats.compare <- data.frame(DSM_product = "", MD = 0, RMSD = 0, RMSD.Adjusted = 0)
stats.compare[1, ] <- c("SG2",
                        me(values(gssurgo),values(sg.adj)),
                        rmse(values(gssurgo),values(sg.adj)),
                        rmse.adj(values(gssurgo),values(sg.adj))
                        )
stats.compare[2, ] <- c("PSP",
                        me(values(gssurgo),values(polaris.adj)),
                        rmse(values(gssurgo),values(polaris.adj)),
                        rmse.adj(values(gssurgo),values(polaris.adj))
                        )
print(stats.compare)


## ----make.sp-----------------------------------------------------------------------------------------------------
gssurgo.sp <- as(raster(gssurgo), "SpatialPointsDataFrame")
sg.sp <- as(raster(sg), "SpatialPointsDataFrame")
polaris.sp <- as(raster(polaris), "SpatialPointsDataFrame") 


## ----------------------------------------------------------------------------------------------------------------
range.init <- 800  # m 
cutoff.init <- range.init*3 # m
width.init <- 100   # plenty of cells for narrow bins


## ----compute.variogram, fig.height=6, fig.width=8----------------------------------------------------------------
system.time(
  v.gssurgo <- gstat::variogram(voi ~ 1, loc = gssurgo.sp, 
                                cutoff=cutoff.init, width=width.init)
) # system.time
# plot(v.gssurgo, pl=T)
#
system.time(
  v.sg <- variogram(voi ~ 1, loc = sg.sp, cutoff=cutoff.init, width=width.init)
) # system.time
# plot(v.sg, pl=T)
#
system.time(
v.polaris <- variogram(voi ~ 1, loc = polaris.sp, cutoff=cutoff.init, width=width.init)
) # system.time
#  plot(v.polaris, pl=T)


## ----model.variogram, fig.height=6, fig.width=8------------------------------------------------------------------
vm.gssurgo <- vgm(0.8*max(v.gssurgo$gamma), "Exp", range.init, 0)
vmf.gssurgo <- fit.variogram(v.gssurgo, model=vm.gssurgo)
# plot(v.gssurgo, pl=T, model=vmf.gssurgo)
#
vm.sg <- vgm(0.8*max(v.sg$gamma), "Exp", range.init, 0)
vmf.sg <- fit.variogram(v.sg, model=vm.sg)
# plot(v.sg, pl=T, model=vmf.sg)
#
vm.polaris <- vgm(0.8*max(v.polaris$gamma), "Exp", range.init, 0)
vmf.polaris <- fit.variogram(v.polaris, model=vm.polaris)
# plot(v.polaris, pl=T, model=vmf.polaris)


## ----table.compare.variograms------------------------------------------------------------------------------------
vmeasure.compare <- data.frame(product = "", Range = 0, StructSill = 0, PropNugget = 0)
vmeasure.compare[1,] <- c("gSSURGO", 
                       round(vmf.gssurgo[2,"range"], 0),
                       round(vmf.gssurgo[2,"psill"], 2),
                       round(vmf.gssurgo[1,"psill"]/sum(vmf.gssurgo[,"psill"]), 2)
                       )
vmeasure.compare[2,] <- c("SG2", 
                       round(vmf.sg[2,"range"], 0),
                       round(vmf.sg[2,"psill"], 2),
                       round(vmf.sg[1,"psill"]/sum(vmf.sg[,"psill"]), 2)
                       )
vmeasure.compare[3, ] <-  c("PSP", 
                       round(vmf.polaris[2,"range"], 0),
                       round(vmf.polaris[2,"psill"], 2),
                       round(vmf.polaris[1,"psill"]/sum(vmf.polaris[,"psill"]), 2))
# multiply all range parameters by 3 to get effective range
vmeasure.compare[, 2:4] <- apply(vmeasure.compare[, 2:4],  2, as.numeric)
vmeasure.compare[, "Range"] <- vmeasure.compare[, "Range"]*3
print(vmeasure.compare)


## ----write.table.compare.variograms------------------------------------------------------------------------------
names(vmeasure.compare) <- c("Product", "Effective range", 
                             "Structural Sill", "Proportional Nugget")
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
x <- xtable(vmeasure.compare, row.names=FALSE, digits=2)
autoformat(x)
capture.output(print(x, include.rownames=FALSE), file=
                 paste0("../LaTeX_tables/compare_variograms_local_",
                           AOI.dir.prefix, "_", voi.depth, ".tex"))


## ----plot.vgms, fig.width=map.fig.width, fig.height=map.fig.height-----------------------------------------------
ylims <- c(0, max(v.gssurgo$gamma, v.sg$gamma, 
                  max(v.polaris$gamma))*1.1)
p0 <- plot(v.gssurgo, ylim=ylims, model=vmf.gssurgo, main="gSSURGO", xlab="separation (m)", pch=20)
p1 <- plot(v.polaris, ylim=ylims, model=vmf.polaris, main="PSP", xlab="separation (m)", pch=20)
p2 <- plot(v.sg, ylim=ylims, model=vmf.sg, main="SG2", xlab="separation (m)", pch=20)
print(p0, split=c(1, 1, n.figs.col, n.figs.row), more=T) 
print(p1, split=c(2, 1, n.figs.col, n.figs.row), more=T) 
print(p2, split=c(3, 1, n.figs.col, n.figs.row), more=F)


## ----hist.equal.cuts, fig.width=8, fig.height=4------------------------------------------------------------------
n.class <- 8
#
# values.all computed above
values.all.sort <- sort(values.all)
#
n <- length(values.all) - sum(is.na(values.all))
(cut.positions <- round(n/n.class))
(cuts <- values.all.sort[cut.positions * 1:(n.class-1)])
hist(values.all, breaks=36, main="Histogram equalization")
abline(v=cuts, col="blue", lwd=2)


## ----classify.setup----------------------------------------------------------------------------------------------
(cut.names <- cut(zlim, breaks=c(zlim[1], cuts, zlim[2]),
                  ordered_result=TRUE, include.lowest = TRUE)) 
# make sure lowest value is included
#
# common colour ramp
# color.ramp <- bpy.colors(n.class)
color.ramp <- brewer.pal(n.class, "PuBu")  # safe for colour-blind viewers, from RColorBrewer
#
(class.limits <- c(zlim[1], cuts, zlim[2]))


## ----------------------------------------------------------------------------------------------------------------
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
x <- xtable(data.frame(quantiles=c("minimum", paste0("q", 1:3), 
                                   "median", paste0("q", 5:7), "maximum"),
                       limits=round(class.limits,2)), 
            align="lrr",
            row.names=FALSE, digits=2)
autoformat(x)
capture.output(print(x, include.rownames=FALSE), file=
                 paste0("../LaTeX_tables/class_limits_local_",
                           AOI.dir.prefix, "_", voi.depth, ".tex"))


## ----classify.raster---------------------------------------------------------------------------------------------
gssurgo.class <- classify(gssurgo, rcl=class.limits)
# gssurgo.class <- as.factor(gssurgo.class)
table(values(gssurgo.class))
# .l <- range(values(gssurgo.class), na.rm=TRUE)
# terra::plot(gssurgo.class,
#             col=color.ramp[.l[1]:.l[2]], type="classes",
#             main="SoilGrids250")
#
sg.class <- classify(sg, rcl=class.limits)
# sg.class <- as.factor(sg.class)
table(values(sg.class))
# .l <- range(values(sg.class), na.rm=TRUE)
# terra::plot(sg.class,
#             col=color.ramp[.l[1]:.l[2]], type="classes",
#             main="SoilGrids250")
#
polaris.class <- classify(polaris, rcl=class.limits)
# psu.class <- as.factor(psu.class)
table(values(polaris.class))
# .l <- range(values(polaris.class), na.rm=TRUE)
# terra::plot(polaris.class,
#             col=color.ramp[.l[1]:.l[2]], type="classes",
#             main="PSP")
#


## ----show.classified, fig.width=map.fig.width, fig.height=map.fig.height-----------------------------------------
par(mfrow=c(n.figs.row, n.figs.col))
.l <- range(values(gssurgo.class), na.rm=TRUE)
terra::plot(gssurgo.class,
            col=color.ramp[.l[1]:.l[2]], type="classes",
            main="gSSURGO")
.l <- range(values(sg.class), na.rm=TRUE)
.l <- range(values(polaris.class), na.rm=TRUE)
terra::plot(polaris.class,
            col=color.ramp[.l[1]:.l[2]], type="classes",
            main="PSP")
terra::plot(sg.class,
            col=color.ramp[.l[1]:.l[2]], type="classes",
            main="SG2")
par(mfrow=c(1,1))


## ----xclass.1----------------------------------------------------------------------------------------------------
table(as.vector(gssurgo.class), as.vector(sg.class),
      useNA = "ifany")


## ----xclass.2a---------------------------------------------------------------------------------------------------
table(as.vector(gssurgo.class), as.vector(polaris.class),
      useNA = "ifany")


## ----xclass.3----------------------------------------------------------------------------------------------------
table(as.vector(sg.class), as.vector(polaris.class),
      useNA = "ifany")


## ----polygonize--------------------------------------------------------------------------------------------------
gssurgo.poly <- terra::as.polygons(gssurgo.class, dissolve=TRUE, trunc=FALSE)
sg.poly <- terra::as.polygons(sg.class, dissolve=TRUE, trunc=FALSE)
polaris.poly <- terra::as.polygons(polaris.class, dissolve=TRUE, trunc=FALSE)


## ----convert.polygons.sf-----------------------------------------------------------------------------------------
#
gssurgo.sf <- st_as_sf(gssurgo.poly)
gssursgo.sf <- st_cast(gssurgo.sf, "MULTIPOLYGON")
names(gssurgo.sf)[1] <- "class"
#
sg.sf <- st_as_sf(sg.poly)
sg.sf <- st_cast(sg.sf, "MULTIPOLYGON")
names(sg.sf)[1] <- "class"
#
polaris.sf <- st_as_sf(polaris.poly)
polaris.sf <- st_cast(polaris.sf, "MULTIPOLYGON")
names(polaris.sf)[1] <- "class"
#


## ----------------------------------------------------------------------------------------------------------------
# st_is_valid(gssurgo.sf, reason=TRUE)
gssurgo.sf.v <- sf::st_make_valid(gssurgo.sf)
# st_is_valid(gssurgo.sf.v, reason=TRUE)
#
# st_is_valid(sg.sf, reason=TRUE)
sg.sf.v <- sf::st_make_valid(sg.sf)
# st_is_valid(sg.sf.v, reason=TRUE)
st_is_valid(polaris.sf, reason=TRUE)
polaris.sf.v <- sf::st_make_valid(polaris.sf)
# st_is_valid(polaris.sf.v, reason=TRUE)


## ----vmaps.gssurgo.sg.compute------------------------------------------------------------------------------------
regions.gssurgo.sg <- vmeasure_calc(x = gssurgo.sf.v, 
                                 y = sg.sf.v, 
                                 x_name = class, y_name = class)
print(regions.gssurgo.sg)
names(regions.gssurgo.sg)
names(regions.gssurgo.sg$map1)


## ----------------------------------------------------------------------------------------------------------------
attr(regions.gssurgo.sg, "precision")  # NULL, means a system default


## ----vmaps.gssurgo.sg, fig.width=4, fig.height=6, eval=FALSE-----------------------------------------------------
##   ## produced maps -- the homogeneity of the regions.gssurgo.sg
## terra::plot(regions.gssurgo.sg$map1["rih"], main = "Homogeneity --  SG2 vs. gSSURGO")
## terra::plot(regions.gssurgo.sg$map2["rih"], main = "Completeness -- SG2 vs. gSSURGO")


## ----vmaps.gssurgo.polaris, compute------------------------------------------------------------------------------
par(mfrow=c(1, 2))
regions.gssurgo.polaris <- vmeasure_calc(x = gssurgo.sf.v, 
                                      y = polaris.sf.v, 
                                      x_name = class, y_name = class)
print(regions.gssurgo.polaris)
names(regions.gssurgo.polaris)
names(regions.gssurgo.polaris$map1)


## ----vmaps.gssurgo.polaris, fig.width=4, fig.height=6, eval=FALSE------------------------------------------------
## terra::plot(regions.gssurgo.polaris$map1["rih"], main = "Homogeneity -- PSP vs. gSSURGO")
## terra::plot(regions.gssurgo.polaris$map2["rih"], main = "Completeness -- PSP vs. gSSURGO")
## par(mfrow=c(1, 1))


## ----vmaps.sg.polaris.compute------------------------------------------------------------------------------------
regions.sg.polaris <- vmeasure_calc(x = sg.sf.v, 
                                    y = polaris.sf.v, 
                                    x_name = class, y_name = class)
print(regions.sg.polaris)
names(regions.sg.polaris)
names(regions.sg.polaris$map1)
## produced maps -- the homogeneity of the regions


## ----vmaps.sg.polaris, fig.width=4, fig.height=6, eval=FALSE-----------------------------------------------------
## par(mfrow=c(1, 2))
## terra::plot(regions.sg.polaris$map1["rih"], main = "Homogeneity -- PSP -- SG2")
## terra::plot(regions.sg.polaris$map2["rih"], main = "Completeness -- PSP -- SG2")
## par(mfrow=c(1, 1))


## ----------------------------------------------------------------------------------------------------------------
str(regions.gssurgo.sg, max.level = 1)
vmeasure.compare <- data.frame(Products = "", V_measure = 0, Homogeneity = 0, Completeness = 0)
vmeasure.compare[1,] <- c("SG2 vs. gSSURGO", 
                       round(regions.gssurgo.sg$v_measure, 4),
                       round(regions.gssurgo.sg$homogeneity, 4),
                       round(regions.gssurgo.sg$completeness, 4))
vmeasure.compare[2,] <- c("PSP vs. gSSURGO", 
                          round(regions.gssurgo.polaris$v_measure, 4),
                          round(regions.gssurgo.polaris$homogeneity, 4),
                          round(regions.gssurgo.polaris$completeness, 4))
vmeasure.compare[3,] <- c("PSP vs. SG2", 
                          round(regions.sg.polaris$v_measure, 4),
                          round(regions.sg.polaris$homogeneity, 4),
                          round(regions.sg.polaris$completeness, 4))
print(vmeasure.compare)


## ----------------------------------------------------------------------------------------------------------------
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
x <- xtable(vmeasure.compare, row.names=FALSE, digits=2)
autoformat(x)
capture.output(print(x, include.rownames=FALSE), file=
                 paste0("../LaTeX_tables/compare_vmeasure_local_",
                           AOI.dir.prefix, "_", voi.depth, ".tex"))


## ----------------------------------------------------------------------------------------------------------------
r.gssurgo <- raster(gssurgo.class)
r.sg <- raster(sg.class)
r.polaris <- raster(polaris.class)


## ----check.landscape---------------------------------------------------------------------------------------------
check_landscape(r.gssurgo)
check_landscape(r.sg)
check_landscape(r.polaris)


## ----show.landscape.function-------------------------------------------------------------------------------------
(my.pal <- c(brewer.pal(n.class, "RdYlGn"), "#FFFFFF"))
show.landscape <- function(r.map, r.title) {
  check_landscape(r.map)
  l <- range(values(r.map), na.rm = TRUE)
  g <- show_landscape(r.map, discrete = TRUE) +
    # use the positions in the palette corresponding to the class numbers
    # force the NA to the last entry in the palette
    scale_fill_manual(values=c(my.pal[(l[1]+1):(l[2]+1)], my.pal[n.class+1])) +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank()) +
    labs(title = r.title)
  return(g)
}


## ----show.landscape, fig.width=map.fig.width, fig.height=map.fig.height------------------------------------------
g1<- show.landscape(r.gssurgo, "gSSURGO")
g2 <- show.landscape(r.polaris, "PSP")
g3 <- show.landscape(r.sg, "SG2")
g3
grid.arrange(g1, g2, g3, nrow=1, ncol=3)


## ----show.core.areas, fig.width=12, fig.height=12----------------------------------------------------------------
show_cores(r.gssurgo)
# show_cores(r.sg)
# if (exists("r.gsm")) show_cores(r.gsm)
# if (exists("r.issr8")) show_cores(r.issr8)
# if (exists("r.psu")) show_cores(r.psu)
# if (exists("r.polaris")) show_cores(r.polaris)
# if (exists("r.landgis")) show_cores(r.landgis)


## ----show.patch.level.metrics, fig.width=4, fig.height=5---------------------------------------------------------
show_lsm(r.gssurgo, what="lsm_p_contig")
# show_lsm(r.sg, what="lsm_p_contig")
# if (exists("r.gsm")) show_lsm(r.gsm, what="lsm_p_contig")
# if (exists("r.issr8")) show_lsm(r.issr8, what="lsm_p_contig")
# if (exists("r.psu")) show_lsm(r.psu, what="lsm_p_contig")
# if (exists("r.polaris")) show_lsm(r.polaris, what="lsm_p_contig")
# if (exists("r.landgis")) show_lsm(r.landgis, what="lsm_p_contig")


## ----------------------------------------------------------------------------------------------------------------
lst <- paste0("lsm_l_", c("shdi", "shei", "lsi", "ai", "frac_mn"))
ls.metrics.gssurgo <- calculate_lsm(r.gssurgo, what=lst)
ls.metrics.sg <- calculate_lsm(r.sg, what=lst)
ls.metrics.polaris <- calculate_lsm(r.polaris, what=lst)


## ----------------------------------------------------------------------------------------------------------------
metrics.table <- data.frame(product=c("gSSURGO", "SG2", "PSP"),
                            rbind(round(ls.metrics.gssurgo$value, 3),
                                  round(ls.metrics.sg$value, 3),
                                  round(ls.metrics.polaris$value, 3)))
names(metrics.table)[2:6] <- ls.metrics.gssurgo$metric


## ----metrics.table-----------------------------------------------------------------------------------------------
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
x <- xtable(metrics.table, row.names=FALSE, digits=3)
autoformat(x)
capture.output(print(x, include.rownames=FALSE), file=
                 paste0("../LaTeX_tables/landscape_metrics_local_",
                           AOI.dir.prefix, "_", voi.depth, ".tex"))


## ----metrics.cove------------------------------------------------------------------------------------------------
library(motif) # `lsp_signature`
library(stars) # `motif` functions require this format
# normalized co-occurence vector 8 x 8
# plot(st_as_stars(r.sg))
# plot(st_as_stars(r.gssurgo))
# plot(st_as_stars(r.polaris))
#
cove.sg <- lsp_signature(st_as_stars(r.sg), type="cove")
cove.gssurgo <- lsp_signature(st_as_stars(r.gssurgo), type="cove")
cove.polaris <- lsp_signature(st_as_stars(r.polaris), type="cove")


## ----distance.cove-----------------------------------------------------------------------------------------------
# combine the vectors into a dataframe, one row per vector
cove.df <- data.frame(cove.gssurgo)$signature[[1]][1,]
cove.df <- rbind(cove.df, cove.sg$signature[[1]][1,])
cove.df <- rbind(cove.df, cove.polaris$signature[[1]][1,])
row.names(cove.df) <- c( "gSSURGO", "SG2", "PSP")
# compute the distances
#
# philentropy::getDistMethods()
cove.dists <- round(
  philentropy::distance(cove.df, method = "jensen-shannon", 
                        use.row.names =TRUE, 
                        as.dist.obj = TRUE,
                        diag = FALSE)
  ,4)
print(cove.dists)


## ----table.cove--------------------------------------------------------------------------------------------------
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
x <- xtable(as.matrix(cove.dists), row.names=TRUE, digits=3)
autoformat(x)
capture.output(print(x, include.rownames=TRUE), file=
                 paste0("../LaTeX_tables/compare_landscape_patterns_local_",
                           AOI.dir.prefix, "_", voi.depth, ".tex"))


params <-
list(lrc_long = -120L, lrc_lat = 37L, size = 1L, voi.n = 3L, 
    quantile.n = "NA", depth.n = 3L)

## ----compare.which-----------------------------------------------------------------------------------------------
products <- c("POLARIS", "SPCG100USA") #, "LandGIS", "ISSR-800", "GSM v0.5"


## ----------------------------------------------------------------------------------------------------------------
n.products <- 2 + length(products)
n.figs.row <- ceiling(sqrt(n.products))
n.figs.col <- ceiling(n.products/n.figs.row)
map.fig.width <- n.figs.col*5
map.fig.height <- n.figs.row*5
# difference maps have one fewer subfigure, put one more per row
n.figs.row.diff <- n.figs.row-1
n.figs.col.diff <- n.figs.col+1
map.fig.width.diff <- n.figs.col.diff*4
map.fig.height.diff <- n.figs.row.diff*4


## ----------------------------------------------------------------------------------------------------------------
library(rgdal)      # R interface to GDAL
library(terra)      # for raster maps
library(sf)         # Simple Features spatial data
# library(gridExtra)  # arrange multiple plots
library(knitr)      # for fancy tables
library(xtable)     # (same)


## ----base.dir----------------------------------------------------------------------------------------------------
base.dir <- "/Volumes/Pythagoras/ds/DSM_export/"
base.dir.gnatsgo <- paste0(base.dir, "gNATSGO")
base.dir.sg <- paste0(base.dir, "SoilGrids250")
base.dir.gsm <- paste0(base.dir, "GSM_USA")
base.dir.issr8 <- paste0(base.dir, "ISSR8")
base.dir.polaris <- paste0(base.dir, "POLARIS")
base.dir.psu <- paste0(base.dir, "SPCG100USA")
base.dir.landgis <- paste0(base.dir, "LandGIS")


## ----base.dir.import---------------------------------------------------------------------------------------------
base.dir.import <- "/Volumes/Pythagoras/ds/DSM_import/"
base.dir.polaris.import <- paste0(base.dir.import, "POLARIS")


## ----base.dir.compare--------------------------------------------------------------------------------------------
base.dir.compare <- paste0("/Volumes/Pythagoras/ds/Compare_PSM")


## ----------------------------------------------------------------------------------------------------------------
print(paste("lrc_long:", params$lrc_long, "; lrc_lat:", params$lrc_lat, "; size:", params$size))
print(paste("voi.n:", params$voi.n, "; depth.n:", params$depth.n))


## ----------------------------------------------------------------------------------------------------------------
voi.list.gnatsgo <- c("claytotal_r", "silttotal_r", "sandtotal_r",
                  "ph1to1h2o_r", "cec7_r", "om_r",   # note SOM not SOC
                  "dbthirdbar_r", "sieveno10_r") # passing 2.0 mm sieve, complement is coarse fragments
voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
voi.list.gsm <- c("claytotal_r_g_kg", "silttotal_r_g_kg", "sandtotal_r_g_kg", 
                  "ph1to1h2o_r_ions_pHx10", "ecec_r_cmolc_kg", "soc_r_mr_g_gF",
                  "dbthirdbar_lt2mm_r_g_cm3", "gravel_r_vol_ratio_m3_m3")
voi.list.issr8 <- c("clay", "silt","sand","ph", "cec", "", "", "")
voi.list.polaris <- c("clay", "silt", "sand", "ph", "", "om", "bd", "") 
voi.list.psu <- c("clay", "", "sand", "ph_h2o", "", "soc", "bd", "")
voi.list.landgis <- c("clay.wfraction_usda.3a1a1a",
                      "silt.wfraction_usda.3a1a1a",
                      "sand.wfraction_usda.3a1a1a",
                      "ph.h2o_usda.4c1a2a",
                      "",
                      "organic.carbon_usda.6a1c",
                      "bulkdens.fineearth_usda.4a1h",
                      "coarsefrag.vfraction_usda_3b1")


## ----------------------------------------------------------------------------------------------------------------
voi.n <- params$voi.n   # variable of interest, SoilGrids name
voi.gnatsgo <- voi.list.gnatsgo[voi.n]
voi.sg <- voi.list.sg[voi.n]
voi.issr8 <- voi.list.issr8[voi.n]
voi.polaris <- voi.list.polaris[voi.n]
voi.psu <- voi.list.psu[voi.n]
voi.gsm <- voi.list.gsm[voi.n]


## ----------------------------------------------------------------------------------------------------------------
depth.list.gnatsgo <- c("05", "515", "1530", "3060", "60100", "100200")
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
# SPCGUSA100 predicts at points, these were averaged to GSM slices during import
# --- these have the SG names
# LandGIS predicts at points, these were averaged to GSM slices during import
# --- these have the SG names
depth.list.polaris <- gsub("-", "_", depth.list.sg)
depth.list.issr8 <- gsub("-", "", depth.list.sg)
depth.list.gsm <- c("000_005", "005_015", "015_030", "030_060", "060_100", "100_200")


## ----------------------------------------------------------------------------------------------------------------
depth <- params$depth.n


## ----lrc---------------------------------------------------------------------------------------------------------
tile.lrc <- c(params$lrc_long, params$lrc_lat) # lower-right corner
tile.size <- params$size                # tile dimensions


## ----ulc---------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-tile.size, tile.lrc[2]+tile.size) # upper-left corner


## ----aoi.dir.prefix----------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## ----adjust.fig.path---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.path = paste0(knitr::opts_chunk$get("fig.path"), 
                                        AOI.dir.prefix, "/",
                                        voi.sg, "_", depth.list.sg[depth], "_"))


## ----bbox.4326---------------------------------------------------------------------------------------------------
m <- matrix(c(tile.ulc[1],tile.lrc[1],  #ulc
              tile.ulc[2], tile.lrc[2]), nrow=2) #lrc
bb.ll <- st_sfc(st_multipoint(m))
st_crs(bb.ll) <- 4326   # ESPG code for WGS84 long/lat


## ----bbox.igh----------------------------------------------------------------------------------------------------
# convert to Homolosine. Note epsg=152160 is not in PROJ4 database
crs.igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
(bb.igh <- st_transform(bb.ll, crs.igh))
(bb.igh <- st_coordinates(bb.igh)[,1:2])
(bb <- as.vector(t(bb.igh)))


## ----bbox.aea----------------------------------------------------------------------------------------------------
crs.aea <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
(bb.aea <- st_transform(bb.ll, crs.aea))


## ----get.tiles.gnatsgo-------------------------------------------------------------------------------------------
src.dir <-  paste0(base.dir.gnatsgo ,"/", 
                   AOI.dir.prefix)
(voi.depth.name <- paste0(voi.gnatsgo, "_", depth.list.gnatsgo[depth]))
(file.name <- paste0(src.dir, "/", voi.depth.name, '.tif'))
if (file.exists(file.name)) {
  r.gnatsgo <- terra::rast(file.name)
  names(r.gnatsgo) <- "gnatsgo"
  print(r.gnatsgo)
  summary(r.gnatsgo)
} else { stop("No gNATSGO tile, stopping") }


## ----get.tiles.sg------------------------------------------------------------------------------------------------
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
  summary(r.sg)
} else { stop("No SoilGrids250 tile, stopping") }


## ----get.tiles.gsm-----------------------------------------------------------------------------------------------
# GSM
if ("GSM v0.5" %in% products) {
  src.dir <-  paste0(base.dir.gsm ,"/", AOI.dir.prefix)
  voi.depth.name <- paste0(voi.gsm, "_", depth.list.gsm[depth])
  (file.name <- paste0(src.dir, "/GSM_mu_", voi.depth.name, ".tif"))
  if (file.exists(file.name)) {
    r.gsm <- terra::rast(file.name)
    names(r.gsm) <- "GSMv0.5"
    print(r.gsm)
    summary(r.gsm)
  } 
}


## ----get.tiles.psu-----------------------------------------------------------------------------------------------
# SPCG100USA
if ("SPCG100USA" %in% products) {
  src.dir <-  paste0(base.dir.psu ,"/", AOI.dir.prefix)
  (file.name <- paste0(src.dir, "/", voi.psu, "_", depth.list.sg[depth], '.tif'))
  if (file.exists(file.name)) {
    r.psu <- terra::rast(file.name)
    names(r.psu) <- "spcg100usa"
    print(r.psu)
    summary(r.psu)
  }
}


## ----get.tiles.polaris-------------------------------------------------------------------------------------------
# POLARIS -- only the mean prediction in this script
if ("POLARIS" %in% products) {
  (file.name <- paste0(base.dir.polaris.import, "/",
                       AOI.dir.prefix, "/",
                       voi.list.polaris[params$voi.n], "/mean/",
                       depth.list.polaris[depth], "/",
                       AOI.dir.prefix, ".tif"))
  if (file.exists(file.name)) {
    r.p <- terra::rast(file.name)
    names(r.p) <- "polaris"
    print(r.p)
    summary(r.p)
  }
}


## ----get.tiles.landgis-------------------------------------------------------------------------------------------
# LandGIS -- only the mean prediction in this script
if ("LandGIS" %in% products) {
  (file.name <- paste0(base.dir.landgis, "/",
                       AOI.dir.prefix, "/",
                       voi.list.landgis[voi.n], "_",
                       depth.list.sg[depth],
                       ".tif"))
  if (file.exists(file.name)) {
    r.landgis <- terra::rast(file.name)
    names(r.landgis) <- "landgis"
    print(r.landgis)
    summary(r.landgis)
  }
}


## ----get.tiles.issr8---------------------------------------------------------------------------------------------
if ("ISSR-800" %in% products) {
  (file.name <- paste0(base.dir.issr8, "/",
                       AOI.dir.prefix, "/",
                       voi.list.issr8[voi.n], "_",
                       depth.list.issr8[depth],
                       ".tif"))
  if (file.exists(file.name)) {
    r.issr8 <- terra::rast(file.name)
    names(r.issr8) <- "ISSR-800"
    print(r.issr8)
    summary(r.issr8)
  }
}


## ----show.conversions--------------------------------------------------------------------------------------------
df <- data.frame(property=voi.list.sg, 
                 #"clay"  "silt"  "sand"  "phh2o" "cec"   "soc"   "bdod"  "cfvo" 
                 sg=c("%%","%%","%%","pHx10","mmol(c)/kg","dg/kg","cg/cm3", "cm3/dm3"),  #SG
                 ## metadata for gNATSGO:
                 ## https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053631
                 gnatsgo=c("%","%","%","pH","meq/Cg","1.724 * %","g/cm3", "100-%"),  # gNATSGO
                 # sieveno10_r is complement of cfvo; som not soc
                 gsm=c("%%","%%","%%","pHx10","cmol(c)/kg","g/gF","Mg/m3", "m3/m3"),  # GSM
                 p=c("%","%", "%","pH","", "log10(1.724 * %)", "g/cm3", ""),  # POLARIS
                 spcg=c("%","%","%","pHx10", "", "%%","g/cm3", ""),  # SPCG100USA
                 lgis=c("%","%","%","pHx10","","5g/Kg","10 kg/m3", "%"),  # LandGIS
                 issr=c("%","%","%","pH","cmol(+)/kg","","g/cm3", "%")  # ISSR-800
                 )
knitr::kable(
  df, caption = 'Properties and units of measure',
  col.names=c("Property", "SoilGrids", "gNATSGO", "GlobalSoilMap",
              "POLARIS", "SPCG100USA", "LandGIS", "ISSR-800"),
  booktabs = TRUE)


## ----make.conversion.matrix--------------------------------------------------------------------------------------
som.to.soc <- 1/1.724138 # this was used in the lab, I know it has been heavily criticized
conversions <- data.frame(property=voi.list.sg, 
                 # sg=c("%%","%%","%%","pHx10","mmol(c)/kg","dg/kg","cg/cm3", "cm3/dm3"), #SG
                 gnatsgo=c(10, 10, 10, 10 , 10, 1000*som.to.soc, 100, NA),  # gNATSGO
                 gsm=c(1, 1, 1, 1 , 10, 0.01, 0.01, 0.1),  # GSM
                 p=c(10,  10, 10,  10, NA, NA, 100, NA),  # POLARIS -- SOM is special case
                 spcg=c(10,  10, 10, 1, NA,  10,   100, NA),  # SPCG100USA
                 lgis=c(10,  10, 10, 1, NA, 20, 0.1, 0.1),  # LandGIS
                 issr=c(10,  10, 10,  10, 10, NA, 1, 0.1)  # ISSR-800
                 )
conversions <- format(conversions, digits=4, nsmall=0, scientific=FALSE)
knitr::kable(
  conversions, caption = 'Conversion factors, multiply by these to match SoilGrids250',
  col.names=c("Property", "gNATSGO", "GlobalSoilMap", 
              "POLARIS", "SPCG100USA", "LandGIS", "ISSR-800"),
  booktabs = TRUE,
  align = "r")


## ----polaris.soc-------------------------------------------------------------------------------------------------
if (exists("r.p") && (voi.sg=="soc")) {
    r.p <- (10^r.p)*som.to.soc*1000 
}


## ----gnatsgo.cfvo------------------------------------------------------------------------------------------------
if (voi.sg == "cfvo") {
  r.gnatsgo <- (100 - r.natsgo)*0.1
}


## ----convert-----------------------------------------------------------------------------------------------------
# this property's factors
(factors <- conversions[match(voi.sg, conversions$property),])

# gNATSGO
fact <- as.numeric(factors["gnatsgo"])
if (!is.na(fact) && (fact != 1)) { r.gnatsgo <- r.gnatsgo*fact }

# GSM
if (exists("r.gsm")) {
  fact <- as.numeric(factors["gsm"])
  if (!is.na(fact) && (fact != 1)) { r.gsm <- r.gsm*fact }
}

# POLARIS
if (exists("r.p")) {
  fact <- as.numeric(factors["p"])
  if (!is.na(fact) && (fact != 1)) { r.p <- r.p*fact }
}

# SPCG100USA
if (exists("r.psu")) {
  fact <- as.numeric(factors["spcg"])
  if (!is.na(fact) && (fact != 1)) { r.psu <- r.psu*fact }
}

# LandGIS
if (exists("r.landgis")) {
  fact <- as.numeric(factors["lgis"])
  if (!is.na(fact) && (fact != 1)) { r.landgis <- r.landgis*fact }
}

# ISSR-800
if (exists("r.issr8")) {
  fact <- as.numeric(factors["issr"])
  if (!is.na(fact) && (fact != 1)) { r.issr8 <- r.issr8*fact }
}


## ----------------------------------------------------------------------------------------------------------------
summary(r.gnatsgo)
summary(r.p)
summary(r.psu)
summary(r.sg)


## ----crs.show.1--------------------------------------------------------------------------------------------------
rgdal::showP4(crs(r.sg))
data.frame(sg=res(r.sg)[1], 
           polaris=ifelse(exists("r.p"), res(r.p)[1], ""),
           gsm=ifelse(exists("r.gsm"), res(r.gsm)[1], ""),
           landgis=ifelse(exists("r.landgis"), res(r.landgis)[1], ""))


## ----crs.show.2--------------------------------------------------------------------------------------------------
rgdal::showP4(crs(r.gnatsgo))
if (exists("r.psu")) rgdal::showP4(crs(r.psu))
if (exists("r.issr8")) rgdal::showP4(crs(r.issr8))


## ----crs.resample------------------------------------------------------------------------------------------------
r.gnatsgo.sg <- terra::resample(r.gnatsgo, r.sg, method="cubic")
if (exists("r.gsm")) { r.gsm.sg <- terra::resample(r.gsm, r.sg, method="cubic") }
if (exists("r.p")) { r.p.sg <- terra::resample(r.p, r.sg, method="cubic") }
if (exists("r.psu")) { r.psu.sg <- terra::resample(r.psu, r.sg, method="cubic") }


## ----crs.project-------------------------------------------------------------------------------------------------
if (exists("r.landgis")) { 
  r.landgis.sg <- terra::project(r.landgis, r.sg, method="cubic") 
  }
if (exists("r.issr8")) { 
  r.issr8.sg <- terra::project(r.issr8, r.sg, method="cubic") 
  }


## ----mask.lakes--------------------------------------------------------------------------------------------------
r.gnatsgo.sg <- mask(r.gnatsgo.sg, r.sg)
if (exists("r.gsm.sg")) { r.gsm.sg <- mask(r.gsm.sg, r.sg) }
if (exists("r.p.sg")) { r.p.sg <- mask(r.p.sg, r.sg) }
if (exists("r.issr8.sg")) { r.issr8.sg <- mask(r.issr8.sg, r.sg) }
if (exists("r.psu.sg")) { r.psu.sg <- mask(r.psu.sg, r.sg) }
if (exists("r.landgis.sg")) { r.landgis.sg <- mask(r.landgis.sg, r.sg) }


## ----make.polygon------------------------------------------------------------------------------------------------
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


## ----------------------------------------------------------------------------------------------------------------
r.gnatsgo.sg <- crop(r.gnatsgo.sg, bb.poly)
r.sg <- crop(r.sg, bb.poly)
if (exists("r.gsm.sg")) { r.gsm.sg <- crop(r.gsm.sg, bb.poly) }
if (exists("r.p.sg")) { r.p.sg <- crop(r.p.sg, bb.poly) }
if (exists("r.issr8.sg")) { r.issr8.sg <- crop(r.issr8.sg, bb.poly) }
if (exists("r.psu.sg")) { r.psu.sg <- crop(r.psu.sg, bb.poly) }
if (exists("r.landgis.sg")) { r.landgis.sg <- crop(r.landgis.sg, bb.poly) }


## ----plot.resampled, fig.width=map.fig.width, fig.height=map.fig.height------------------------------------------
par(mfrow=c(n.figs.row, n.figs.col))
plot(r.gnatsgo.sg, main="gNATSGO")
plot(r.sg, main="SG2")
if (exists("r.gsm.sg")) { plot(r.gsm.sg, main="GSM v0.5") }
if (exists("r.p.sg")) { plot(r.p.sg, main="PSP") }
if (exists("r.psu.sg")) { plot(r.psu.sg, main="SPCG") }
if (exists("r.landgis.sg")) { plot(r.landgis.sg, main="LandGIS") }
if (exists("r.issr8.sg")) { plot(r.issr8.sg, main="ISSR-800") }
par(mfrow=c(1, 1))


## ----compare.zlim------------------------------------------------------------------------------------------------
zlim <- c(min(values(r.sg)*10, na.rm = TRUE),
          max(values(r.sg)*10, na.rm = TRUE))/10
zlim <- c(floor(min(zlim[1]*10, values(r.gnatsgo.sg)*10, na.rm=TRUE)),
          ceiling(max(zlim[2]*10, values(r.gnatsgo.sg)*10, na.rm=TRUE)))/10
if (exists("r.gsm.sg")) {
  zlim <- c(floor(min(zlim[1]*10, values(r.gsm.sg)*10, na.rm=TRUE)),
          ceiling(max(zlim[2]*10, values(r.gsm.sg)*10, na.rm=TRUE)))/10
}
if (exists("r.p.sg")) {
  zlim <- c(floor(min(zlim[1]*10, values(r.p.sg)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(r.p.sg)*10, na.rm=TRUE)))/10
}
if (exists("r.psu.sg")) {
  zlim <- c(floor(min(zlim[1]*10, values(r.psu.sg)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(r.psu.sg)*10, na.rm=TRUE)))/10
}
if (exists("r.landgis.sg")) {
  zlim <- c(floor(min(zlim[1]*10, values(r.landgis.sg)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(r.landgis.sg)*10, na.rm=TRUE)))/10
}
if (exists("r.issr8.sg")) {
  zlim <- c(floor(min(zlim[1]*10, values(r.issr8.sg)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(r.issr8.sg)*10, na.rm=TRUE)))/10
}


## ----hist.densities----------------------------------------------------------------------------------------------
max.dens <- function(r.map) {  # argument: the raster map
  h <- hist(r.map,  breaks=24, plot = FALSE)
  max(h$counts/(diff(h$breaks[1:2]))/sum(h$counts))
}
yl <- max(max.dens(r.gnatsgo.sg), max.dens(r.sg))
if (exists("r.gsm.sg")) { yl <- max(yl, max.dens(r.sg)) }
if (exists("r.p.sg")) { yl <- max(yl, max.dens(r.p.sg))}
if (exists("r.psu.sg")) { yl <- max(yl, max.dens(r.psu.sg))}
if (exists("r.landgis.sg")) { yl <- max(yl, max.dens(r.landgis.sg))}
if (exists("r.issr8.sg")) { yl <- max(yl, max.dens(r.issr8.sg))}
yl <- c(0, yl) # standardize the density axis


## ----hist.sg.props, fig.width=map.fig.width, fig.height=map.fig.height-------------------------------------------
par(mfrow=c(n.figs.row, n.figs.col))
hist(r.sg, breaks=24, main="SG2",
     xlim=zlim, xlab="", freq = FALSE, ylim=yl)
hist(r.gnatsgo.sg, breaks=24, main="gNATSGO",
     xlim=zlim, xlab="", freq = FALSE, ylim=yl)
if (exists("r.gsm.sg")) {
  hist(r.gsm.sg, breaks=24, main="GSM v0.5",
       xlim=zlim, xlab="", freq = FALSE, ylim=yl) 
}
if (exists("r.p.sg")) {
  hist(r.p.sg, breaks=24, main="PSP",
       xlim=zlim, xlab="", freq = FALSE, ylim=yl)
}
if (exists("r.psu.sg")) {
  hist(r.psu.sg, breaks=24, main="SPCG",
       xlim=zlim, xlab="", freq = FALSE, ylim=yl)
}
if (exists("r.landgis.sg")) {
  hist(r.landgis.sg, breaks=24, main="LandGIS",
       xlim=zlim, xlab="", freq = FALSE, ylim=yl)
}
if (exists("r.issr8.sg")) {
  hist(r.issr8.sg, breaks=24, main="ISSR-800",
       xlim=zlim, xlab="", freq = FALSE, ylim=yl)
}
par(mfrow=c(1,1))


## ----map.sg.props, fig.width=map.fig.width, fig.height=map.fig.height--------------------------------------------
par(mfrow=c(n.figs.row, n.figs.col))
terra::plot(r.gnatsgo.sg, main="gNATSGO", range=zlim)
terra::plot(r.sg, main="SG2", range=zlim)
if (exists("r.gsm.sg")) {
  terra::plot(r.gsm.sg, main="GSM v0.5", range=zlim)
}
if (exists("r.p.sg")) {
  terra::plot(r.p.sg, main="PSP", range=zlim)
}
if (exists("r.psu.sg")) {
  terra::plot(r.psu.sg, main="SPCG", range=zlim)
}
if (exists("r.landgis.sg")) {
  terra::plot(r.landgis.sg, main="LandGIS", range=zlim)
}
if (exists("r.issr8.sg")) {
  terra::plot(r.issr8.sg, main="ISSR-800", range=zlim)
}
par(mfrow=c(1,1))


## ----pairwise----------------------------------------------------------------------------------------------------
v.all <- data.frame(gNATSGO=values(r.gnatsgo.sg),
                    SG2=values(r.sg),
                    GSM=NA,
                    PSP=NA,
                    SPCG=NA,
                    LandGIS=NA,
                    ISSR8=NA)
# remove any fields w/o data, so correlation only shows the products we want to compare
if (exists("r.gsm.sg")) { v.all$GSM <- values(r.gsm.sg) }
if (exists("r.p.sg")) { v.all$PSP <- values(r.p.sg) }
if (exists("r.psu.sg")) { v.all$SPCG <- values(r.psu.sg) }
if (exists("r.landgis.sg")) { v.all$LandGIS <-  values(r.landgis.sg) } 
if (exists("r.issr8.sg")) { v.all$ISSR8 <- values(r.issr8.sg) }
# reset the names to the abbreviations
colnames(v.all) <- c("gNATSGO", "SG2", "GSM", "PSP", "SPCG", "LandGIS", "ISSR8")
# remove all-NA columns from the data frame before computing correlations
v.all <- v.all[,colSums(!is.na(v.all))>0]
cor.all <- cor(v.all, use="pairwise.complete.obs")
cor.upper <- cor.all
cor.upper[lower.tri(cor.upper)] <- NA
print(round(cor.upper, 3))


## ----corrplot, fig.width=(n.figs.col*2+1), fig.height=n.figs.col*2+1---------------------------------------------
library(corrplot)
corrplot.mixed(cor.all, upper="ellipse", lower="number", diag="n",
               lower.col = "black")


## ----------------------------------------------------------------------------------------------------------------
diff.gnatsgo.sg <- r.gnatsgo.sg - r.sg
if (exists("r.gsm.sg")) { diff.gnatsgo.gsm <- r.gnatsgo.sg - r.gsm.sg }
if (exists("r.p.sg")) {  diff.gnatsgo.p <-  r.gnatsgo.sg - r.p.sg }
if (exists("r.psu.sg")) {  diff.gnatsgo.psu <-  r.gnatsgo.sg - r.psu.sg }
if (exists("r.landgis.sg")) {  diff.gnatsgo.landgis <-  r.gnatsgo.sg - r.landgis.sg }
if (exists("r.issr8.sg")) { diff.gnatsgo.issr8 <-  r.gnatsgo.sg - r.issr8.sg }


## ----stats.compare.sg, warning=FALSE-----------------------------------------------------------------------------
stats.compare <- data.frame(Product = "", MD = 0, RMSD = 0, RMSD.Adjusted = 0)
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
                        me(values(r.gnatsgo.sg),values(r.sg)),
                        rmse(values(r.gnatsgo.sg),values(r.sg)),
                        rmse.adj(values(r.gnatsgo.sg),values(r.sg))
); i <- 2
if (exists("r.gsm.sg")) {
  stats.compare[i, ] <- c("GSM v0.5",
                        me(values(r.gnatsgo.sg),values(r.gsm.sg)),
                        rmse(values(r.gnatsgo.sg),values(r.gsm.sg)),
                        rmse.adj(values(r.gnatsgo.sg),values(r.gsm.sg))
                        ); i <- i + 1
}
if (exists("r.p.sg")) {
  stats.compare[i, ] <- c("PSP",
                        me(values(r.gnatsgo.sg),values(r.p.sg)),
                        rmse(values(r.gnatsgo.sg),values(r.p.sg)),
                        rmse.adj(values(r.gnatsgo.sg),values(r.p.sg))
                        ); i <- i + 1
}
if (exists("r.psu.sg")) {
  stats.compare[i, ] <- c("SPCG",
                        me(values(r.gnatsgo.sg),values(r.psu.sg)),
                        rmse(values(r.gnatsgo.sg),values(r.psu.sg)),
                        rmse.adj(values(r.gnatsgo.sg),values(r.psu.sg))
                        ); i <- i + 1
}
if (exists("r.landgis.sg")) {
  stats.compare[i, ] <- c("LandGIS",
                        me(values(r.gnatsgo.sg),values(r.landgis.sg)),
                        rmse(values(r.gnatsgo.sg),values(r.landgis.sg)),
                        rmse.adj(values(r.gnatsgo.sg),values(r.landgis.sg))
                        ); i <- i + 1
}
if (exists("r.issr8.sg")) {
  stats.compare[i, ] <- c("ISSR-800",
                        me(values(r.gnatsgo.sg),values(r.issr8.sg)),
                        rmse(values(r.gnatsgo.sg),values(r.issr8.sg)),
                        rmse.adj(values(r.gnatsgo.sg),values(r.issr8.sg))
                        )
}


## ----save.stats.compare.sg---------------------------------------------------------------------------------------
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
x <- xtable(stats.compare, row.names=FALSE, digits=3)
autoformat(x)
capture.output(print(x, include.rownames=FALSE), 
               file=paste0("../LaTeX_tables/SoilGrids250_compare_statistics_",
                           AOI.dir.prefix, "_", voi.sg, "_", depth.list.sg[depth], ".tex"))


## ----zlim.diff.sg------------------------------------------------------------------------------------------------
zlim <- c(NA, NA)
zlim <- c(floor(min(zlim[1]*10, values(diff.gnatsgo.sg)*10, na.rm=TRUE)),
          ceiling(max(zlim[2]*10, values(diff.gnatsgo.sg)*10, na.rm=TRUE)))/10
if (exists("diff.gnatsgo.gsm")) {
  zlim <- c(floor(min(zlim[1]*10, values(diff.gnatsgo.gsm)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(diff.gnatsgo.gsm)*10, na.rm=TRUE)))/10
}
if (exists("diff.gnatsgo.p")) {
  zlim <- c(floor(min(zlim[1]*10, values(diff.gnatsgo.p)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(diff.gnatsgo.p)*10, na.rm=TRUE)))/10
}
if (exists("diff.gnatsgo.psu")) {
  zlim <- c(floor(min(zlim[1]*10, values(diff.gnatsgo.psu)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(diff.gnatsgo.psu)*10, na.rm=TRUE)))/10
}
if (exists("diff.gnatsgo.landgis")) {
  zlim <- c(floor(min(zlim[1]*10, values(diff.gnatsgo.landgis)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(diff.gnatsgo.landgis)*10, na.rm=TRUE)))/10
}
if (exists("diff.gnatsgo.issr8")) {
  zlim <- c(floor(min(zlim[1]*10, values(diff.gnatsgo.issr8)*10, na.rm=TRUE)),
            ceiling(max(zlim[2]*10, values(diff.gnatsgo.issr8)*10, na.rm=TRUE)))/10
}


## ----diff.hist.densities-----------------------------------------------------------------------------------------
yl <- max(max.dens(diff.gnatsgo.sg))
if (exists("r.gsm.sg")) { yl <- max(yl, max.dens(diff.gnatsgo.gsm))}
if (exists("r.p.sg")) { yl <- max(yl, max.dens(diff.gnatsgo.p))}
if (exists("r.psu.sg")) { yl <- max(yl, max.dens(diff.gnatsgo.psu))}
if (exists("r.landgis.sg")) { yl <- max(yl, max.dens(diff.gnatsgo.landgis))}
if (exists("r.issr8.sg")) { yl <- max(yl, max.dens(diff.gnatsgo.issr8))}


## ----hist.diff.sg, fig.width=map.fig.width.diff, fig.height=map.fig.height.diff----------------------------------
yl <- c(0, yl) # Standardize density axis
par(mfrow=c(n.figs.row.diff, n.figs.col.diff))
hist(diff.gnatsgo.sg, main="gNATSGO - SG2", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
if (exists("r.gsm.sg")) { 
  hist(diff.gnatsgo.gsm, main="gNATSGO - GSM v0.5", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
}
if (exists("r.p.sg")) {
  hist(diff.gnatsgo.p, main="gNATSGO - PSP", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
}
if (exists("r.psu.sg")) {
  hist(diff.gnatsgo.psu, main="gNATSGO - SPCG", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
}
if (exists("r.landgis.sg")) {
  hist(diff.gnatsgo.landgis, main="gNATSGO - LandGIS", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
}
if (exists("r.issr8.sg")) {
  hist(diff.gnatsgo.p, main="gNATSGO - ISSR-800", xlab="",
     xlim=zlim,  breaks=24, freq = FALSE, ylim=yl)
}
par(mfrow=c(1,1))


## ----plot.diff.sg, fig.width=map.fig.width.diff, fig.height=map.fig.height.diff----------------------------------
par(mfrow=c(n.figs.row.diff, n.figs.col.diff))
  terra::plot(diff.gnatsgo.sg, main="Difference gNATSGO - SG2",
            range=zlim, col=bpy.colors(64))
if (exists("r.gsm.sg")) { 
  terra::plot(diff.gnatsgo.gsm, main="Difference gNATSGO - GSM v0.5",
            range=zlim, col=bpy.colors(64))
}
if (exists("diff.gnatsgo.p")) {
  terra::plot(diff.gnatsgo.p, main="Difference gNATSGO - PSP",
            range=zlim, col=bpy.colors(64))
}
if (exists("diff.gnatsgo.psu")) {
  terra::plot(diff.gnatsgo.psu, main="Difference gNATSGO - SPCG",
            range=zlim, col=bpy.colors(64))
}
if (exists("diff.gnatsgo.landgis")) {
  terra::plot(diff.gnatsgo.landgis, main="Difference gNATSGO - LandGIS",
            range=zlim, col=bpy.colors(64))
}
if (exists("diff.gnatsgo.issr8")) {
  terra::plot(diff.gnatsgo.issr8, main="Difference gNATSGO - ISSR-800",
            range=zlim, col=bpy.colors(64))
}
par(mfrow=c(1,1))


## ----save.dir----------------------------------------------------------------------------------------------------
dest.dir.save <-  file.path(base.dir.compare,
                       AOI.dir.prefix)
if (!dir.exists(dest.dir.save)) {
   dir.create(dest.dir.save, recursive = TRUE)
}


## ----save.tiles--------------------------------------------------------------------------------------------------
# gNATSGO
voi.depth.sg <- paste0(voi.sg, "_", depth.list.sg[depth])
dest.name <- paste0(dest.dir.save,"/gnatsgo_tile_250_",  voi.depth.sg, ".tif")
f <- terra::writeRaster(r.gnatsgo.sg, file=dest.name,
                        overwrite=TRUE,
                        wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
                        filetype="GTiff")
# GDALinfo(dest.name)
print(paste("Wrote ", dest.name))
# SoilGrids250
dest.name <- paste0(dest.dir.save,"/sg_tile_250_",  voi.depth.sg, ".tif")
f <- terra::writeRaster(r.sg, file=dest.name,
                        overwrite=TRUE,
                        wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
                        filetype="GTiff")
# GDALinfo(dest.name)
print(paste("Wrote ", dest.name))
# GSM v0.5
if (exists("r.gsm.sg")) { 
  dest.name <- paste0(dest.dir.save,"/gsm_tile_250_",  voi.depth.sg, ".tif")
  f <- terra::writeRaster(r.gnatsgo.sg, file=dest.name,
                          overwrite=TRUE,
                          wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
                          filetype="GTiff")
  #  GDALinfo(dest.name)
  print(paste("Wrote ", dest.name))
}
# SPCG100USA
if (exists("r.psu.sg")) {
  dest.name <- paste0(dest.dir.save,"/psu_tile_250_",  voi.depth.sg, ".tif")
  f <- terra::writeRaster(r.psu.sg, file=dest.name,
                          overwrite=TRUE, 
                          wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
                          filetype="GTiff")
  #  GDALinfo(dest.name)
  print(paste("Wrote ", dest.name))
}
# POLARIS
if (exists("r.p.sg")) {
  dest.name <- paste0(dest.dir.save,"/polaris_tile_250_",  voi.depth.sg, ".tif")
  f <- terra::writeRaster(r.p.sg, file=dest.name,
            overwrite=TRUE,
            wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
            filetype="GTiff")
  #  GDALinfo(dest.name)
  print(paste("Wrote ", dest.name))
}
# LandGIS
if (exists("r.landgis.sg")) {
  dest.name <- paste0(dest.dir.save,"/landgis_tile_250_",  voi.depth.sg, ".tif")
  f <- terra::writeRaster(r.landgis.sg, file=dest.name,
            overwrite=TRUE, 
            wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
            filetype="GTiff")
  #  GDALinfo(dest.name)
  print(paste("Wrote ", dest.name))
}
# ISSR-800
if (exists("r.issr8.sg")) {
  dest.name <- paste0(dest.dir.save,"/issr8_tile_250_",  voi.depth.sg, ".tif")
  f <- terra::writeRaster(r.issr8.sg, file=dest.name,
            overwrite=TRUE,
            wopt=list(gdal=c("TFW=YES"), datatype="FLT4S"),
            filetype="GTiff")
  #  GDALinfo(dest.name)
  print(paste("Wrote ", dest.name))
}


params <-
list(lrc_long = -120L, lrc_lat = 37L, size = "NA", voi.n = 3L, 
    quantile.n = "NA", depth.n = 3L)

## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, fig.align = 'center', fig.path = './figs/uncert_sg_polaris/')
knitr::opts_chunk$set(cache.extra = R.version.string)


## ----------------------------------------------------------------------------------------------------------------
quantile.list <- c("Q0.05", "Q0.5", "Q0.95")
quantile.list.polaris <- c("p5", "p5", "p95")
quantile.list.gnatsgo <- paste0("_", c("l", "r", "h"))


## ----------------------------------------------------------------------------------------------------------------
voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
voi.list.polaris <- c("clay", "silt", "sand", "ph", "", "om", "bd", "") 
voi.list.gnatsgo <- c("claytotal", "silttotal", "sandtotal",
                  "ph1to1h2o", "cec7", "om",   # note SOM not SOC
                  "dbthirdbar", "sieveno10") # passing 2.0 mm sieve, complement is coarse fragments


## ----------------------------------------------------------------------------------------------------------------
depth.list <- paste0(c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"),"cm")


## ----------------------------------------------------------------------------------------------------------------
library(rgdal)
library(terra)
library(sf)


## ----------------------------------------------------------------------------------------------------------------
base.dir <- "/Volumes/Pythagoras/ds/"
base.dir.sg <- paste0(base.dir, "DSM_export/SoilGrids250")  # extracted tiles
base.dir.polaris <- paste0(base.dir, "DSM_import/POLARIS")  # unprocessed tiles
base.dir.gnatsgo <- paste0(base.dir, "DSM_export/gNATSGO")  # extracted tiles


## ----------------------------------------------------------------------------------------------------------------
print(paste("lrc_long:", params$lrc_long, "; lrc_lat:", params$lrc_lat))
print(paste("voi.n:", params$voi.n, "; depth.n:", params$depth.n))


## ----------------------------------------------------------------------------------------------------------------
voi <- voi.list.sg[params$voi.n]   # variable of interest, SoilGrids name
voi.polaris <- voi.list.polaris[params$voi.n]
#
depth <- depth.list[params$depth.n] # "0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"
depth.polaris <- gsub( "-", "_", strsplit(depth, "cm")[[1]])
#
voi_layer_05 <- paste(voi, depth, "Q0.05", sep="_") # layer of interest, 5% 
voi_layer_50 <- paste(voi, depth, "Q0.5", sep="_") # layer of interest, 50% 
voi_layer_95 <- paste(voi, depth, "Q0.95", sep="_") # layer of interest, 95% 
#
voi.gnatsgo <- voi.list.gnatsgo[params$voi.n]
depth.gnatsgo <- gsub( "-", "", strsplit(depth, "cm")[[1]])


## ----lrc---------------------------------------------------------------------------------------------------------
tile.lrc <- c(params$lrc_long, params$lrc_lat) # lower-right corner
size <- params$size                # tile dimensions


## ----------------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-1, tile.lrc[2]+1) # upper-left corner
m <- matrix(c(tile.ulc[1],tile.lrc[1],  #ulc
              tile.ulc[2], tile.lrc[2]  #lrc
              ),
            nrow=2)
bb.ll <- st_sfc(st_multipoint(m))
st_crs(bb.ll) <- 4326


## ----------------------------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## ----adjust.fig.path---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.path = paste0(knitr::opts_chunk$get("fig.path"), 
                                        AOI.dir.prefix, "/",
                                        voi, "_", depth, "_"))


## ----------------------------------------------------------------------------------------------------------------
(dest.dir.sg <-  file.path(base.dir.sg,
                          AOI.dir.prefix,
                          voi))
## Q0.05
(file.sg <- paste0(dest.dir.sg, "/Q0.05/", depth, "/", voi_layer_05, '_4326.tif'))
if (file.exists(file.sg)) {
  r.sg.05 <- terra::rast(file.sg)
} else {
  print("No SoilGrids250 Q0.05 tile"); stop("Missing input file")
}
## Q0.5
(file.sg <- paste0(dest.dir.sg, "/Q0.5/", depth, "/", voi_layer_50, '_4326.tif'))
if (file.exists(file.sg)) {
  r.sg.50 <- terra::rast(file.sg)
} else {
  print("No SoilGrids250 Q0.5 tile"); stop("Missing input file")
}
## Q0.95
(file.sg <- paste0(dest.dir.sg, "/Q0.95/", depth, "/", voi_layer_95, '_4326.tif'))
if (file.exists(file.sg)) {
  r.sg.95 <- terra::rast(file.sg)
} else {
  print("No SoilGrids250 Q0.95 tile"); stop("Missing input file")
}
(cbind(summary(r.sg.05), summary(r.sg.50), summary(r.sg.95)))


## ----get.tiles.polaris-------------------------------------------------------------------------------------------
(file.name <- paste0(base.dir.polaris, "/",
                     AOI.dir.prefix, "/",
                     voi.polaris, "/p5/",
                     depth.polaris, "/",
                     AOI.dir.prefix, ".tif"))
 if (file.exists(file.name)) {
  r.p.5 <- terra::rast(file.name)
  names(r.p.5) <- "p95"
} else {
  print("No POLARIS p5 tile"); stop("Missing input file")
}
(file.name <- paste0(base.dir.polaris, "/",
                     AOI.dir.prefix, "/",
                     voi.polaris, "/p50/",
                     depth.polaris, "/",
                     AOI.dir.prefix, ".tif"))
 if (file.exists(file.name)) {
  r.p.50 <- terra::rast(file.name)
  names(r.p.50) <- "p50"
} else {
  print("No POLARIS p50 tile"); stop("Missing input file")
}
(file.name <- paste0(base.dir.polaris, "/",
                     AOI.dir.prefix, "/",
                     voi.polaris, "/p95/",
                     depth.polaris, "/",
                     AOI.dir.prefix, ".tif"))
 if (file.exists(file.name)) {
  r.p.95 <- terra::rast(file.name)
  names(r.p.95) <- "p95"
} else {
  print("No POLARIS p95 tile"); stop("Missing input file")
}
(cbind(summary(r.p.5), summary(r.p.50), summary(r.p.95)))


## ----------------------------------------------------------------------------------------------------------------
(aggregation.factor <- res(r.sg.50)/res(r.p.50))
r.p.5 <- terra::aggregate(r.p.5, fact=aggregation.factor, fun="mean")
r.p.50 <- terra::aggregate(r.p.50, fact=aggregation.factor, fun="mean")
r.p.95 <- terra::aggregate(r.p.95, fact=aggregation.factor, fun="mean")


## ----get.tiles.gnatsgo-------------------------------------------------------------------------------------------
# low
file.name <- paste0(base.dir.gnatsgo, "/",
                     AOI.dir.prefix, "/",
                     voi.gnatsgo, "_", 
                     "l_",
                     depth.gnatsgo, ".tif")
if (file.exists(file.name)) {
  r.gn.05 <- terra::rast(file.name)
  names(r.gn.05) <- "l"
} else {
  print("No gNATSGO 'low' tile"); stop("Missing input file")
}

file.name <- paste0(base.dir.gnatsgo, "/",
                     AOI.dir.prefix, "/",
                     voi.gnatsgo, "_", 
                     "r_",
                     depth.gnatsgo, ".tif")
if (file.exists(file.name)) {
  r.gn.50 <- terra::rast(file.name)
  names(r.gn.50) <- "r"
} else {
  print("No gNATSGO 'representative' tile"); stop("Missing input file")
}

file.name <- paste0(base.dir.gnatsgo, "/",
                     AOI.dir.prefix, "/",
                     voi.gnatsgo, "_", 
                     "h_",
                     depth.gnatsgo, ".tif")
if (file.exists(file.name)) {
  r.gn.95 <- terra::rast(file.name)
  names(r.gn.95) <- "h"
} else {
  print("No gNATSGO 'high' tile"); stop("Missing input file")
}

(cbind(summary(r.gn.05), summary(r.gn.50), summary(r.gn.95)))


## ----------------------------------------------------------------------------------------------------------------
r.gn.05 <- terra::project(r.gn.05, r.sg.50)
r.gn.50 <- terra::project(r.gn.50, r.sg.50)
r.gn.95 <- terra::project(r.gn.95, r.sg.50)


## ----trim.sg-----------------------------------------------------------------------------------------------------
r.sg.05 <- terra::resample(r.sg.05, r.p.5, method="near")
r.sg.50 <- terra::resample(r.sg.50, r.p.5, method="near")
r.sg.95 <- terra::resample(r.sg.95, r.p.5, method="near")


## ----trim.gn-----------------------------------------------------------------------------------------------------
r.gn.05 <- terra::resample(r.gn.05, r.p.5, method="near")
r.gn.50 <- terra::resample(r.gn.50, r.p.5, method="near")
r.gn.95 <- terra::resample(r.gn.95, r.p.5, method="near")


## ----mask.polaris------------------------------------------------------------------------------------------------
r.p.05 <- mask(r.p.5, r.sg.05)
r.p.50 <- mask(r.p.50, r.sg.50)
r.p.95 <- mask(r.p.95, r.sg.95)


## ----mask.natsgo-------------------------------------------------------------------------------------------------
r.gn.05 <- mask(r.gn.05, r.sg.05)
r.gn.50 <- mask(r.gn.50, r.sg.50)
r.gn.95 <- mask(r.gn.95, r.sg.95)


## ----show.conversions--------------------------------------------------------------------------------------------
df <- data.frame(property=voi.list.sg, 
                 #"clay"  "silt"  "sand"  "phh2o" "cec"   "soc"   "bdod"  "cfvo" 
                 sg=c("%%","%%","%%","pHx10","mmol(c)/kg","dg/kg","cg/cm3", "cm3/dm3"),  #SG
                 p=c("%","%", "%","pH","", "log10(%)", "g/cm3", ""),  # POLARIS,
                 gnatsgo=c("%","%","%","pH","meq/Cg","1.724 * %","g/cm3", "100-%")  # gNATSGO
)
knitr::kable(
  df, caption = 'Properties and units of measure',
  col.names=c("Property", "SoilGrids250","POLARIS", "gNATSGO"),
  booktabs = TRUE)


## ----make.conversion.matrix--------------------------------------------------------------------------------------
som.to.soc <- 1/1.724138 # this was used in the lab, I know it has been heavily criticized
conversions <- data.frame(property=voi.list.sg, 
                 # sg=c("%%","%%","%%","pHx10","mmol(c)/kg","dg/kg","cg/cm3", "cm3/dm3"), #SG
                 p=c(10,  10, 10,  10, NA, NA, 100, NA), # POLARIS -- SOM is special case
                 gnatsgo=c(10, 10, 10, 10 , 10, 1000*som.to.soc, 100, NA)  # gNATSGO
)
knitr::kable(
  conversions, caption = 'Conversion factors, multiply by these to match SoilGrids250',
  col.names=c("Property","POLARIS", "gNATSGO"),
  booktabs = TRUE)


## ----convert-----------------------------------------------------------------------------------------------------
factors <- conversions[match(voi, conversions$property),]
# POLARIS
fact <- as.numeric(factors[2])
if (!is.na(fact) && (fact != 1)) { 
  r.p.05 <- r.p.05*fact 
  r.p.50 <- r.p.50*fact 
  r.p.95 <- r.p.95*fact 
}
# gNATSGO
fact <- as.numeric(factors[3])
if (!is.na(fact) && (fact != 1)) { 
  r.gn.05 <- r.gn.05*fact 
  r.gn.50 <- r.gn.50*fact 
  r.gn.95 <- r.gn.95*fact 
}


## ----polaris.soc-------------------------------------------------------------------------------------------------
if (voi=="soc") {
    r.p.05 <- ((10^r.p.05)*0.58*1000) 
    r.p.50 <- ((10^r.p.50)*0.58*1000) 
    r.p.95 <- ((10^r.p.95)*0.58*1000) 
}


## ----display.quantiles.sg, fig.width=12, fig.height=4------------------------------------------------------------
zlim = c(floor(min(values(r.sg.05), na.rm=TRUE)),
         ceiling(max(values(r.sg.95), na.rm=TRUE)))
par(mfrow=c(1,3))
terra::plot(r.sg.05, col=rev(hcl.colors(64)),
     main=paste0("Q05, SG2, ",
                voi, ", ", depth), range=zlim)
terra::plot(r.sg.50, col=rev(hcl.colors(64)),
     main=paste0("Q50, SG2, ",
                voi, ", ", depth), range=zlim)
terra::plot(r.sg.95, col=rev(hcl.colors(64)),
     main=paste0("Q95, SG2, ",
                voi, ", ", depth), range=zlim)
par(mfrow=c(1,1))


## ----display.quantiles.polaris, fig.width=12, fig.height=4-------------------------------------------------------
zlim = c(floor(min(values(r.p.05), na.rm=TRUE)),
         ceiling(max(values(r.p.95), na.rm=TRUE)))
par(mfrow=c(1,3))
terra::plot(r.p.05, col=rev(hcl.colors(64)),
     main=paste0("Q05, PSP, ",
                voi, ", ", depth), range=zlim)
terra::plot(r.p.50, col=rev(hcl.colors(64)),
     main=paste0("Q50, PSP, ",
                voi, ", ", depth), range=zlim)
terra::plot(r.p.95, col=rev(hcl.colors(64)),
     main=paste0("Q95, PSP, ",
                voi, ", ", depth), range=zlim)
par(mfrow=c(1,1))


## ----display.quantiles.gnatsgo, fig.width=12, fig.height=4-------------------------------------------------------
zlim = c(floor(min(values(r.gn.05), na.rm=TRUE)),
         ceiling(max(values(r.gn.95), na.rm=TRUE)))
par(mfrow=c(1,3))
terra::plot(r.gn.05, col=rev(hcl.colors(64)),
     main=paste0("Low, gNATSGO, ",
                voi, ", ", depth), range=zlim)
terra::plot(r.gn.50, col=rev(hcl.colors(64)),
     main=paste0("representative, gNATSGO, ",
                voi, ", ", depth), range=zlim)
terra::plot(r.gn.95, col=rev(hcl.colors(64)),
     main=paste0("high, gNATSGO, ",
                voi, ", ", depth), range=zlim)
par(mfrow=c(1,1))


## ----iqr---------------------------------------------------------------------------------------------------------
r.iqr.sg <- (r.sg.95 - r.sg.05)
r.iqr.p <- (r.p.95 - r.p.05)
r.iqr.gn <- (r.gn.95 - r.gn.05)


## ----iqr.summary-------------------------------------------------------------------------------------------------
df <- cbind(summary(r.iqr.sg), summary(r.iqr.p), summary(r.iqr.gn))
colnames(df) <- c("SG2", "PSP", "gNATSGO")
print(df)


## ----iqr.hist, fig.width=12, fig.height=4------------------------------------------------------------------------
zlim = c(floor(min(values(r.iqr.sg), values(r.iqr.p), values(r.iqr.gn), na.rm=TRUE)),
         ceiling(max(values(r.iqr.sg), values(r.iqr.p), values(r.iqr.gn), na.rm=TRUE)))
par(mfrow=c(1,3))
hist(r.iqr.sg, main="SG2", xlim=zlim, xlab="IQR 5/95%")
hist(r.iqr.p, main="PSP", xlim=zlim, xlab="IQR 5/95%")
hist(r.iqr.gn, main="gNATSGO", xlim=zlim, xlab="range low/high estimate")
par(mfrow=c(1,1))


## ----iqr.maps, fig.width=12, fig.height=4------------------------------------------------------------------------
par(mfrow=c(1,3))
terra::plot(r.iqr.sg, col=rev(topo.colors(64)),
     main="SG2 IQR 5/95%", range=zlim)
terra::plot(r.iqr.p, col=rev(topo.colors(64)),
     main="PSP IQR 5/95%", range=zlim)
terra::plot(r.iqr.gn, col=rev(topo.colors(64)),
     main="gNATSGO range low/high", range=zlim)
par(mfrow=c(1,1))


## ----iqr.diff.hist, fig.width=6, fig.height=4--------------------------------------------------------------------
r.iqr.diff <- (r.iqr.sg - r.iqr.p)
summary(r.iqr.diff)
hist(r.iqr.diff, main="IQR difference  5/95%, SG2-PSP")


## ----iqr.diff.map------------------------------------------------------------------------------------------------
terra::plot(r.iqr.diff, col=rev(bpy.colors(64)),
     main="IQR difference  5/95%, SG2-PSP")


## ----range.diff.hist.sg, fig.width=6, fig.height=4---------------------------------------------------------------
r.iqr.diff <- (r.iqr.sg - r.iqr.gn)
summary(r.iqr.diff)
hist(r.iqr.diff, main="IQR/range  5/95% vs. low/high, SG2-gNATSGO")


## ----range.diff.map.sg-------------------------------------------------------------------------------------------
terra::plot(r.iqr.diff, col=rev(bpy.colors(64)),
     main="IQR/range  5/95% vs. low/high, SG2-gNATSGO")


## ----range.diff.hist.p, fig.width=6, fig.height=4----------------------------------------------------------------
r.iqr.diff <- (r.iqr.p - r.iqr.gn)
summary(r.iqr.diff)
hist(r.iqr.diff, main="IQR/range  5/95% vs. low/high, PSP-gNATSGO")


## ----range.diff.map.p--------------------------------------------------------------------------------------------
terra::plot(r.iqr.diff, col=rev(bpy.colors(64)),
     main="IQR/range  5/95% vs. low/high, PSP-gNATSGO")


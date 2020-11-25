## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, fig.align = 'center')
knitr::opts_chunk$set(cache.extra = R.version.string)


## -----------------------------------------------------------------------------------------------------------------------------------------------
base.dir <- "/Users/rossiter/ds/"
base.dir.polaris <- paste0(base.dir, "POLARIS/")
base.dir.import <- "/Volumes/Pythagoras/ds/"
base.dir.polaris.import <- paste0(base.dir.import, "POLARIS/")


## -----------------------------------------------------------------------------------------------------------------------------------------------
library(terra)  # for raster import and display


## -----------------------------------------------------------------------------------------------------------------------------------------------
quantile.list.polaris <- c("p5", "p50", "p95", "mean")


## -----------------------------------------------------------------------------------------------------------------------------------------------
voi.list.polaris <- c("clay", "silt", "sand", "ph", "om", "bd") 


## -----------------------------------------------------------------------------------------------------------------------------------------------
voi.polaris <- "clay"   # variable of interest, POLARIS name
quantile.polaris <- "mean"  # p5, p50, mean, p95


## -----------------------------------------------------------------------------------------------------------------------------------------------
depth.list.polaris <- c("0_5", "5_15", "15_30", "30_60", "60_100", "100_200")


## -----------------------------------------------------------------------------------------------------------------------------------------------
depth.polaris <- "0_5" # "0-5", "5-15", "15-30", "30-60", "60-100", "100-200"
(voi_layer <- paste(voi.polaris, depth.polaris, quantile.polaris, sep="_")) # layer of interest 


## ----lrc----------------------------------------------------------------------------------------------------------------------------------------
tile.lrc <- c(-77, 35) # lower-right corner: central NY


## ----ulc----------------------------------------------------------------------------------------------------------------------------------------
tile.ulc <- c(tile.lrc[1]-1, tile.lrc[2]+1) # upper-left corner


## ----dir.prefix---------------------------------------------------------------------------------------------------------------------------------
AOI.dir.prefix <- paste0("lat", tile.lrc[2], tile.ulc[2],
                         "_lon", tile.ulc[1], tile.lrc[1])


## -----------------------------------------------------------------------------------------------------------------------------------------------
(dest.dir.polaris.import <-  paste0(base.dir.polaris.import, 
                            AOI.dir.prefix, "/",
                            voi.polaris, "/", 
                            quantile.polaris, "/", 
                            depth.polaris))
if (!dir.exists(dest.dir.polaris.import)) {
   dir.create(dest.dir.polaris.import, recursive = TRUE)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------
polaris.tile <- paste0("lat", tile.lrc[2], tile.ulc[2],
                        "_lon", tile.ulc[1], tile.lrc[1],
                        ".tif")
(dest.file <- paste0(dest.dir.polaris.import, "/", polaris.tile))
if (!file.exists(dest.file)) {
   download.file(
      url = 
         paste0("http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/",
                voi.polaris, "/", quantile.polaris, "/", depth.polaris, "/",
                polaris.tile), 
      destfile = dest.file,
      method = "auto")
} else {
   print("Local copy of file already exists")
}


## -----------------------------------------------------------------------------------------------------------------------------------------------
r <- terra::rast(dest.file)
print(r)
plot(r)


library(soilDB)
library(aqp)
library(reshape2)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(mapview)
library(leafsync)
library(viridis)


## explore soil properties associated with "Table Mountain" 

x <- readOGR(dsn = 'geom', layer = 'AOI_0.2_wgs84')

# table mountain sub-AOI
x <- x[1, ]

sand <- ISSR800.wcs(aoi = x, var = 'sand_025cm')
clay <- ISSR800.wcs(aoi = x, var = 'clay_025cm')
ph <- ISSR800.wcs(aoi = x, var = 'ph_025cm')
som <- ISSR800.wcs(aoi = x, var = 'om_kg_sq_m')
cec <- ISSR800.wcs(aoi = x, var = 'cec_025cm')
txt <- ISSR800.wcs(aoi = x, var = 'texture_025cm')

levelplot(txt, margin = FALSE)

m1 <- mapview(sand, na.color = NA)
m2 <- mapview(cec, na.color = NA)
m3 <- mapview(ph, na.color = NA)
m4 <- mapview(som, na.color = NA)

sync(m1, m2, m3, m4)


mu <- mukey.wcs(x, db = 'gssurgo')
levelplot(mu, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)


## until this is part of soilDB or similar pkg
source('https://raw.githubusercontent.com/ncss-tech/soilDB/master/misc/soil-data-aggregation/local-functions.R')

mu.5.15 <- linkComponentHorizonTabular(mu, vars = c('sandtotal_r', 'claytotal_r', 'ph1to1h2o_r'), interval = c(5, 15))

levelplot(mu.5.15[[1]], margin = FALSE, main = 'Sand 5-15cm', scales = list(draw = FALSE), maxpixels = 1e6, col.regions = viridis)

levelplot(mu.5.15[[2]], margin = FALSE, main = 'Clay 5-15cm', scales = list(draw = FALSE), maxpixels = 1e6, col.regions = viridis)

levelplot(mu.5.15[[3]], margin = FALSE, main = 'pH 1:1 H2O 5-15cm', scales = list(draw = FALSE), maxpixels = 1e6, col.regions = viridis)



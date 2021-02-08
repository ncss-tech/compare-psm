## 2020-12-11
## D.E. Beaudette
## AOI context maps, using soil color at 25cm as background

options(stringsAsFactors = FALSE)

library(rasterVis)
library(sp)
library(sf)
library(spData) 
library(rgdal) 
library(ragg) 

# gNATSGO CRS
gNATSGO.crs <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

# state outlines are good context
data("us_states")
us_states <- as(us_states, 'Spatial')
us_states <- spTransform(us_states, CRS(gNATSGO.crs))


# AOIs in AEA / gNATSGO CRS
aoi <- readOGR(dsn = 'geom', layer = 'AOI_1_aea')
sub_aoi <- readOGR(dsn = 'geom', layer = 'AOI_0.2_aea')

## TODO: put this online
# soil color background image
# this is a 4-band RGBA image
soilcolor <- brick('E:/gis_data/SSURGO-color/CONUS/CONUS_025cm.tif')

## base graphics for context map

agg_png(file = 'figures/context-map.png', width = 1200, height = 740, res = 90, scaling = 1.5)

plotRGB(
  soilcolor,
  # smoother figure
  # interpolate = TRUE,
  # increase for final version
  maxpixels = 1e6,
  scale = 255,
  colNA = 'black'
  # use ext argument to modify extent
  
)

plot(us_states, border = 'white', lwd = 1, add = TRUE)
plot(aoi, border = 'green', lwd = 2, add = TRUE, lend = 1)
text(aoi, labels = aoi$id, font = 2, col = 'green', cex = 1)

dev.off()

## zoomed context maps

# i: single AOI SPDF
# b: buffer in meters
plotZoom <- function(i, b = 50000) {
  zoom.ext <- extent(
    buffer(
      i,
      width = 50000
    )
  )
  
  plotRGB(
    soilcolor,
    scale = 255,
    # use ext argument to modify extent
    ext = zoom.ext,
    colNA = 'black',
    # maxpixels = 10000
  )
  
  # state boundaries
  plot(us_states, border = 'white', lwd = 1, add = TRUE)
  
  # current AOI
  plot(i, border = 'green', lwd = 2, add = TRUE, lend = 1)
  
  # any sub-AOIs within the current AOI
  ovr.res <- over(i, sub_aoi)
  ovr.res <- na.omit(ovr.res)
  if(nrow(ovr.res) > 0) {
    
    plot(sub_aoi[sub_aoi$id == ovr.res$id, ], border = 'green', lwd = 1, add = TRUE, lend = 1)
  }
  
  
  # label AOI
  mtext(i[['name']], side = 1, line = -2, col = 'white', font = 2, cex = 1.5)
  
}


for(i in aoi$id){
  f <- sprintf('figures/aoi-%s.png', i)
  
  agg_png(file = f, width = 800, height = 900, res = 90, scaling = 1.5)
  
  plotZoom(aoi[which(aoi$id == i), ])
  
  dev.off()
}





## interesting idea, but runs out of memory
# https://stackoverflow.com/questions/16093802/how-to-get-rgb-raster-image-with-utm-coordinates
# 
# # copy in which we will store colors
# soilcolor.r <- raster(soilcolor)
# 
# # create a color for each cell
# # this is slow
# cols <- factor(rgb(soilcolor[], maxColorValue=255))
# 
# # store colors as factor levels
# # essentially a color LUT
# soilcolor.r[] <- cols

# 
# 
# ## this works for rasterVis::levelplot
# # expand BBOX around the 
# b <- bbox(us_states)
# x.lim <- c(b[1, 1] - 1e5, b[1, 2] + 1e5)
# y.lim <- c(b[2, 1] - 1e5, b[2, 2] + 1e5)
# 


# pp <- levelplot(soilcolor.r, maxpixels = ncell(soilcolor) + 1,
#                 margin = FALSE, xlim = x.lim, ylim = y.lim,
#                 scales = list(draw=FALSE),
#                 col.regions = as.character(levels(cols))
#                 panel=function(...) {
#                   panel.levelplot(...)
#                   sp.polygons(us_states, col='white', lwd=1)
#                   sp.polygons(aoi, col='black', lwd=1)
#                 }
# )
# 




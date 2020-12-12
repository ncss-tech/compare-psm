## 2020-12-11
## D.E. Beaudette
## AOI / tile definitions and BBOX geometry, stored in AOI.csv

## contents of AOI.csv borrowed from DGR's code:

# tile.lrc <- c(-122, 46) # lower-right corner: Centralia WA
# tile.lrc <- c(-76, 41) # lower-right corner: central NY
# tile.lrc <- c(-76, 35)   # lower-right corner: coastal plain NC
# tile.lrc <- c(119, 32)   # lower-right corner: Nanjing area
# tile.lrc <- c(-76, 42)   # lower-right corner: northern tier PA, southern tier NY
# tile.lrc <- c(-91, 36)   # lower-right corner: MO/AR central border

options(stringsAsFactors = FALSE)

library(sp)
library(raster)
library(rgdal)

## init tiles from lower-right corners
bb <- read.csv('AOI.csv')

# compute ulc
bb$ulc.long <- bb$lrc.long - 1
bb$ulc.lat <- bb$lrc.lat + 1

# make a SPDF from LR/UL corners
makePoly <- function(i) {
  # extent object
  # xmin, xmax, ymin, ymax
  e <- extent(
    c(i$ulc.long, i$lrc.long, i$lrc.lat, i$ulc.lat)
  )
  
  # init SP object and set CRS
  p <- as(e, 'SpatialPolygons')
  proj4string(p) <- '+proj=longlat +datum=WGS84'
  
  # promote to SPDF and add ID
  d <- data.frame(id = i[['id']], name = i[['name']])
  p <- SpatialPolygonsDataFrame(p, data = d)
  
  return(p)
}

# convert LR/UL corners -> SpatialPolygonsDataFrame
bb.sp <- lapply(
  split(bb, bb$id),
  makePoly
)

# list -> single SPDF
bb.sp <- do.call('rbind', bb.sp)

# check: OK
plot(bb.sp)


# gNATSGO CRS
gNATSGO.crs <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

# project to gNATSGO CRS
bb.sp.aea <- spTransform(bb.sp, CRS(gNATSGO.crs))

# check: OK
plot(bb.sp.aea)

# buffer by 10km for extraction of gNATSGO
bb.sp.aea.buff <- buffer(bb.sp.aea, width = 10000, dissolve = FALSE)

# check: ok
plot(bb.sp.aea)
plot(bb.sp.aea.buff, add=TRUE, border = 'red')

# save
writeOGR(bb.sp, dsn = 'geom', layer = 'AOI_wgs84', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(bb.sp.aea, dsn = 'geom', layer = 'AOI_aea', driver = 'ESRI Shapefile', overwrite_layer = TRUE)
writeOGR(bb.sp.aea.buff, dsn = 'geom', layer = 'AOI_aea_10km_buffer', driver = 'ESRI Shapefile', overwrite_layer = TRUE)



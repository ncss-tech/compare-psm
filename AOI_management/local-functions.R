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


# make AOI from specified LRC and offset
makeAOI <- function(bb, o = 1) {

  # compute ulc
  bb$ulc.long <- bb$lrc.long - o
  bb$ulc.lat <- bb$lrc.lat + o
  
  
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
  
  # file names
  f.bb <- sprintf("AOI_%s_wgs84", o)
  f.aea.bb <- sprintf("AOI_%s_aea", o)
  f.aea.buff.bb <- sprintf("AOI_%s_aea_10km_buffer", o)
  
  # save
  writeOGR(bb.sp, dsn = 'geom', layer = f.bb, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  writeOGR(bb.sp.aea, dsn = 'geom', layer = f.aea.bb, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  writeOGR(bb.sp.aea.buff, dsn = 'geom', layer = f.aea.buff.bb, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
    
}




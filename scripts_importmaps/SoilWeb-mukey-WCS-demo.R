

## DEB: this has all been replaced by recent additions to soilDB
## https://github.com/ncss-tech/soilDB/blob/master/misc/WCS-demo.R




##
## notes
##

# https://www.mapserver.org/ogc/wcs_server.html?highlight=web%20coverage%20service#configuring-your-mapfile-to-serve-wcs-layers

# https://mapserver.org/mapfile/projection.html


# https://cran.r-project.org/web/packages/slga/vignettes/slga.html

# construct URLs and compute image dimensions:
# https://github.com/obrl-soil/slga/blob/master/R/url_generate.R
# https://github.com/obrl-soil/slga/tree/master/R




library(raster)
library(rasterVis)


# aoi.wgs84: BBOX in WGS84 GCS ~ c(-121,37,-120,38)
# res: grid resolution in native CRS (meters) [ISSR-800: 800, gNATSGO: 30]
prepareAOI <- function(aoi.wgs84, res) {
  # manage extent / raster / projection with some temp objects
  e <- extent(aoi.wgs84)
  r <- raster(e)
  projection(r) <- CRS('+proj=longlat +datum=WGS84')
  
  # ISSR-800 and gNATSGO CRS
  crs <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'
  
  # double-check, likely a better approach
  r <- suppressWarnings(projectExtent(r, crs))
  
  # AOI and image calculations in native CRS
  aoi.native <- extent(r)
  
  # best practices for rounding?
  w <- round(abs(aoi.native[3] - aoi.native[1]) / res)
  h <- round(abs(aoi.native[4] - aoi.native[2]) / res)
  
  return(
    list(
      aoi = aoi.native,
      width = w,
      height = h
    )
  )
}


# ISSR-800 native CRS
prepareAOI(c(-121,37,-120,38), res = 800)

# gNATSGO native CRS
# that is a huge file
prepareAOI(c(-121,37,-120,38), res = 30)

## TODO: after I define a new EPSG code the WCS will 
#        be able to skip the server-side raster warping
#        and resampling


## TODO: automatic linking of raster attribute tables

## tempoary interface
# raster warping / resampling done server-side for now (not ideal!)
# var: raster data source name
# aoi: BBOX in WGS84 GCS ~ c(-121,37,-120,38)
# fmt: datatype
# res: resolution in GCS (... yes I know this is dumb)
WCS.demo <- function(var, aoi, fmt, res = 0.002) {
  
  ## TODO: make sure this is correct in general
  ## TODO: think about resampling issues
  ## TODO: data should be stored so that resampling / warping is not neccessary
  # compute image dimensions 
  w <- round(abs(aoi[3] - aoi[1]) / res)
  h <- round(abs(aoi[4] - aoi[2]) / res)
  
  ## possible formats: 
  # GEOTIFF_BYTE (8bit unsigned integers)
  # GEOTIFF_16 (16bit signed integers)
  # GEOTIFF_FLOAT (32bit floating point)
  
  # base URL + parameters
  # double-check version spec 1.0.0?
  u <- sprintf(
    'https://soilmap2-1.lawr.ucdavis.edu/cgi-bin/mapserv?map=/soilmap2/website/wcs/mukey-WCS.map&SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&CRS=EPSG:4326&coverage=%s&FORMAT=%s&BBOX=%s&WIDTH=%s&HEIGHT=%s',
    var, fmt, paste(aoi, collapse = ','), w, h
  )
  
  
  # make space for the resulting GeoTiff
  tf <- tempfile()
  download.file(u, destfile = tf, mode = 'wb')
  
  # load and return
  res <- raster(tf)
  
  return(res)
}


## try it out with a couple of demo raster data sources


# pH 0-5cm ISSR-800
pH_05cm <- WCS.demo(var = 'ph_05cm', fmt = 'GEOTIFF_FLOAT', aoi = c(-121,37,-120,38), res = 0.002)

# survey type ISSR-800
survey.type <- WCS.demo(var = 'survey_type', fmt = 'GEOTIFF_BYTE', aoi = c(-121,37,-120,38), res = 0.01)

# WEG ISSR-800
weg <- WCS.demo(var = 'weg', fmt = 'GEOTIFF_BYTE', aoi = c(-121,37,-120,38), res = 0.01)

## I made a small subset 
# map unit keys
# file is downloaded as floating point values, must be converted to integers
gNATSGO_mukey <- WCS.demo(var = 'gnatsgo', fmt = 'GEOTIFF_FLOAT', aoi = c(-121,37,-120,38), res = 0.002)

## TODO: this should be specified in the WCS (?) configuration
## may have to be manually (data source specific) set
# specification of NODATA
NAvalue(pH_05cm) <- 0
NAvalue(survey.type) <- 0
NAvalue(weg) <- 0
NAvalue(gNATSGO_mukey) <- 0

# read into memory
pH_05cm <- readAll(pH_05cm)
survey.type <- readAll(survey.type)
weg <- readAll(weg)
gNATSGO_mukey <- readAll(gNATSGO_mukey)

# convert categorical data -> integer-keyed values (RAT)
survey.type <- ratify(survey.type)
weg <- ratify(weg)
gNATSGO_mukey <- ratify(gNATSGO_mukey)

# looks good
levelplot(pH_05cm, margin = FALSE)

# looks good
levelplot(weg, att = 'ID', margin = FALSE)

# integer map unit keys, colors don't mean anything
levelplot(gNATSGO_mukey, att = 'ID', margin = FALSE, colorkey = FALSE)

# all SSURGO
# "holes" are small water features which have been back-filled with STATSGO, not ideal
levelplot(survey.type, att = 'ID', margin = FALSE, colorkey = FALSE)

# these are SSURGO / STATSGO map unit keys
ll <- levels(gNATSGO_mukey)[[1]]
head(ll, 10)


# use SDA + SSURGO aggregation engine (SQL mostly) to aggregate data
# link aggregated data to RAT
# convert to grid of values
# done!
library(aqp)
library(soilDB)
library(reshape2)

IS <- format_SQL_in_statement(ll$ID)
ws <- sprintf("mukey IN %s", IS)
x <- fetchSDA(WHERE = ws, duplicates = TRUE, droplevels = TRUE, stringsAsFactors = FALSE)

## TODO: this is still a very (dumb) approach to aggregating the data

## TODO: add more variables
# component level aggregation
a <- slab(x, cokey ~ ph1to1h2o_r, slab.structure = c(0, 5), slab.fun = mean, na.rm = TRUE)
# long -> wide format
w <- dcast(a, cokey ~ variable, value.var = 'value')

# MU-level aggregation / subset
s <- site(x)[, c('mukey', 'cokey', 'comppct_r')]
s <- merge(s, w, by = 'cokey', sort = FALSE)


# STATSGO map unit
# l <- split(s, s$mukey)
# i <- i <- l[['660849']]

## TODO: generalize
# component percentage weighted mean
agg.data <- lapply(
  split(s, s$mukey), function(i, var = 'ph1to1h2o_r') {
    # remove NA first
    i <- na.omit(i)
    # weighted mean
    wm <- sum(i[[var]]* i$comppct_r) / sum(i$comppct_r)
    
    res <- data.frame(
      mukey = i$mukey[1],
      var = wm,
      stringsAsFactors = FALSE
    )
    
    names(res)[2] <- var
    
    return(res)
  })

# list -> DF
agg.data <- do.call('rbind', agg.data)

# join into RAT
rat <- merge(ll, agg.data, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# re-pack RAT
levels(gNATSGO_mukey) <- rat

# convert to raster of values via RAT
gNATSGO.pH_05cm <- deratify(gNATSGO_mukey, att = 'ph1to1h2o_r')

# not too bad
levelplot(gNATSGO.pH_05cm,margin = FALSE, main = 'gNATSGO 1:1 H2O pH 0-5cm')



##
## Dylan's notes for later
##

# 
# library(raster)
# 
# x <- raster('E:/gis_data/MapunitRaster_30m.tif')
# x
# 
# # INT4U should suffice
# dataType(x)
# 
# # ~ 9Mb
# file.size('E:/gis_data/MapunitRaster_30m.tif') / 1024 / 1024
# 
# projectExtent(x, '+proj=longlat +datum=NAD83')
# 
# # BBOX=minx,miny,maxx,maxy: Bounding box corners (lower left, upper right)
# 
# 
# 

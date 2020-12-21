library(aqp)
library(soilDB)
library(reshape2)

library(sp)
library(raster)
library(rasterVis)
library(viridis)


# note that grid resolution isn't quite right

# BBOX in WGS84 coordinates
a <- c(-121, 37, -120, 38)

# attempt in AEA ~ 800m

# floating point grids
pH_05cm <- ISSR800.wcs(var = 'ph_05cm', aoi = a)
pH_3060cm <- ISSR800.wcs(var = 'ph_3060cm', aoi = a)

clay_05cm <- ISSR800.wcs(var = 'clay_05cm', aoi = a)
clay_3060cm <- ISSR800.wcs(var = 'clay_3060cm', aoi = a)

silt_3060cm <- ISSR800.wcs(var = 'silt_3060cm', aoi = a)
sand_3060cm <- ISSR800.wcs(var = 'sand_3060cm', aoi = a)

# compositional data may lose some fidelity due to marginal aggregation
z <- sand_3060cm + silt_3060cm + clay_3060cm
levelplot(z - 100)

# 16bit integer grids
wei <- ISSR800.wcs(var = 'wei', aoi = a)

# 8bit unsigned (BYTE) grids with RAT
drainage_class <- ISSR800.wcs(var = 'drainage_class', aoi = a)
weg <- ISSR800.wcs(var = 'weg', aoi = a)
str <- ISSR800.wcs(var = 'str', aoi = a)


# attempt in GCS ~ 600m
pH_05cm.gcs <- ISSR800.wcs(var = 'ph_05cm', aoi = a, res = 0.004, crs = 'EPSG:4326')
drainage_class.gcs <- ISSR800.wcs(var = 'drainage_class', aoi = a, res = 0.004, crs = 'EPSG:4326')
weg.gcs <- ISSR800.wcs(var = 'weg', aoi = a, res = 0.004, crs = 'EPSG:4326')



# AEA
levelplot(stack(pH_05cm, pH_3060cm), margin = FALSE)

levelplot(stack(clay_05cm, clay_3060cm), margin = FALSE)

levelplot(stack(sand_3060cm, silt_3060cm, clay_3060cm), margin = FALSE)

levelplot(wei, margin = FALSE)

levelplot(drainage_class, margin = FALSE)
levelplot(weg, margin = FALSE)
levelplot(str, margin = FALSE)

# GCS
levelplot(pH_05cm.gcs, margin = FALSE)
levelplot(drainage_class.gcs, margin = FALSE)
levelplot(weg.gcs, margin = FALSE)


##
## gNATSGO
##

# note: this is not real aggregation
# resampled to ~300m (643kb)
gn.300m <- mukey.wcs(var = 'gnatsgo', aoi = a, res = 300)

# native ~ 30m (64Mb)
gn.30m <- mukey.wcs(var = 'gnatsgo', aoi = a, res = 30)

# AEA
levelplot(gn.300m, att = 'ID', margin = FALSE, colorkey = FALSE)


## overly-simplistic aggregation of tabular data from SDA which must be generalized / abstracted
## this will eventually be available as a macro / stored procedure in SDA

# get unique mukey
ll <- levels(gn.300m)[[1]]
IS <- format_SQL_in_statement(ll$ID)

# query SDA by mukey
# this will bring down most of the interesting site / horizon level attributes from SSURGO/STATSGO
ws <- sprintf("mukey IN %s", IS)
x <- fetchSDA(WHERE = ws, duplicates = TRUE, droplevels = TRUE, stringsAsFactors = FALSE)


## TODO: account for NAs: source data through aggregation steps

# component level aggregation for variables and depth intervals of interest
# note that we get an "extra" depth interval of 5-30
x.a <- slab(x, cokey ~ ph1to1h2o_r + claytotal_r, slab.structure = c(0, 5, 30, 60), slab.fun = mean, na.rm = TRUE)

# remove 5-30cm interval
x.a <- x.a[which(x.a$top != 5 & x.a$bottom != 30), ]

# make an ID for reshaping
x.a$variable.id <- sprintf("%s%s%s", x.a$variable, x.a$top, x.a$bottom)

# long -> wide format
w <- dcast(x.a, cokey ~ variable.id, value.var = 'value')

# check: ok
head(w)

# MU-level aggregation / subset
s <- site(x)[, c('mukey', 'cokey', 'comppct_r')]
s <- merge(s, w, by = 'cokey', sort = FALSE)

# STATSGO map unit for testing
# l <- split(s, s$mukey)
# i <- i <- l[['660849']]

## TODO: generalize
wt.mean.component <- function(i, var, wt = 'comppct_r') {
  
  # remove NA in target variable
  idx <- which(is.na(i[[var]]) | is.na(i[[wt]]))
  if(length(idx) > 0) {
    i <- i[-idx, ] 
  }
  
  # weighted mean
  wm <- sum(i[[var]] * i[[wt]]) / sum(i[[wt]])
  
  # pack results
  res <- data.frame(
    mukey = i$mukey[1],
    var = wm,
    stringsAsFactors = FALSE
  )
  
  # re-name for convenience later
  names(res)[2] <- var
  
  return(res)
}


# component percentage weighted mean
ss <- split(s, s$mukey)

## TODO: generalize this
clay05 <- lapply(ss, wt.mean.component, var = 'claytotal_r05')
clay05 <- do.call('rbind', clay05)

clay3060 <- lapply(ss, wt.mean.component, var = 'claytotal_r3060')
clay3060 <- do.call('rbind', clay3060)

ph05 <- lapply(ss, wt.mean.component, var = 'ph1to1h2o_r05')
ph05 <- do.call('rbind', ph05)

ph3060 <- lapply(ss, wt.mean.component, var = 'ph1to1h2o_r3060')
ph3060 <- do.call('rbind', ph3060)

# merge all aggregate data into RAT
rat <- merge(ll, clay05, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)
rat <- merge(rat, clay3060, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)
rat <- merge(rat, ph05, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)
rat <- merge(rat, ph3060, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# NA present?
# if so, does it matter?

# preservation of mukey?
nrow(ll) == nrow(rat)

# re-pack RAT
levels(gn.300m) <- rat

# convert to raster of values via RAT
gn.300m.pH_05cm <- deratify(gn.300m, att = 'ph1to1h2o_r05')
gn.300m.pH_3060cm <- deratify(gn.300m, att = 'ph1to1h2o_r3060')
gn.300m.clay_05cm <- deratify(gn.300m, att = 'claytotal_r05')
gn.300m.clay_3060cm <- deratify(gn.300m, att = 'claytotal_r3060')

# hey, it worked!
levelplot(gn.300m.pH_05cm, margin = FALSE, main = 'gNATSGO 1:1 H2O pH 0-5cm')
levelplot(gn.300m.pH_3060cm, margin = FALSE, main = 'gNATSGO 1:1 H2O pH 30-60cm')

levelplot(gn.300m.clay_05cm, margin = FALSE, main = 'gNATSGO % clay 0-5cm')
levelplot(gn.300m.clay_3060cm, margin = FALSE, main = 'gNATSGO % clay 30-60cm')


## this has to be done with identical grid topology
# compare
pH_05cm <- ISSR800.wcs(var = 'ph_05cm', aoi = a, res = 300)

rs <- stack(gn.300m.pH_05cm, pH_05cm)

pp <- levelplot(rs, margin = FALSE, main = '1:1 H2O pH 0-5cm', scales = list(draw = FALSE), maxpixels = 1e6, col.regions = viridis)

row.names(pp) <- c('SSURGO/STATSGO (gNATSGO at 300m)', 'SSURGO/STATSGO (ISSR-800 at 800m)')

pp

## example with muaggatt table

## example with value1 table





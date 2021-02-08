## 2020-12-11
## D.E. Beaudette
## AOI / tile definitions and BBOX geometry, stored in AOI.csv

## contents of AOI.csv borrowed from DGR's code:

# tile.lrc <- c(-122, 46)  # lower-right corner: Centralia WA
# tile.lrc <- c(-76, 41)   # lower-right corner: central NY
# tile.lrc <- c(-76, 35)   # lower-right corner: coastal plain NC
# tile.lrc <- c(119, 32)   # lower-right corner: Nanjing area
# tile.lrc <- c(-76, 42)   # lower-right corner: northern tier PA, southern tier NY
# tile.lrc <- c(-91, 36)   # lower-right corner: MO/AR central border

options(stringsAsFactors = FALSE)

library(sp)
library(raster)
library(rgdal)

source('local-functions.R')


## AOI are created from lower-right corners

## save 3 versions to SHP

# 1 x 1 degree AOIs
bb <- read.csv('AOI.csv')
makeAOI(bb, o = 1)

# 0.2 x 0.2 degree AOIs
bb <- read.csv('AOI-detail-0.2deg.csv')
makeAOI(bb, o = 0.2)


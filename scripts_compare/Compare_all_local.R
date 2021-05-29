# Function to compare a set of maps with parameters, 
#  by rendering their respective R Markdown scripts
# Arguments:
# lrc_long: -76
# lrc_lat: 42 
# size: 1
# voi.n: 4
# depth.n: 4
# quantile.n: 4 
# test.tile.size: 0.15  # degrees
# test.tile.x.offset: 0.6  # west from right edge
# test.tile.y.offset: 0.2  # north from bottom edge
## see lists of VOI, depth, quantile in `Compare_local.Rmd`

## the products to compare are hard-coded in `Compare_local.Rmd`
##  and `Compare_local_patterns.Rmd`

compare.one <- function(lrc_long, lrc_lat, voi.n, depth.n, test.tile.size, test.tile.x.offset, test.tile.y.offset) {
  library(knitr)
  param.list.patterns <- list(lrc_long=lrc_long,
                     lrc_lat=lrc_lat,
                     size=1,
                     voi.n=voi.n,
                     depth.n=depth.n,
                     quantile.n=4) 
  # only compare means
  param.list <- c(param.list.patterns,  
                     test.tile.size=test.tile.size,
                     test.tile.x.offset=test.tile.x.offset,
                     test.tile.y.offset=test.tile.y.offset)
  
  out.file.name <- paste0("CompareLocal", lrc_long, '_', lrc_lat, '_', 
                          voi.n, '_', 
                          depth.n, '.html')
  
  rmarkdown::render("Compare_local.Rmd",
                    params = param.list,
                    output_file = out.file.name,
                    envir = new.env(parent = globalenv())
  )
  
  out.file.name <- paste0("CompareLocalPattern_", lrc_long, '_', lrc_lat, '_', 
                          voi.n, '_', 
                          depth.n, '.html')
  
  rmarkdown::render("Compare_local_patterns.Rmd",
                    params = param.list.patterns,
                    output_file = out.file.name,
                    envir = new.env(parent = globalenv())
  )
  
}

## ad-hoc calls to this
# depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
# voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")

# North Carolina coastal plain clay, 0-5, 30-60
# for (d.n in c(1,4)) {
#   compare.one(lrc_long=-77, lrc_lat=35, 
#               voi.n=1,  
#               depth.n=d.n, 
#               test.tile.size=0.15,
#               test.tile.x.offset=0.34, # Old Sparta
#               test.tile.y.offset=0.70
#   )
# }

# Central NY State pH, 0-5, 30-60
# for (d.n in c(1,4)) {
#   compare.one(lrc_long=-76, lrc_lat=42, 
#               voi.n=4,  
#               depth.n=d.n, 
#               test.tile.size=0.15,
#               test.tile.x.offset=0.3,  # Cayuta
#               test.tile.y.offset=0.45
#   )
# }

# 1 x 1 degree: lower right corner of Tile lat3839_Lon-8786 (POLARIS tile)
# 0.15 x 0.15 degree :lower right corner lat3844_lon-8661

# SW Indiana SOC, BD; 0-5, 5-15
# for (voi in c(6, 7)) {
#   for (d.n in c(1,2)) {
#     compare.one(lrc_long=-86, lrc_lat=38, 
#                 voi.n=voi,  
#                 depth.n=d.n, 
#                 test.tile.size=0.15,
#                 test.tile.x.offset=0.61, # SW of French Lick
#                 test.tile.y.offset=0.44
#     )
#   }  
# }

# California sand 5-15, 15-30
for (d.n in c(2, 3)) {
  compare.one(lrc_long=-120, lrc_lat=37,
              voi.n=3,
              depth.n=d.n,
              test.tile.size=0.15,
              test.tile.x.offset=0.54,
              test.tile.y.offset=0.77
  )
}

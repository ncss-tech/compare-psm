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
for (d.n in c(1,4)) {
  compare.one(lrc_long=-76, lrc_lat=42, 
              voi.n=4,  # voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
              depth.n=d.n, 
              test.tile.size=0.15,
              test.tile.x.offset=0.3,
              test.tile.y.offset=0.45
  )
}

# Function to compare a set of maps with parameters, 
#  by rendering their respective R Markdown scripts
# Arguments:
# lrc_long: -76
# lrc_lat: 42 
# size: 1
# voi.n: 4
# depth.n: 4
# quantile.n: 4 

## see lists of VOI, depth, quantile in `Compare_regional.Rmd`

## the list of products to compare must be set within `Compare_regional.Rmd`
##  and `Compare_regional_patterns.Rmd`
## also the subtile limits for patterns

compare.one <- function(lrc_long, lrc_lat, voi.n, depth.n, test.tile.size, test.tile.x.offset, test.tile.y.offset) {
  library(knitr)
  param.list <- list(lrc_long=lrc_long,
                     lrc_lat=lrc_lat,
                     size=1,
                     voi.n=voi.n,
                     depth.n=depth.n,
                     quantile.n=4)  # only compare means
  
  param.list.patterns <- c(param.list, # for the pattern analysis
                           test.tile.size=test.tile.size,
                           test.tile.x.offset=test.tile.x.offset,
                           test.tile.y.offset=test.tile.y.offset)
  
  out.file.name <- paste0("Compare_", lrc_long, '_', lrc_lat, '_', 
                          voi.n, '_', 
                          depth.n, '.html')
  
  rmarkdown::render("Compare_regional.Rmd",
                    params = param.list,
                    output_file = out.file.name,
                    envir = new.env(parent = globalenv())
  )
  
  out.file.name <- paste0("ComparePattern_", lrc_long, '_', lrc_lat, '_', 
                          voi.n, '_', 
                          depth.n, '.html')
  
  rmarkdown::render("Compare_regional_patterns.Rmd",
                    params = param.list.patterns,
                    output_file = out.file.name,
                    envir = new.env(parent = globalenv())
  )
  
}

## ad-hoc calls to this
# depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
for (d.n in c(1,2)) {
  compare.one(lrc_long=-86, lrc_lat=38, 
              voi.n=7,  # voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
              depth.n=d.n, 
              test.tile.size=0.2,
              test.tile.x.offset=0.58,
              test.tile.y.offset=0.37
  )
}

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

compare.one <- function(lrc_long, lrc_lat, voi.n, depth.n) {
  library(knitr)
  param.list <- list(lrc_long=lrc_long,
                     lrc_lat=lrc_lat,
                     size=1,
                     voi.n=voi.n,
                     depth.n=depth.n,
                     quantile.n=4)  # only compare means
  
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
                    params = param.list,
                    output_file = out.file.name,
                    envir = new.env(parent = globalenv())
  )
  
}

## ad-hoc calls to this
for (d.n in c(1,4)) {
  compare.one(lrc_long=-77, lrc_lat=35, voi.n=1, depth.n=d.n)
}

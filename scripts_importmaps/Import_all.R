# Function to import all with parameters, 
#  by rendering their respective R Markdown scripts
# Arguments:
# lrc_long: -76
# lrc_lat: 42 
# size: 1
# voi.n: 4
# depth.n: 4
# quantile.n: 4 

## see lists of VOI, depth, quantile in `../ComparePSMproductsUSA.Rmd`

import.all <- function(lrc_long, lrc_lat, size=1, voi.n, depth.n=1, quantile.n=4, which=1:6) {
  library(knitr)
  psm.list <- c(
    "GSM_USA_V05",
    "SoilGrids250",
    "POLARIS",
    "SPCG100USA",
    "LandGIS",
    "ISSR8")
  for (psm in psm.list[which]) {
    print(paste("Importing", psm))
    param.list <- list(lrc_long=lrc_long,
                       lrc_lat=lrc_lat,
                       size=size,
                       voi.n=voi.n,
                       depth.n=depth.n,
                       quantile.n=quantile.n)
    
    out.file.name <- paste0(lrc_long, '_', lrc_lat, '_', 
                             voi.n, '_', quantile.n, '_', 
                             depth.n, '.html')
    
    rmarkdown::render(paste0(psm, "_import.Rmd"),
                      params = param.list,
                      output_file = out.file.name,
                      envir = new.env(parent = globalenv())
                      )
  }
}

import.all(lrc_long=-120, lrc_lat=38, size=1, voi.n=2, depth.n=1, quantile.n=4, which=c(3))

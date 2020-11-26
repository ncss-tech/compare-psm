# Function to import all with parameters, 
#  by rendering their respective R Markdown scripts
# Arguments:
## aoi
## voi
## depth

import.all <- function(aoi, voi, depth, size, quantile, which=1:6) {
  library(knitr)
  psm.list <- c(
    "GSM_USA_V05",
    "SoilGrids250",
    "POLARIS",
    "SPCG100USA",
    "LandGIS",
    "ISSR8")
  for (psm in psm.list) {
    rmarkdown::render(psm,
                      params = list(aoi=aoi,
                                    voi=voi,
                                    depth=depth,
                                    size=size,
                                    quantile=quantile),
                      output_file = paste0(aoi, '_', voi, '_', depth, '.html'),
                      envir = parent.frame()
                      )
  }
}


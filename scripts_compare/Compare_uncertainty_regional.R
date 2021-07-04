# Function to compare the uncertainty from SoilGrids250 and POLARIS and gNATSGO
#  by rendering their respective R Markdown scripts
# Arguments:
# lrc_long: -76
# lrc_lat: 42 
# size: 1
# voi.n: 4
# depth.n: 4
# quantile.n: NA 
### n.b. quantiles must have been imported

## see lists of VOI, depth in `Compare_regional.Rmd`

compare.one <- function(lrc_long, lrc_lat, voi.n, depth.n) {
  library(knitr)
  param.list <- list(lrc_long=lrc_long,
                     lrc_lat=lrc_lat,
                     size=NA,
                     voi.n=voi.n,
                     quantile.n=NA,
                     depth.n=depth.n)  # only compare means
  
  out.file.name <- paste0("Compare_uncertainty", lrc_long, '_', lrc_lat, '_', 
                          voi.n, '_', 
                          depth.n, '.html')
  
  rmarkdown::render("Compare_regional_uncertainty.Rmd",
                    params = param.list,
                    output_file = out.file.name,
                    envir = new.env(parent = globalenv()))

}

# voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
# depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")

## CNY pH 0-5, 30-60
# for (v in c(4)) {
#   # depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
#   for (d in c(1,4)) {
#     compare.one(lrc_long=-76, lrc_lat=42, voi.n=v, depth.n=d)
#   }
# }

# NC clay
for (v in c(1)) {
  for (d in c(1,4)) {
    compare.one(lrc_long=-77, lrc_lat=35, voi.n=v, depth.n=d)
  }
}

# Indiana soc
for (v in c(6)) {
  for (d in c(1,2)) {
    compare.one(lrc_long=-86, lrc_lat=38, voi.n=v, depth.n=d)
  }
}

## California sand 5-15, 15-30
for (v in c(3)) {
  for (d in c(2,3)) {
    compare.one(lrc_long=-120, lrc_lat=37, voi.n=v, depth.n=d)
  }
}


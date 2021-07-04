# Function to import all with parameters, 
#  by rendering their respective R Markdown scripts
# Arguments:
# lrc_long: -76
# lrc_lat: 42 
# size: 1
# voi.n: 4
# depth.n: 4
# quantile.n: 4 

## see lists of VOI, depth, in `../scripts_compare/Compare_regional.Rmd`
## see list of quantiles (only relevant for uncertainty) in `../scripts_compare/Compare_regional_uncertainty.Rmd`

import.all <- function(lrc_long, lrc_lat, size=1, voi.n, depth.n=1, quantile.n=4, which=1:4) {
  library(knitr)
  psm.list <- c(
    "gNATSGO_WCS",
    "SoilGrids250",
    "POLARIS",
    "SPCG100USA",
    "gSSURGO_WCS",
    "LandGIS",
    "GSM_USA_V05",
    "ISSR8")
  for (psm in psm.list[which]) {
    print(paste("Importing", psm))
    print(paste0("Lower-right corner: (", lrc_long, ", ", lrc_lat, ")"))
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


# # Central NY State
# for (d in c(1, 4)) {
#   for (q in c(2)) {
#     for (v in c(4)) {
#       print(paste("depth:", d, "; quantile:", q, "; variable:", v))
#       import.all(lrc_long=-76, lrc_lat=42, size=1, voi.n=v, depth.n=d, quantile.n=q, which=1)
#     }
#   }
# }

# North Carolina
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
for (d in c(1, 4)) {
  # quantile.list <- c("Q0.05", "Q0.5", "Q0.95", "mean")
  for (q in c(1,2,3)) {
    # voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
    for (v in c(1)) {
      print(paste("depth:", d, "; quantile:", q, "; variable:", v))
      import.all(lrc_long=-77, lrc_lat=35, size=1, voi.n=v, depth.n=d, quantile.n=q, which=3)
    }
  }
}

# Indiana
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
for (d in c(1, 2)) {
  # quantile.list <- c("Q0.05", "Q0.5", "Q0.95", "mean")
  for (q in c(1,2,3)) {
    # voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
    for (v in c(6)) {
      print(paste("depth:", d, "; quantile:", q, "; variable:", v))
      import.all(lrc_long=-86, lrc_lat=38, size=1, voi.n=v, depth.n=d, quantile.n=q, which=3)
    }
  }
}

# California
depth.list.sg <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
for (d in c(2, 3)) {
  # quantile.list <- c("Q0.05", "Q0.5", "Q0.95", "mean")
  for (q in c(1,2,3)) {
    # voi.list.sg <- c("clay", "silt", "sand", "phh2o", "cec", "soc", "bdod", "cfvo")
    for (v in c(3)) {
      print(paste("depth:", d, "; quantile:", q, "; variable:", v))
      import.all(lrc_long=-120, lrc_lat=37, size=1, voi.n=v, depth.n=d, quantile.n=q, which=3)
    }
  }
}
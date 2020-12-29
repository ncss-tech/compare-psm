# Extract code from scripts
library(knitr)
import.scripts <- paste0(c(
  "gNATSGO_WCS",
  "GSM_USA_V05",
  "SoilGrids250",
  "POLARIS",
  "SPCG100USA",
  "LandGIS",
  "ISSR8"), "_import.")
for (psm in import.scripts) {
  knitr::purl(paste0("./scripts_importmaps/", psm, "Rmd"),
              output=paste0("./scripts_importmaps/", psm, "R"))
}

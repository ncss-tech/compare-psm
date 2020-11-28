# figure to illustrate inconsistency in legacy gSSURGO

library(terra)
# tile imported by GSM_USA_V05_import.Rmd
(r <- rast("/Users/rossiter/ds/GSM_USA/lat3637_lon-92-91/GSM_mu_claytotal_r_g_kg_030_060.tif"))
r <- r/10 # convert back to %clay
pdf("/Users/rossiter/ds/GSM_USA/lat3637_lon-92-91/MO_AR_GSM_clay_pct_030_060.pdf",
    width=5.5, height=6.5)
terra::plot(r, col=topo.colors(64), range=c(0,100))
dev.off()

(r <- rast("/Volumes/Pythagoras/ds/POLARIS/lat3637_lon-92-91/clay/mean/30_60/lat3637_lon-92-91.tif"))
pdf("/Volumes/Pythagoras/ds/POLARIS/lat3637_lon-92-91/clay/mean/30_60/MO_AR_POLARIS_clay_pct_030_060.pdf",
    width=5.5, height=6.5)
terra::plot(r, col=topo.colors(64), range=c(0,100))
dev.off()

# figure to illustrate inconsistency in legacy gSSURGO

library(terra)
# GSM tile imported by GSM_USA_V05_import.Rmd
(r.gsm <- rast("/Users/rossiter/ds/GSM_USA/lat3637_lon-92-91/GSM_mu_claytotal_r_g_kg_030_060.tif"))
r.gsm <- r.gsm/10 # convert back to %clay
ext(r.gsm)
pdf("/Users/rossiter/ds/GSM_USA/lat3637_lon-92-91/MO_AR_GSM_clay_pct_030_060.pdf",
    width=5.5, height=6.5)
terra::plot(r.gsm, col=topo.colors(64), range=c(0,100))
dev.off()

# POLARIS tile imported by POLARIS_import.Rmd
(r.p <- rast("/Volumes/Pythagoras/ds/POLARIS/lat3637_lon-92-91/clay/mean/30_60/lat3637_lon-92-91.tif"))
ext(r.p)
pdf("/Volumes/Pythagoras/ds/POLARIS/lat3637_lon-92-91/clay/mean/30_60/MO_AR_POLARIS_clay_pct_030_060.pdf",
    width=5.5, height=6.5)
terra::plot(r.p, col=topo.colors(64), range=c(0,100))
dev.off()

# SoilGrids tile imported by SoilGrids250_import.Rmd
(r.sg <- rast("/Users/rossiter/ds/SoilGrids250/lat3637_lon-92-91/clay/mean/30-60cm/clay_30-60cm_mean_4326.tif"))
ext(r.sg)
(r.sg <- terra::crop(r.sg, r.p))
r.sg <- r.sg/10
pdf("/Users/rossiter/ds/SoilGrids250/lat3637_lon-92-91/clay/mean/30-60cm/MO_AR_SoilGrids250_clay_pct_030_060.pdf",
    width=5.5, height=6.5)
terra::plot(r.sg, col=topo.colors(64), range=c(0,100))
dev.off()

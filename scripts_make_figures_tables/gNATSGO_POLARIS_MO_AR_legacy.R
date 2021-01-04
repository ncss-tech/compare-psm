# figure to illustrate inconsistency in legacy gSSURGO

library(terra)
# gNATSGO tile imported by gNATSGO_USA_V05_import.Rmd
(r.gnatsgo <- rast("/Users/rossiter/ds/gNATSGO/lat3637_lon-92-91/claytotal_r_3060.tif"))
# already %clay
ext(r.gnatsgo)
pdf("/Users/rossiter/ds/gNATSGO/lat3637_lon-92-91/MO_AR_gNATSGO_clay_pct_030_060.pdf",
    width=5.5, height=6.5)
terra::plot(r.gnatsgo, col=topo.colors(64), range=c(0,100))
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

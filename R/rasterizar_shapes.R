# 31 de agosto de 2022
# Creacion de raster de distancia humedales y rios para modular la sensibilidad de un pixel dado la distancia a estos
# cuerpos de agua

library(sf)
library(terra)
library(dplyr)
library(lwgeom)
sf::sf_use_s2(FALSE)

ae <- vect("extent_ae/ae_complete.shp")

# raster base
r_base <- rast(x = ae, resolution = 30/(1000*111.32))
cob <- st_read("zon_social/socioeco_uso_suelo/CobTierra_2018_IDEAM/cobertura_tierra_ae_final.shp")

#-----------------------------------------------------
# modular impacto con distancia a rios y humedales

rios <- st_read("zon_social/background/river_lines_0/River_ae_wgs84.shp") |> mutate(value=1) |>
  vect() |> rasterize(r_base, field = "value")
rios[is.na(rios)] <- 0
cob_rios <- cob |> filter(simple == "cuerpos de agua") |> mutate(value=1) |> vect() |> 
  rasterize(r_base, field = "value")
cob_rios[is.na(cob_rios)] <- 0
hum <- st_read("zon_social/background/Humedales_shp/Hum_ae_wgs84.shp") |> mutate(value=1) |>
  vect() |> rasterize(r_base, field = "value")
hum[is.na(hum)] <- 0

modulo <- sum(rios, cob_rios, hum) %>% mask(ae)
modulo[modulo != 0 ] <- 1

writeRaster(modulo, "zon_social/modulo.tif", overwrite = T)

# la herramienta de distancia en R cuelga el computador, se genera en QGIS

modulo <- rast("zon_social/dist_modulo.tif")
modulo <- 1 - ((modulo - 0)/(504.2301 - 0))


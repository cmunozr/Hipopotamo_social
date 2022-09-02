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
cob <- st_read("zon_social/insumos/cobertura_simp_ae.shp")

#-----------------------------------------------------
# modular impacto con distancia a rios y humedales

rios <- st_read("zon_social/insumos/rios_ae.shp") |> mutate(value=1) |>
  vect() |> rasterize(r_base, field = "value")
rios[is.na(rios)] <- 0
cob_agua <- cob |> filter(simple == "cuerpos de agua") |> mutate(value=1) |> vect() |> 
  rasterize(r_base, field = "value")
cob_agua[is.na(cob_agua)] <- 0
hum <- st_read("zon_social/insumos/Humedal_ae.shp") |> mutate(value=1) |>
  vect() |> rasterize(r_base, field = "value")
hum[is.na(hum)] <- 0

modulo <- sum(rios, cob_agua, hum) %>% mask(ae)
modulo[modulo != 0 ] <- 1

writeRaster(modulo, "zon_social/modulo.tif", overwrite = T)

# la herramienta de distancia en R cuelga el computador, se genera en QGIS
# luego se carga y se estandariza entre 0 y 1

modulo <- rast("zon_social/dist_modulo.tif") |> mask(ae)
range_modulo <- minmax(modulo)
modulo <- 1 - ((modulo - range_modulo[1])/(range_modulo[2] - range_modulo[1]))
writeRaster(modulo, "zon_social/modulo.tif", overwrite = T)



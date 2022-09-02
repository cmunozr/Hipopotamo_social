# 31 de agosto de 2022
# Zonificacion 2
# Densidad poblacional

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

# 1. Cabeceras municipales, caserios y asentamientos humanos en area rural dispersa
cascos_caserios <- st_read("zon_social/insumos/cascos_caserios_bf_500m_ae.shp") |>
  mutate(value=1) |> vect() |> rasterize(r_base, field = "value")
cascos_caserios[is.na(cascos_caserios)] <- 0

R_catastro <- st_read("zon_social/insumos/R_construccion_ae.shp") |>st_transform(st_crs(ae)) |> 
  mutate(value=1) |> vect() |> rasterize(r_base, field = "value")
R_catastro[is.na(R_catastro)] <- 0

cob_construcciones <- cob |> filter(nivel_2 == "1.1. Zonas urbanizadas"| nivel_2 == "1.4. Zonas verdes artificializadas, no agrícolas" )|> 
  mutate(value=1) |> vect() |> mask(ae) |> rasterize(r_base, field = "value")
cob_construcciones[is.na(cob_construcciones)] <- 0

alto_impacto2 <- sum(cascos_caserios, R_catastro, cob_construcciones)
alto_impacto2[alto_impacto2 != 0 ] <- 3
rm(cascos_caserios, R_catastro, cob_construcciones)

# 2. Ãreas con equipamientos comunitarios, vias y caminos

R_eqp1 <- st_read("zon_social/insumos/Administrativo_R.shp") |> st_transform(st_crs(ae))
R_eqp1 <- R_eqp1[st_as_sf(ae), ] |> mutate(value=1) |> vect() |> mask(ae) |> rasterize(r_base, field = "value")
R_eqp1[is.na(R_eqp1)] <- 0

vias <- st_read("zon_social/insumos/vias_bf100m_ae.shp") %>% st_transform(st_crs(ae)) %>% 
  filter(TIPO_VIA %in% c(2, 3,4,5,6,7)) |> mutate(value=1) |> vect() |> mask(ae) |> 
  rasterize(r_base, field = "value")
vias[is.na(vias)] <- 0

medio_impacto2 <- sum(R_eqp1, vias)
medio_impacto2[medio_impacto2 != 0 ] <- 2
rm(R_eqp1, vias)

# 3. Ãreas no urbanizadas y espacios naturales

runap <- st_read("zon_social/insumos/runap_ae.shp") |> mutate(value=1) |> vect() |> rasterize(r_base, field = "value")
runap[is.na(runap)] <- 0

# aquellas coberturas sin categoria simple son las construccione humanas, tampoco usar los cuerpos de agua porque
# no son habitables

cob_noconsnoagua <- cob |> filter(!is.na(simple)| simple != "cuerpos de agua")|> mutate(value=1) |> vect() |> 
  mask(ae) |> rasterize(r_base, field = "value")
cob_noconsnoagua[is.na(cob_noconsnoagua)] <- 0

bajo_impacto2 <- sum(runap, cob_noconsnoagua)
bajo_impacto2[bajo_impacto2 != 0 ] <- 1
rm(cob_noconsnoagua, runap)

# suma de capas

zonsocial2 <- sum(alto_impacto2, medio_impacto2, bajo_impacto2) %>% mask(ae)

#Resultados intermedios
# writeRaster(zonsocial2, "zon_social/densidad_poblacional/densidad_nomodulado.tif", overwrite = T)
# zonsocial2 <- rast("zon_social/densidad_poblacional/densidad_nomodulado.tif")

# scores modulados
modulo <- rast("zon_social/modulo.tif")
zonsocial2_modulado <- modulo * zonsocial2
writeRaster(zonsocial2_modulado, "zon_social/densidad_poblacional/densidad_modulado.tif", overwrite = T)

# 31 de agosto de 2022
# Zonificacion 1
# Actividades socioeconomicas segun uso actual del suelo

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

# 1. Ãreas para pesca y extraccin minera aluvial (playones)

# 1.1. Pesca: veredas con informacion de pesca artesanal

pesca <- st_read("zon_social/insumos/pesca_ae.shp") |> filter(pesca == 1) |> 
  mutate(value=1) |> vect() |> rasterize(r_base, field = "value")
pesca[is.na(pesca)] <- 0

# 1.2. extraccion minera aluvial: coberturas, Agencia nacional deminas

min_1 <- cob |> filter(nivel_3 == "1.3.1. Zonas de extracción minera") |> mutate(value=1) |> vect() |> 
  rasterize(r_base, field = "value")
min_1[is.na(min_1)] <- 0

min_2 <- st_read("zon_social/insumos/anh_min.ae.shp") %>% 
  st_transform(st_crs(ae)) %>% filter(ETAPA == "Explotación", grepl(pattern = "ARENA", MINERALES))|>
  mutate(value=1) |> vect() |> rasterize(r_base, field = "value")
min_2[is.na(min_2)] <- 0

# definir alto impacto al sumar minas y pesca
alto_impacto1 <- sum(pesca, min_1, min_2)
alto_impacto1[alto_impacto1 != 0 ] <- 3

# 2. Ãreas de uso pecuario y agricola

# 2.1 coberturas pasto y cultivos (mosaicos)

medio_impacto1 <- cob |> filter(simple == "Pastos"| simple == "Mosaicos de cultivos"| simple == "Cultivos") |>
  mutate(value=2) |> vect() |> rasterize(r_base, field = "value")
medio_impacto1[is.na(medio_impacto1)] <- 0
medio_impacto1[medio_impacto1 != 0] <- 2

# 3. areas naturales y de conservacion

runap <- st_read("zon_social/insumos/runap_ae.shp") |> mutate(value=1) |>
  vect() |> rasterize(r_base, field = "value")
runap[is.na(runap)] <- 0
cob_naturales <- cob |> filter(simple == "Areas naturales")|> mutate(value=1) |> vect() |> mask(ae) |> 
  rasterize(r_base, field = "value")
cob_naturales[is.na(cob_naturales)] <- 0

bajo_impacto1 <- sum(runap, cob_naturales)
bajo_impacto1[bajo_impacto1 != 0] <- 1

# suma de capas
zonsocial1 <- sum(alto_impacto1, medio_impacto1, bajo_impacto1) %>% mask(ae)

# writeRaster(zonsocial1, "zon_social/socioeco_uso_suelo/socioeco_nomodulado.tif",overwrite = T)
# zonsocial1 <- rast("zon_social/socioeco_uso_suelo/socioeco_nomodulado.tif")

# scores modulados
modulo <- rast("zon_social/modulo.tif")
zonsocial1_modulado <- modulo * zonsocial1
writeRaster(zonsocial1_modulado, "zon_social/socioeco_uso_suelo/socioeco_modulado.tif", overwrite = T)



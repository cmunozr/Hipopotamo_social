# transformar y cortar vectores

library(dplyr)
library(sf)
library(terra)
sf::sf_use_s2(FALSE)

ae <- read_sf("extent_ae/ae_complete.shp")
ae_MGN <- ae %>% st_transform(st_crs(9377))
st_write(ae_MGN, "extent_ae/ae_complete_MAGNA.shp", delete_layer = T)

dir.create("zon_social/insumos", showWarnings = F)

#------------------------------------
# 1. uso de suelo 2018 IDEAM
clc2018 <- read_sf("zon_social/socioeco_uso_suelo/CobTierra_2018_IDEAM/cobertura_tierra_extent.shp")
ae2 <- ae |> st_transform(st_crs(clc2018))

# seleccionar las zonas de cobertura que se cruza con el area de referencia
clc_ae <- clc2018[ae2, ]

rm(clc2018)

clc_ae <- clc_ae |> vect() |> mask(vect(ae2))|> st_as_sf() |> st_transform(st_crs(ae))
st_write(clc_ae,"zon_social/socioeco_uso_suelo/CobTierra_2018_IDEAM/cobertura_tierra_ae.shp")

# R no es capaz de eliminar las zonas que no estan dentro del area de estudio, se realiza con QGIS

# volver a cargar la capa y simplificar las coberturas
clc_ae <- read_sf("zon_social/socioeco_uso_suelo/CobTierra_2018_IDEAM/cobertura_tierra_ae_C.shp")
# c es una base de datos previamente preparada en donde se le asignan categorias mas grandes a los niveles 3 de 
# la capa de coberturas. La simplificación de las leyendas nivel 3 del uso de suelo está en la columna simple.
c <- read.csv("c.csv")

clc_ae$uso <- NA
clc_ae$simple <- NA

for(i in 1:nrow(c)){
  target <- c$cobnivel3[i]
  index <- which(clc_ae$nivel_3 == target)
  clc_ae$uso[index] <- c$uso[i]
  clc_ae$simple[index] <- c$simplificado[i]
}

write_sf(clc_ae, "zon_social/insumos/cobertura_simp_ae.shp", delete_layer = T)

#rm(ae2, c, clc_ae, i, index, target)

#------------------------------------
# 2. humedales: transformar crs 
Hum <- read_sf("zon_social/background/Humedales_shp/Hum_extent_ae_wgs84.shp")|> st_transform(st_crs(ae))
write_sf(Hum, "zon_social/background/Humedales_shp/Humedal_ae.shp",delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(Hum)

#------------------------------------
# 3. Inventarios de humedales: transformar crs
invHum <- read_sf("zon_social/background/Inventario de Humedales Continentales e Insulares de Colombia/InvHum_extent_ae_wgs84.shp")|> 
  st_transform(st_crs(ae))
write_sf(invHum, "zon_social/background/Inventario de Humedales Continentales e Insulares de Colombia/InventarioHumedales_ae.shp",
         delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(invHum)

#------------------------------------
# 4. rios: transformar crs
rios <- st_read("mapas_salida_campo22Juni/river_lines_0/River_lines.shp") %>% st_transform(st_crs(ae))
st_write(rios, "zon_social/background/river_lines_0/River_lines_wgs84.shp", delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(rios)

#---------------------------------
# 5. ANH_mineria: filtrar poligonos de mineria con explotacion actual
anh_mineria <- st_read("zon_social/socioeco_uso_suelo/ANH2021/Titulos_mineros_ANH2021.shp") %>% 
  st_transform(st_crs(ae)) %>% filter(ETAPA == "Explotación")
st_write(anh_mineria, "zon_social/socioeco_uso_suelo/ANH2021/Titulos_mineros_explot.shp", delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(anh_mineria)

#---------------------------------
# 6. runap: transformar crs
runap <- st_read("zon_social/socioeco_uso_suelo/runap2/runap2Polygon.shp") %>% st_transform((st_crs(ae)))
st_write(runap, "zon_social/socioeco_uso_suelo/runap2/runap2Polygon.shp", delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(runap)

#----------------------------------
# 7. construcciones
constr_ae <- read_sf("zon_social/densidad_poblacional/Catastro/R_construccion.shp") %>% 
  st_transform(st_crs(ae)) %>% terra::vect() %>% terra::mask(vect(ae)) %>% st_as_sf() %>% 
  st_cast("MULTIPOLYGON")
st_write(constr_ae,"zon_social/densidad_poblacional/Catastro/R_construccion_ae.shp", delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(constr_ae)

#----------------------------------
# 8. vias: remover vias nivel 1 por improbabilidad de cruce de hipopotamos
vias_ae <- st_read("zon_social/densidad_poblacional/vias/vias.shp") %>% filter(TIPO_VIA != 1) %>% 
  st_transform(st_crs(ae)) %>% vect() %>% mask(vect(ae)) %>% st_as_sf() %>% st_cast("MULTILINESTRING")
st_write(vias_ae,"zon_social/densidad_poblacional/vias/vias_ae.shp", delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(vias_ae)

#----------------------------------
# 9. cascos_caserios
cascos_caserios_ae <- st_read("mapas_salida_campo22Juni/MGN2020_URB_AREA_CENSAL/MGN_URB_AREA_CENSAL.shp") %>% 
  st_transform(st_crs(ae)) %>% vect() %>% mask(vect(ae)) %>% st_as_sf() %>% st_cast("MULTIPOLYGON")
st_write(cascos_caserios_ae,"mapas_salida_campo22Juni/MGN2020_URB_AREA_CENSAL/MGN_URB_AREA_CENSAL_ae.shp", delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(cascos_caserios_ae)

#----------------------------------
# 10. Administrativo_R (IGAC 100k gdb) extraido con arcmap
admon_ae <- st_read("zon_social/densidad_poblacional/Administrativo_R.shp") %>% 
  st_transform(st_crs(ae)) %>% vect() %>% mask(vect(ae)) %>% st_as_sf() %>% st_cast("MULTIPOLYGON")
st_write(admon_ae,"zon_social/densidad_poblacional/Administrativo_R.shp", delete_layer = T)
# pasar a QGIS para terminar de cortar la capa
rm(admon_ae)




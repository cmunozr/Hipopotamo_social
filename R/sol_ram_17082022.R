library(sf)
library(dplyr)
library(openxlsx)
library(terra)

# ver_ae: veredas no acotadas al rio, es decir toda el area de estudio
# ver_mag: veredas de las zonas del magdalena medio acotadas al rio
# ver_cor: veredas de cordoba acotadas al rio
# csv de filtro para veredas realmente acotadas

# ver_csv_final <- read.xlsx("sol_17082022/ribereñas/tabla_ver_rib_final.xlsx") %>% filter(usar == 1)
# ver_ae <- read_sf("zon_social/background/veredas/CRVeredas_2020_ae.shp")
# #ver_cor <- read_sf("mapas_salida_campo22Juni/Veredas_Zonas_Estudios/cordoba_veredas.shp")
# #ver_mag <- read_sf("mapas_salida_campo22Juni/Veredas_Zonas_Estudios/Veredas.shp")|> select(any_of(colnames(ver_cor)))
# ver_rib_final <- ver_ae %>% filter(CODIGO_VER %in% ver_csv_final$CODIGO_VER)

ae <- st_read("extent_ae/ae_final_wgs84.shp")

#--------------------------
#tablas
# t_ver_ae <- ver_ae |> st_drop_geometry()
# t_ver_cor <- ver_cor |> st_drop_geometry()
# t_ver_mag <- ver_mag |> st_drop_geometry()

t_ver_final <- st_drop_geometry(ver_rib_final)

#unir tablas de cordoba y ribereñas
# t_ver_rib <- rbind(t_ver_mag, t_ver_cor)

dir.create("sol_17082022", showWarnings = F)
# write.xlsx(t_ver_ae, "sol_17082022/tabla_ver_ae.xlsx", sheetName = "veredas_ae")
# write.xlsx(t_ver_rib, "sol_17082022/tabla_ver_rib.xlsx", sheetName = "veredas_rib")
write.xlsx(t_ver_final, "sol_17082022/tabla_ver_final.xlsx", sheetName = "veredas_final")

#--------------------------
# shapes veredas
write_sf(ver_rib_final, "sol_17082022/ver_rib_final.shp", delete_layer = T)
# write_sf(ver_rib, "sol_17082022/ver_rib.shp", delete_layer = T)

#--------------------------
# coberturas 2018 simplificadas acotadas a: veredas area de estudio y veredas ribereñas final
cobertura_rib_final <- st_read("zon_social/socioeco_uso_suelo/CobTierra_2018_IDEAM/cobertura_tierra_ae.shp")
c <- read.csv("c.csv")

cobertura_rib_final$uso <- NA
cobertura_rib_final$simple <- NA

for(i in 1:nrow(c)){
  target <- c$cobnivel3[i]
  index <- which(cobertura_rib_final$nivel_3 == target)
  cobertura_rib_final$uso[index] <- c$uso[i]
  cobertura_rib_final$simple[index] <- c$simplificado[i]
}

write_sf(cobertura_rib_final, "sol_17082022/area_estudio/cobertura_tierra_ae_final.shp")


desem <- read_sf("zon_social/socioeco_uso_suelo/SEPEC/SitiosDesembarco_ActDiaria.shp")
a <- read.xlsx("zon_social/socioeco_uso_suelo/SEPEC/Estimacion_vol_arte_desembarcados_2020.xlsx") %>% 
  filter(.data = ., Cuenca == "Magdalena", !is.na(Longitud), !is.na(Latitud))

a$Latitud <- gsub(pattern = "°", replacement = " ", x = a$Latitud) %>% gsub(pattern = "''", replacement = "", x = .) %>% 
  gsub(pattern = "'", replacement = " ", x = .) %>% gsub(pattern = "N", replacement = "", x = .) %>% 
  conv_unit(., from = "deg_min_sec", to = "dec_deg")

a$Longitud <- gsub(pattern = " ", replacement = "", x = a$Longitud) %>% 
  gsub(pattern = "°", replacement = " ", x = a$Longitud) %>% gsub(pattern = "''", replacement = "", x = .) %>% 
  gsub(pattern = "'", replacement = " ", x = .) %>% gsub(pattern = "W", replacement = "", x = .) %>%
  paste0("-", .) %>% conv_unit(., from = "deg_min_sec", to = "dec_deg") 

gen.st.points <- function(dat, collon = col.lon, collat = col.lat) {
  st.points <- dat %>%
    st_as_sf(coords = c(collon, collat), crs = st_crs("EPSG:4326")) %>%
    st_transform(st_crs("EPSG:4326"))
}

asp <- gen.st.points(dat = a, collon = "Longitud", collat = "Latitud")

write_sf(asp, "cualquier.shp")
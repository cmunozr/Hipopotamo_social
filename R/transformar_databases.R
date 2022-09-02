# 31 de agosto de 2022
# Construccion de base de datos georeferenciada de pesca artesanal

library(data.table)
library(dplyr)
library(sf)
library(openxlsx)
library(measurements)
library(stringi)

gen.st.points <- function(dat, collon = col.lon, collat = col.lat) {
  st.points <- dat %>%
    st_as_sf(coords = c(collon, collat), crs = st_crs("EPSG:4326")) %>%
    st_transform(st_crs("EPSG:4326"))
}

#-------------------------

ae <- read_sf("extent_ae/ae_final_wgs84.shp") 
ver <- ae %>% st_drop_geometry() %>% select("CODIGO_VER") %>%  pull() %>% as.numeric()

#------------------------------------
# Censo nacional agropecuario 2014: pesca artesanal
cna_upaGeneral <- fread("zon_social/socioeco_uso_suelo/3erCensoNA_2014/S01_15(Unidad_productora).csv") 


vect_cols <- c("UC_UO", "COD_VEREDA")

cna_upaGeneral <- cna_upaGeneral %>%  select(all_of(vect_cols) | contains("P_S8P")) %>%
  filter(P_S8P107 == 1, COD_VEREDA %in% ver) 

num_upa <- cna_upaGeneral %>% group_by(COD_VEREDA) %>% count(UC_UO) %>% 
  summarize(Num_UC=dplyr::n()) 

ae$pesca <- rep(NA, nrow(ae))
ae$pesca_upa <- rep(NA, nrow(ae))

for(i in 1:nrow(num_upa)){
  index <- which(as.numeric(ae$CODIGO_VER) == num_upa$COD_VEREDA[i])
  ae$pesca[index] <- 1
  ae$pesca_upa[index] <- num_upa$Num_UC[i]
}

cna_pesca <- fread("zon_social/socioeco_uso_suelo/3erCensoNA_2014/S08(Pesca_artesanal).csv") %>% 
  select(any_of(vect_cols) | contains("P_S8P")) %>% filter(COD_VEREDA %in% ver)

num_upa2 <- cna_upaGeneral %>% group_by(COD_VEREDA) %>% count(UC_UO) %>% 
  summarize(Num_UC=dplyr::n()) 

ae$pesca2 <- rep(NA, nrow(ae))
ae$pesca_upa2 <- rep(NA, nrow(ae))

for(i in 1:nrow(num_upa2)){
  index <- which(as.numeric(ae$CODIGO_VER) == num_upa2$COD_VEREDA[i])
  ae$pesca2[index] <- 1
  ae$pesca_upa2[index] <- num_upa2$Num_UC[i]
}

write_sf(ae, "zon_social/socioeco_uso_suelo/pesca")

# despues de revisar la informacion guardada en las columnas pesca vs pesca2
# y las pesca_upa vs pesca_upa2 provenientes de las diferentes tablas generan los mismos
# números

#------------------------------------
# desembarcaderos de pesca
pesca <- read_sf("zon_social/socioeco_uso_suelo/pesca.shp")

# Desembarcaderos sepec
desem <- read_sf("zon_social/socioeco_uso_suelo/SEPEC/SitiosDesembarco_ActDiaria.shp")

pesca_desem <- pesca[desem, ] %>% st_drop_geometry()

for(i in 1:nrow(ae_desem)){
  index <- which(pesca$CODIGO_VER == ae_desem$CODIGO_VER[i])
  pesca$pesca[index] <- 1
}

write_sf(pesca, "zon_social/socioeco_uso_suelo/pesca.shp", delete_layer = T)

#------------------------------------
# Encuestas Zona 2 y 3: revision y ubicación automatica
pesca <- read_sf("zon_social/socioeco_uso_suelo/pesca.shp")

data23 <- read.xlsx("salidas_campo/Encuestas Zonas 2y3 25-07-2022.xlsx", sheet = 1)

# geolocalizar formulario: algunos puntos no tienen coordenadas, algunos no fueron
# bien tomados y otros no hacen parte del area de estudio
workData <- data23[- which(is.na(data23$X) |  is.na(data23$Y)), ] %>% select(.,- which(duplicated(colnames(.)) == T )) %>% 
  filter(Nombre.encuestador != "Paola Acosta ", Municipio != "Medellín")

for(i in 1:nrow(workData)){
  info <- workData$X[i] %>% nchar() %>% as.numeric()
  workData$X[i] <- workData$X[i]/(1*10^(info-1))
  
  info2 <- workData$Y[i] %>% nchar() %>% as.numeric()
  workData$Y[i] <- workData$Y[i]/(1*10^(info2-3))
}

workData <- gen.st.points(workData, "Y", "X")

write_sf(workData, "salidas_campo/Enc2y3.shp", delete_layer = T)

# datos de pesca de las encuestas geolocalizadas
workDatapes <- workData[grepl(pattern = "*pesc*|*Pesc*", x = workData$`Oficio/Cargo`), ]
workDatapes <- pesca[workDatapes, ]

for(i in 1:nrow(pesca)){
  index <- which(pesca$CODIGO_VER == workDatapes$CODIGO_VER[i])
  pesca$pesca[index] <- 1
}

# datos de la encuesta sin coordenadas o erradas con el fin de georeferenciar
nonworkData <- data12 %>% select(.,- which(duplicated(colnames(.)) == T )) %>% 
  filter(Nombre.encuestador == "Paola Acosta " | is.na(X))
write.csv("salidas_campo/togeocode.csv")

# se geocodifica de forma automatica con geopaify, al revisar el documento 
# recuperado del api algunos no coinciden con ellugar o país por lo que fue filtrado
# manualmente
geocoded <- read.csv("salidas_campo/geocoded_by_geoapify-12_8_2022, 7_29_58 p. m..csv")
nonworkData$X <- geocoded$lat
nonworkData$Y <- geocoded$lon

nonworkData_sf <-  nonworkData %>% filter(!is.na(X)) %>% gen.st.points(., "Y", "X")
write_sf(nonworkData_sf, "salidas_campo/Enc2y3_geocoded.shp", delete_layer = T)

# datos de pesca geolocalizados
nonworkDatapes <- nonworkData[grepl(pattern = "*pesc*|*Pesc*", x = nonworkData$`Oficio/Cargo`), ] %>% 
  filter(!is.na(X))

nonworkDatapes <- gen.st.points(nonworkDatapes, "Y", "X")
nonworkDatapes <- pesca[nonworkDatapes, ]

for(i in 1:nrow(pesca)){
  index <- which(pesca$CODIGO_VER == nonworkDatapes$CODIGO_VER[i])
  pesca$pesca[index] <- 1
}

write_sf(pesca, "zon_social/socioeco_uso_suelo/pesca.shp", delete_layer = T)

toManual_workData <- nonworkData %>% filter(is.na(X))
write.csv(toManual_workData, "salidas_campo/Enc2y3_toManual.csv", row.names = F)

#-----------------------------------
# Encuestas Zona 2 y 3
# cruzar aquellos que no se encontraron de forma automatica con el kml de forma manual
pesca <- read_sf("zon_social/socioeco_uso_suelo/pesca.shp")
ptos_compilados <- read_sf("salidas_campo/PuntosGpsCompilados.shp") %>% st_transform(st_crs(pesca)) %>% 
  select(Name)

# el vector patrón se cambia manualmente en busqueda de diferentes formas de escritura y posibles
# expresiones que puedan hacer match
ptos <- ptos_compilados[grepl(x = ptos_compilados$Name, pattern = "sierra"), ]

#---------------------------------------
# Encuestas Zona 2 y 3: ubicación manual
pesca <- read_sf("zon_social/socioeco_uso_suelo/pesca.shp")
ManualGeolo <- read.csv("salidas_campo/Enc2y3_toManualGeoloca.csv")

ManualGeolo_sf <-  ManualGeolo %>% filter(!is.na(X)) %>% gen.st.points(., "Y", "X")
write_sf(ManualGeolo_sf, "salidas_campo/Enc2y3_Manual.shp", delete_layer = T)

ManualGeolopes <- ManualGeolo_sf[grepl(pattern = "*pesc*|*Pesc*", x = ManualGeolo_sf$Oficio.Cargo), ]

ManualGeolopes <- pesca[ManualGeolopes, ]

for(i in 1:nrow(pesca)){
  index <- which(pesca$CODIGO_VER == ManualGeolopes$CODIGO_VER[i])
  pesca$pesca[index] <- 1
}

#---------------------------
# Encuestas Zona 1 y 2: calculo de centroides para las veredas: 
# 
vectVer <- c("PUERTO PITA", "ESTACION COCORNA", "BALSORA", "LA ANGELITA", "MULAS", "PUERTO PERALES")
pesca <- read_sf("zon_social/socioeco_uso_suelo/pesca.shp") %>% filter(NOMBRE_VER %in% vectVer)

st_centroid(pesca)

# DPTOMPIO CODIGO_VER NOM_DEP   NOMB_MPIO      NOMBRE_VER   COD_DPTO pesca pesca_upa pesca2 pesca_upa2             geometry
# * <chr>    <chr>      <chr>     <chr>          <chr>        <chr>    <dbl>     <int>  <dbl>      <int>          <POINT [°]>
#   1 05591    05591002   ANTIOQUIA PUERTO TRIUNFO BALSORA      05           1         0      0          0 (-74.80659 5.956067)
# 2 05591    05591010   ANTIOQUIA PUERTO TRIUNFO PUERTO PITA  05           1         0      0          0  (-74.65424 5.95753)
# 3 05591    05591004   ANTIOQUIA PUERTO TRIUNFO ESTACION CO~ 05           1         0      0          0  (-74.66527 6.01046)
# 4 05591    05591009   ANTIOQUIA PUERTO TRIUNFO PUERTO PERA~ 05          NA        NA     NA         NA (-74.59829 6.007531)
# 5 05585    05585008   ANTIOQUIA PUERTO NARE    LA ANGELITA  05           1         1      1          1 (-74.58919 6.064642)
# 6 05585    05585018   ANTIOQUIA PUERTO NARE    MULAS        05          NA        NA     NA         NA  (-74.6298 6.119623)

# arreglando columnas
tofix <- read.csv("salidas_campo/gotoR.csv")

fixed <- rep(NA, nrow(tofix))
for(i in 1:nrow(tofix)){
  rows <- tofix[i, ] %>% t()
  index <- which(rows == 1)
  fixed[i] <- row.names(rows)[index] %>% na.omit() %>% paste(collapse = ", ")
}
write.csv(fixed, "salidas_campo/fixed.csv", row.names = T)

# percepcion/tolerancia

library(xlsx)
library(sf)

gen.st.points <- function(dat, collon = col.lon, collat = col.lat) {
  st.points <- dat %>%
    st_as_sf(coords = c(collon, collat), crs = st_crs("EPSG:4326")) %>%
    st_transform(st_crs("EPSG:4326"))
}
#-----------------

data_C <- read.xlsx("zon_social/percepcion_tolerancia/Tolerancia Hipos SIG 21-08-2022.xlsx", sheetIndex = 1,
                    encoding = "UTF-8", stringsAsFactors = F, colIndex = c(1:16))
# geolocalizar formulario: 
# datos con coordenadas exactas por evento
WorkData <- data_C[- which(is.na(data_C$X) |  is.na(data_C$Y)), ]
WorkData$X <- as.numeric(WorkData$X)
WorkData$Y <- as.numeric(WorkData$Y)

for(i in 1:nrow(WorkData)){
  if(WorkData$X[i] > 80){
    
  info <- WorkData$X[i] %>% nchar() %>% as.numeric()
  WorkData$X[i] <- WorkData$X[i]/(1*10^(info-1))
  } 
  if(WorkData$Y[i] < -80){
    info2 <- WorkData$Y[i] %>% nchar() %>% as.numeric()
    WorkData$Y[i] <- WorkData$Y[i]/(1*10^(info2-3))  
  }
}

WorkData <- gen.st.points(WorkData, "Y", "X")
write_sf(WorkData, "zon_social/percepcion_tolerancia/Tolerancia.shp", delete_layer = T)

# datos sin georeferenciar
NonWorkData <- data_C[which(is.na(data_C$X) |  is.na(data_C$Y)), ]
write.csv(NonWorkData, "zon_social/percepcion_tolerancia/NonWorkData.csv", row.names = F)


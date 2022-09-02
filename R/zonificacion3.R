# 1 de septiembre de 2022
#  tolerancia/intolerancia

library(sf)
library(terra)
library(dplyr)
library(lwgeom)
sf::sf_use_s2(FALSE)

#-----------------------
# Estructura data.frame de resumen de registros por poligono 
# Var1           Var2             Freq
# <fct>          <fct>           <int>
#   1 05425200304003 inofensivos         0
# 2 05425200304003 muy peligrosos      0
# 3 05425200304003 peligrosos          0
# 4 05425200304003 poco peligrosos     0

select_cat_mult <- function(groupx){
  group <- groupx %>% as.data.frame()
  summed <- sum(group$Freq)
  if(summed == 0){
    res <- NA
    COD <- group$Var1[1]
    muypel <- NA  ; pel <- NA; pocpel <- NA; inof <- NA
    df <- data.frame("nm" = COD, "res" = res, "muypel" = muypel, "pel" = pel, "pocpel" = pocpel, "inof" = inof )
  }else{
    mx <- group$Freq %>% max()
    logic_i <- mx == group$Freq
    
    if(sum(logic_i) > 1){
      res <- ("Indeciso")
    }else{
      mx_index <- group$Freq %>% which.max()
      mx_data <- group$Var2[mx_index]
      res <- paste0("Mayoritariamente ", mx_data)
    }
    COD <- group$Var1[1]
    muypel <- group$Freq[2]  ; pel <- group$Freq[3]; pocpel <- group$Freq[4]; inof <- group$Freq[1]
    df <- data.frame("nm" = COD, "res" = res, "muypel" = muypel, "pel" = pel, "pocpel" = pocpel, "inof" = inof )
  }
  
  return(df)
}
#-----------------------

ae <- vect("extent_ae/ae_complete.shp")
# raster base
r_base <- rast(x = ae, resolution = 30/(1000*111.32))

#-----------------------------------------------------
# rasterizar los shapefiles de veredas y cascos urbanos

cascos_ver <- st_read("zon_social/insumos/union_cascos_veredas_noMeta.shp")
cascos_ver <- cascos_ver[order(cascos_ver$COD), ]
cascos_ver_r <- cascos_ver |> vect() |> rasterize(r_base, field = "COD")


# llamar datos de trabajo de las encuestas
WorkData <- st_read("zon_social/percepcion_tolerancia/Tolerancia.shp") 
WorkData_vect <- WorkData %>% vect()

# extraer datos de la localidad (vereda-casco) en donde se ubica cada registro y adicionar a la base de datos
# original
WorkData$COD <- extract(cascos_ver_r, WorkData_vect)[,"COD"]

#----------------------------
# resumir datos de la columna plgrsdd (peligrosidad), ubicarlos en el mapa
peligrosidad <- table(WorkData$COD, WorkData$plgrsdd) %>% data.frame() %>% group_split(Var1, .keep = T) %>% 
  lapply(X = ., FUN = function(X){select_cat_mult(groupx = X)}) %>% do.call(rbind, .)
peligrosidad <- peligrosidad[order(peligrosidad$nm), ]
peligrosidad_spat <- cbind(cascos_ver, peligrosidad)
peligrosidad_spat <- peligrosidad_spat[!is.na(peligrosidad_spat$res), ]
st_write(peligrosidad_spat, "zon_social/percepcion_tolerancia/peligrosidad.shp", delete_layer = T)

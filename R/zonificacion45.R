# 1 de septiembre de 2022
#  tolerancia/intolerancia

library(sf)
library(terra)
library(dplyr)
library(lwgeom)
sf::sf_use_s2(FALSE)

#-----------------------
select_cat_bin <- function(groupx){
  group <- groupx %>% as.data.frame()
  summed <- sum(group$Freq)
  if(summed == 0){
    res <- NA
    COD <- group$Var1[1]
    no <- NA
    si <- NA
    df <- data.frame("nm" = COD, "res" = res, "no" = no, "si" = si )
  }else{
    mx <- group$Freq %>% max()
    mn <- group$Freq %>% min()
    
    if(mx == mn){
      res <- ("Indeciso")
    }else{
      mx_index <- group$Freq %>% which.max()
      mx_data <- group$Var2[mx_index]
      res <- paste0("Mayoritariamente ", mx_data)
    }
    COD <- group$Var1[1]
    no <- group$Freq[1]
    si <- group$Freq[2]
    df <- data.frame("nm" = COD, "res" = res, "no" = no, "si" = si )
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
# resumir datos de las columnas gusto y miedo, ubicarlos en el mapa
gusto <- table(WorkData$COD, WorkData$gusta) %>% data.frame() %>% group_split(Var1, .keep = T) %>% 
  lapply(X = ., FUN = function(X){select_cat_bin(groupx = X)}) %>% do.call(rbind, .)
gusto <- gusto[order(gusto$nm), ]
gusto_spat <- cbind(cascos_ver, gusto)
gusto_spat <- gusto_spat[!is.na(gusto_spat$res), ]
st_write(gusto_spat, "zon_social/gusto_miedo/gusto.shp", delete_layer = T)


miedo <- table(WorkData$COD, WorkData$miedo) %>% data.frame() %>% group_split(Var1, .keep = T) %>% 
  lapply(X = ., FUN = function(X){select_cat_bin(groupx = X)}) %>% do.call(rbind, .)
miedo <- miedo[order(miedo$nm), ]
miedo_spat <- cbind(cascos_ver, miedo) 
miedo_spat <- miedo_spat[!is.na(miedo_spat$res), ]
st_write(miedo_spat, "zon_social/gusto_miedo/miedo.shp", delete_layer = T)

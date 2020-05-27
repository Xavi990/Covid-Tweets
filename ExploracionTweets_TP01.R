#
# TP 01 Data Mmining 2020
#
# Guadalupe Castaño
# Javier Gamboa
# Laura Eliaschev
#


#---------------------------------
# Limpieza del ambiente de trabajo
#---------------------------------

#Borrado de todas las variables / datos definidos en R y de la consola
rm(list = ls())
cat(rep("\n",50))

#cambio de directorio de trabajo
dir_root = "C:\\Users\\elias\\Documents\\Maestria\\DM20\\TP01"


#------------------------
# Instalacion de paquetes
#------------------------
#install.packages("Rtools")
#install.packages("mongolite")
#install.packages("base")
#install.packages("dplyr")     # manipulación de dataframes 

library(mongolite)
library(readxl)
library(ggplot2)
library(grDevices)            # Equipos graficos y soporte para la base y la red de graficos
library(tcltk)
library(aplpack)
library(corrplot)
#library(tidyverse)
library(dplyr) 


#---------------------
# Importacion de datos
#---------------------

#Levantado de datos de Mongo
tweets_v01         = mongo(db = "DMUBA", collection = "tweets_v01")
hashtags_x_usuario = mongo(db = "DMUBA", collection = "hashtags_usados_x_usuario")
hashtags_usados    = mongo(db = "DMUBA", collection = "hashtags_usados")
locations          = mongo(db = "DMUBA", collection = "locations")
usuarios           = mongo(db = "DMUBA", collection = "users_mongo_covid19")


#Armado de los dataframes
df_tweets_01          = tweets_v01$find()
df_hashtags_x_usuario = hashtags_x_usuario$find()
df_hashtags_usados    = hashtags_usados$find()
df_locations          = locations$find()
df_usuarios           = usuarios$find()


#---------------------------
# Recategorizacion de Paises
#---------------------------

#Cálculo de totales
nombre_cols       = c("Total", "Correctas", "Nulas", "Ruidosas")
nombre_fils       = c("Valor", "Porcentaje")
nombre_dims      <-list(nombre_fils, nombre_cols)              
totales_locations = matrix(data = NA, nrow = 2, ncol = 4, byrow = FALSE, dimnames = nombre_dims)

totales_locations["Valor","Total"]     = sum(df_locations$count)
totales_locations["Valor","Nulas"]     = sum(df_locations$count[which(is.na(df_locations$location))])
totales_locations["Valor","Correctas"] = sum(df_locations$count[which(!is.na(df_locations$location))])
totales_locations["Valor","Ruidosas"]  = 0
totales_locations["Porcentaje", "Total"]     = 100
totales_locations["Porcentaje", "Nulas"]     = (totales_locations["Valor","Nulas"]/totales_locations["Valor", "Total"]*100)
totales_locations["Porcentaje", "Correctas"] = (totales_locations["Valor","Correctas"]/totales_locations["Valor", "Total"]*100)
totales_locations["Porcentaje", "Ruidosas"]  = (totales_locations["Valor","Ruidosas"]/totales_locations["Valor", "Total"]*100)

pie(totales_locations["Porcentaje",c(2:4)], col=rainbow(25),font=8,cex=1.5,radius=1,border=F,main="Info Locaciones")


#Levantado del archivo de ciudades por pais y borrado de las dos últimas columnas que no nos interesan
#rm(datos_ciudades)
datos_ciudades  = read.csv(header=TRUE, file="C:\\Users\\elias\\Documents\\Maestria\\DM20\\TP01\\worldcities.csv")
datos_ciudades <- datos_ciudades[,-c(3:4)]


#Agregado de columna paises al datafrema de locations
cant_locations = length(df_locations$location)
pais           = c(NA*cant_locations)
df_locations   = cbind(df_locations, pais)

#Algoritmo 1: sólo reemplazo paises
df_locations$pais <- c(NA*cant_locations)
df_paises = unique(datos_ciudades$country)
for (pais in df_paises) {
  lista   = grep(pais,   df_locations$location)
  df_locations$pais = replace(df_locations$pais, lista, pais)
} 


#Algoritmo 2: usando funciones de R --> muy lento
for (j in 1:length(datos_ciudades$country)) {
  pais   = as.vector(datos_ciudades$country)[j]
  ciudad = as.vector(datos_ciudades$name)[j]

  lista_pais   = grep(pais,   df_locations$location)
  lista_ciudad = grep(ciudad, df_locations$location)
  lista        = c(lista_pais,lista_ciudad)
  lista       <- unique(sort(lista))
  
  df_locations$pais = replace(df_locations$pais, lista, pais)
} 


#Algoritmo 3 usando for anidados --> muy lento
for (i in 1:length(df_locations$location)) {
  for (j in 1:length(datos_ciudades$name)) {
        if (grepl(datos_ciudades$country[j], df_locations[i,2]) |
            grepl(datos_ciudades$name[j],    df_locations[i,2])) {
              df_locations[i,3] <- as.vector(datos_ciudades[j,2])
        }
  }
}


# Algoritmo 5: usando funciones para apply

#primer función que obtiene el pais de los registros bien cargados
getPais <- function(lugar) {
  pais = NA
  fila_pais = match(lugar, datos_ciudades$country)
  if (is.na(fila_pais)) {
    fila_pais = match(lugar, datos_ciudades$name)
  }
  if (!is.na(fila_pais)){
    pais = as.vector(datos_ciudades$country[fila_pais])
  }
  return (pais)
} 

#pruebitas
prulis = list("Buenos Aires", "Colombia", "Barcelona")
getPais(prulis)
a=lapply(prulis, getPais)

#Corrida OK
df_locations$pais <- lapply(df_locations$location, getPais)


#segunda funcion que obtiene los paises de buscar en toda la tabla ==> Lenta
getPais2 <- function(lugar) {
  pais = NA
  lista_ciudades = lapply(datos_ciudades$name, grepl, lugar)
  pais = which(as.vector(lista_ciudades)>0)
  return (pais)
}

#pruebita
prupais2=lapply(df_locations$location, getPais2)

#tercera funcion que obtiene los paises de buscar en toda la tabla 
getPais3 <- function(lugar) {
  pais = NA
  fila_pais=grep(lugar, datos_ciudades$name, fixed=TRUE)
  pais = as.vector(datos_ciudades$country)[fila_pais]  
  return (pais)
}

prupais3=lapply(df_locations$location, getPais3)

#encontré un registro con ruido
ruido1=as.vector(grep("Justamente",df_locations$location))
df_locations$location[ruido1]="Justamente"







#
# TP 01 Data Mmining 2020
#
# Guadalupe Castaño
# Javier Gamboa
# Laura Eliaschev
#


#Borrado de todas las variables / datos definidos en R
rm(list = ls())


#cambio de directorio de trabajo
dir_root = "C:\\Users\\elias\\Documents\\Maestria\\DM20\\TP01"


#Instalacion de paquetes
#install.packages("Rtools")
#install.packages("mongolite")
library(mongolite)
#library(readxl)
#library(ggplot2)


#Levantado de datos de Mongo
tweets_v01 = mongo(db = "DMUBA", collection = "tweets_v01")
hashtags_x_usuario = mongo(db = "DMUBA", collection = "hashtags_usados_x_usuario")
hashtags_usados = mongo(db = "DMUBA", collection = "hashtags_usados")
locations = mongo(db = "DMUBA", collection = "locations")

#Armado del dataframe
df_tweets_01 = tweets_v01$find()
df_hashtags_x_usuario = hashtags_x_usuario$find()
df_hashtags_usados = hashtags_usados$find()
df_locations = locations$find()


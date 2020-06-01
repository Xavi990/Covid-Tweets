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
#install.packages("stringi")

library(mongolite)
library(readxl)
library(ggplot2)
library(grDevices)            # Equipos graficos y soporte para la base y la red de graficos
library(tcltk)
library(aplpack)
library(corrplot)
library(dplyr) 
library(stringi)
library(ggplot2)
library(scales)

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

#rm(df_locations)

#---------------------------
# Recategorizacion de Paises
#---------------------------


#Levantado del archivo de ciudades por pais y borrado de las dos últimas columnas que no nos interesan
#rm(datos_ciudades)
datos_ciudades  = read.csv(header=TRUE, file="C:\\Users\\elias\\Documents\\Maestria\\DM20\\TP01\\worldcities.csv")
datos_ciudades <- datos_ciudades[,-c(3:4)]
#datos_ciudades 


#Agregado de columna paises al datafrema de Tweets
#rm (pais)
#rm (df_tweets_02)
df_tweets_02 = data.frame(df_tweets_01$location)
colnames(df_tweets_02)[1] <- "location"
cant_tweets  = length(df_tweets_02$location)
lista_loc_ok    = c(NA*cant_tweets)
lista_pais_nula = c(NA*cant_tweets)
df_tweets_02$loc_ok <- lista_loc_ok
df_tweets_02$pais   <- lista_pais_nula 

#
# Generación de dato Pais a partir del dato Location
#

#Saco acentos, ascii raros y paso a minúsculas
df_tweets_02$loc_ok <- chartr("áéíóúü","aeiouu",df_tweets_02$location)
df_tweets_02$loc_ok <- iconv(df_tweets_02$loc_ok,'utf-8','ascii', sub = '')
df_tweets_02$loc_ok <- tolower(df_tweets_02$loc_ok)

#Arreglo a mano datos de España, Brasil y Argentina
lista   = grep("espaa",   df_tweets_02$loc_ok)
df_tweets_02$pais = replace(df_tweets_02$pais, lista, "Spain")
lista   = grep("brasil",   df_tweets_02$loc_ok)
df_tweets_02$pais = replace(df_tweets_02s$pais, lista, "Brazil")
lista   = grep("ciudad autonoma",   df_tweets_02$loc_ok)
df_tweets_02$pais = replace(df_tweets_02$pais, lista, "Argentina")
lista   = grep("CABA",   df_tweets_02$loc_ok)
df_tweets_02$pais = replace(df_tweets_02$pais, lista, "Argentina")


# Solo busco por pais
df_paises = unique(datos_ciudades$country)
for (pais in df_paises) {
  lista   = grep(tolower(pais),   df_tweets_02$loc_ok)
  df_tweets_02$pais = replace(df_tweets_02$pais, lista, pais)
} 


# Busco en las ciudades para aquellos que tienen el país nulo
lista = which(is.na(df_tweets_02$pais))
for (fila in lista) {
  fila_pais = match(df_tweets_02$loc_ok[fila], tolower(datos_ciudades$name))
  if (!is.na(fila_pais)) {
    df_tweets_02$pais[fila] <- as.vector(datos_ciudades$country[fila_pais])
  }
}

#Reemplazo Spain por España
lista   = grep("Spain",   df_tweets_02$pais)
df_tweets_02$pais = replace(df_tweets_02$pais, lista, "España")

#Listo! Paso los paises al dataframe original
df_tweets_01$pais <- df_tweets_02$pais

#Guardo el archivo en .csv
write.csv(df_tweets_01, file="new_tweets_01.scv")

#---------
# Graficos
#---------


#Porcentaje de Paises nulos
factor_paises_nulos <- summary(as.factor(is.na(df_tweets_01$pais))) 
freq_paises_nulos = table(is.na(df_tweets_01$pais))
df_paises_nulos = as.data.frame(freq_paises_nulos)


pnulos   = freq_paises_nulos[1]/(freq_paises_nulos[1]+freq_paises_nulos[2])*100
pnonulos = freq_paises_nulos[2]/(freq_paises_nulos[1]+freq_paises_nulos[2])*100

categorias  <- c("Nulos", "No nulos")
porcentajes <- c(pnulos, pnonulos)
df <- data.frame(categorias, porcentajes)

ggplot(df,mapping = aes(x="",y=porcentajes, fill=categorias))+
  geom_bar(stat = "identity",color="white")

ggplot(df,aes(x="",y=porcentajes, fill=categorias))+
  geom_bar(stat = "identity",color="white")+
  coord_polar(theta="y")

pie(factor_paises_nulos, col=rainbow(25),font=8,cex=1.5,radius=1,border=F,main="Porcentaje Paises Nulos")



#Cantidad de Tweets por Pais
tabla_paises = head(sort(table(df_tweets_01$pais), decreasing = TRUE), n=15)
df <- data.frame(pais=tabla_paises)
colnames(df)[1] <- "cantidad"
colnames(df)[2] <- "paises"


dimnames(tabla_paises)
nrow(tabla_paises)
hist(tabla_paises, breaks = 15, freq = TRUE)
plot(tabla_paises, main="Tweets por pais", xlab="Pais", ylab="Cantidad", type="h", lwd=10, col="turquoise")
barplot(height = tabla_paises[cantidad], names.arg = tabla_paises[paises], axisnames = TRUE)


cantidad = df$cantidad
paises   = df$paises

ggplot(df, aes(cantidad, paises)) +
  geom_bar(stat = "identity",fill = rgb(0.2, 0.2, 1, 0.3), color = "blue") +
  coord_flip() + 
  theme_minimal()


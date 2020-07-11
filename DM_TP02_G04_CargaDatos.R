#
# TP 02 Data Mmining 2020
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
dir_root = "C:\\Users\\elias\\Documents\\Maestria\\DM20\\TP02"


#------------------------
# Instalacion de paquetes
#------------------------
#install.packages("Rtools")
#install.packages("mongolite")
#install.packages("base")
#install.packages("dplyr")     # manipulación de dataframes 
#install.packages("stringi")
#install.packages("arules")

library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)



# Nombres de las colecciones que definimos en mongo
c1 = "usuarios_nhashtgs_dia"
c2 = "usuarios_hashtags"
c3 = "tweets_mongo_covid19" 
c4 = "backup_usuarios"

usr_nhashtgs_dia <- mongo(collection = c1, db = "DMUBA")
usr_hashtags     <- mongo(collection = c2, db = "DMUBA")
tweets           <- mongo(collection = c3, db = "DMUBA")
users            <- mongo(collection = c4, db = "DMUBA")

# Lectura de las colecciones
df_c1 = usr_nhashtgs_dia$find(query = '{}',
                              fields = '{ "_id": false, "user_id" : true, "dia_semana" : true, "n_hashtags": true, "friends_count": true, "followers_count": true, "retweet_favorite_count": true, "retweet_retweet_count": true}')
#View(head(df_c1))

df_c2 = usr_hashtags$find(query = '{}', fields = '{"_id": false, "user_id" : true, "cat_hashtags" : true}')
#View(head(df_c2))

df_c3 = tweets$find()
df_c4 = users$find(query = '{}', 
                    fields = '{ "_id": false, "user_id" : true,"account_created_at" : true , "friends_count" : true, "favourites_count":true, "verified": true,"statuses_count": true  }')



# Análisis exploratirio: contamos y ploteamos la Cantidad de Tweets por Día
df_c3$fecha_tweet = as.Date(df_c3$created_at,'%Y-%m-%d') 
tweets_x_dia     = aggregate(df_c3$fecha_tweet,by=list(df_c3$fecha_tweet),FUN=length)

Graph1 <- ggplot( data = tweets_x_dia, aes( Group.1, x )) +
   geom_line(colour="blue", linetype=2, size=1.2) + 
   geom_point(colour="purple", size=3.5, shape=20, fill="white")+
   scale_x_date(date_breaks="3 day") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   geom_text(aes(label = round(x, 1)),
             vjust = -0.5, hjust = 0.5, size=4,
             show.legend = FALSE) +
   theme(axis.text.x=element_text(angle=90))+
   theme_bw() + labs(
     x = "Date",
     y = "Tweets", 
     title = "Cantidad de Tweets por Día"
) 

Graph1

# PRIMERA IMPRESION
# En el 1er TP teniamos datos hasta el 15 de Mayo, acá se agregaron hasta el 7/06. Pero sin uniformidad.
# La mayoria de los tweets son del 29 y 30 de Mayo (dificultad para ver relacion de dias)
# Son 443.734 tweets en total


#-------------------------------
# Generación de nuevas variables
#-------------------------------

# Paso a fecha el atributo created at
#class("account_created_at") #atributo es Character/string
df_c4$Account_created_at = as.Date(df_c4$account_created_at, tz = "UTC",'%Y-%m-%d') 


# Calculo de la antiguedad Cuentas Calculada en Años. 
# Esto lo podemos usar para ver relacion Followers/tweets con Antiguedad
today <- as.Date(Sys.Date(),,'%Y-%m-%d')
df_c4$antiguedad_cuenta =  (today - df_c4$Account_created_at)/365


# Creación de la variable categórica por día 
dias = c( "lunes",  "martes",  "miércoles",  "jueves",  "viernes",  "sábado",  "domingo")
df_c1$cat_nombre_dia = apply(df_c1[c(4)], 1, function(x){return(dias[x[1]])})
table(df_c1$cat_nombre_dia)

barplot(table(df_c1$n_hashtags), main = "Uso de hashtags", xlab = "# hashtags", ylab="Cantidad")

# Elimino los términos de búsqueda de los hashtags
df_c2 = df_c2[-grep(x =  df_c2$cat_hashtags, pattern = "^(covid|corona|cuarentena|casos|Pandemia)"),]








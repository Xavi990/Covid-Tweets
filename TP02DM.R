library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)

#Levanto Dataset original para ver cantidad de Tweets x Dia y uniformidad de los datos
#tweets_v01 = mongo(db = "DMUBA", collection = "tweets_mongo_covid19")

#Armado de los dataframes
#df_tweets_01 = tweets_v01$find()

#Son 443.734 tweets

#df_tweets_01$FechaTweet = as.Date(df_tweets_01$created_at,'%Y-%m-%d') 
#Contamos y ploteamos la Cantidad de Tweets por Día
#TweetsxDia = aggregate(df_tweets_01$FechaTweet,by=list(df_tweets_01$FechaTweet),FUN=length)
#

#la mayoria de los tweets son del 29 y 30 de Mayo (dificultad para ver relacion de dias)

# Libraries para Plotear Series de tiempo de Tweets por dia
library(ggplot2)
library(dplyr)

# Graph1 <- ggplot( data = TweetsxDia, aes( Group.1, x )) +
#   geom_line(colour="blue", linetype=2, size=1.2) + 
#   geom_point(colour="purple", size=3.5, shape=20, fill="white")+
#   scale_x_date(date_breaks="3 day") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   geom_text(aes(label = round(x, 1)),
#             vjust = -0.5, hjust = 0.5, size=4,
#             show.legend = FALSE) +
#   theme(axis.text.x=element_text(angle=90))+
#   theme_bw() + labs(
#     x = "Date",
#     y = "Tweets", 
#     title = "Cantidad de Tweets por Día"
#     
#   ) 

#Graph1

#en el 1er TP teniamos datos hasta el 15 de Mayo, acá se agregaron hasta el 7/06. Pero sin uniformidad





#1ero levantar en MongoDB las collections
#borrar collections del TP anterior
#ir a cmd 
#cd (ruta del bin de Mongo en la pc)
#mongorestore -h localhost --archive=./tweets_covid_curso2020.gz --gzip
#mongorestore -h localhost --archive=./users_covid_curso2020.gz --gzip
#Luego en Robo3t ver las collection y correr el JSON que arma collections usuarios_nhashtgs_dia & usuarios_hashtags


# ---------- Parte Clase Para R  --------------------------

# Nombres de las colecciones que definimos en mongo
c1 = "usuarios_nhashtgs_dia"
c2 = "usuarios_hashtags"

# Conecciones a mongo
usuarios_nhashtgs_dia <- mongo(collection = c1, db = "DMUBA")
usuarios_hashtags <- mongo(collection = c2, db = "DMUBA")

users <- mongo(collection = "backup_usuarios", db = "DMUBA")
tweets <- mongo(collection = "tweets_mongo_covid19", db = "DMUBA")

# Lectura de las colecciones
df_c1 = usuarios_nhashtgs_dia$find(query = '{}',
                                   fields = '{ "_id": false, "user_id" : true, "dia_semana" : true, "n_hashtags": true, "friends_count": true, "followers_count": true, "retweet_favorite_count": true, "retweet_retweet_count": true}')
View(head(df_c1))

df_c2 = usuarios_hashtags$find(query = '{}', fields = '{"_id": false, "user_id" : true, "cat_hashtags" : true}')
View(head(df_c2))

df_user_features = users$find(query = '{}', 
                              fields = '{ "_id": false, "user_id" : true,"account_created_at" : true , "friends_count" : true, "favourites_count":true, "verified": true,"statuses_count": true  }')

#Transformo Account created at

#class("account_created_at") #atributo es Character/string

df_user_features$Account_created_at = as.Date(df_user_features$account_created_at, tz = "UTC",'%Y-%m-%d') 

#1) Antiguedad Cuentas Calculada en Años. Esto lo podemos usar para ver relacion Followers/tweets con Antiguedad
today <- as.Date(Sys.Date(),,'%Y-%m-%d')
df_user_features$antiguedad_cuenta =  (today - df_user_features$Account_created_at)/365

# consulta para joiner cantidad de Tweets por Usuario
#df_cantidadT = tweets$find(query = '{}', fields = '{"user_id" : true, "_id": false, "statuses_count": false }')

#Cantidad de Tweets por Usuario
#tabla_QT = table(df_cantidadT$user_id)
#df_cantidadT2 <- data.frame(user_id=tabla_QT)
#colnames(df_cantidadT2)[1] <- "user_id"
#colnames(df_cantidadT2)[2] <- "Cantidad_Tweets_x_Usuario"

#library(plyr)

#df_user_features = join(df_user_features, df_cantidadT2,      type = "inner")


# Pulir esto

# Elimino los términos de búsqueda de los hashtags (Mejorar)
df_c2 = df_c2[-grep(x =  df_c2$cat_hashtags, pattern = "^(covid|corona|cuarentena|casos)"),]


#

# Creación de la variable categórica día 
dias = c( "lunes",  "martes",  "miércoles",  "jueves",  "viernes",  "sábado",  "domingo")
df_c1$cat_nombre_dia = apply(df_c1[c(4)], 1, function(x){return(dias[x[1]])})
table(df_c1$cat_nombre_dia)

barplot(table(df_c1$n_hashtags), main = "Uso de hashtags", xlab = "# hashtags", ylab="Cantidad")

# Hasta acá con la creacion de variables


# ---------- Discretizaciones  --------------------------
# 1) friends_count ### Mejorar BINS en mas classificaciones
hist(df_c1$friends_count)
hist(log10(df_c1$friends_count))
df_c1$cat_friends = discretize(log10(df_c1$friends_count),method = "fixed", breaks = c(-Inf, 1, 2.5, 3.5, 5, Inf), labels=c("muy pocos","pocos", "medio", "muchos", "muchísimos")) #original c(-Inf, 1, 3, 4, 5, Inf)
table(df_c1$cat_friends)

# 2) followers_count
hist(log10(df_c1$followers_count))
df_c1$cat_followers = discretize(log10(df_c1$followers_count),method = "fixed", breaks = c(-Inf, 1, 3, 4, 5, Inf), labels=c("muy pocos","pocos", "medio", "muchos", "muchísimos")) #original c(-Inf, 1, 3, 4, 5, Inf)
table(df_c1$cat_followers)

# 3) retweet_favorite_count  #### Mehjorar bins de division en base a lo q se ve en el Hist, donde cae patrones etc
hist(df_c1$retweet_favorite_count)
hist(log10(df_c1$retweet_favorite_count))
df_c1$cat_favorite_count = discretize(log10(df_c1$retweet_favorite_count),method = "fixed", breaks = c(-Inf, 1.5, 3.5, Inf), labels=c("bajo", "medio", "alto")) # original 1.3,inf
table(df_c1$cat_retweet_favorite_count)

# 4) retweet_retweet_count
hist(log10(df_c1$retweet_retweet_count))
df_c1$cat_retweet_count = discretize(log10(df_c1$retweet_retweet_count), method = "fixed", breaks = c(-Inf, 1.5, 3, Inf), labels=c("bajo", "medio", "alto")) # original c(-Inf, 1.5, 3, Inf)
table(df_c1$cat_retweet_retweet_count)

View(head(df_c1))

# 5) Antiguedad Cuenta
class(df_user_features$antiguedad_cuenta)
df_user_features$antiguedad_cuenta = as.integer(df_user_features$antiguedad_cuenta)
hist(df_user_features$antiguedad_cuenta)
df_user_features$cat_antiguedad_cuenta = discretize((df_user_features$antiguedad_cuenta), method = "fixed", breaks = c(-Inf, 1, 3, 6, Inf), labels=c("nueva", "media", "antigua", "muy antigua")) 
table(df_user_features$cat_antiguedad_cuenta)

# 6) Actividad en Twitter ( que tanto twitteo en su historia cada usuario)
hist(df_user_features$statuses_count)
hist(log10(df_user_features$statuses_count))
df_user_features$cat_nivel_actividad = discretize(log10(df_user_features$statuses_count), method = "fixed", breaks = c(-Inf, 2.5, 3.5, 4.5, Inf), labels=c("muy bajo", "bajo", "medio", "alto")) 
table(df_user_features$cat_nivel_actividad)

#7)#transformamos verified as tipo factor
df_user_features$cat_verificada =  as.factor(ifelse(df_user_features$verified, "si", NA))

# Rotación de las matriz de datos de usuario y las categóricas
df_tuples_c1 = df_c1 %>% 
  pivot_longer(
    cols =starts_with("cat"),
    names_to = "feat", 
    values_to = "val", 
    names_prefix = "cat_",
    values_drop_na = TRUE) %>% 
  select("user_id", "feat", "val")

head(df_tuples_c1)  

# Rotación de las matriz de datos de usuario y los hashtags
df_tuples_c2 = df_c2 %>% 
  pivot_longer(
    cols =starts_with("cat"),
    names_to = "feat", 
    values_to = "val", 
    names_prefix = "cat_",
    values_drop_na = TRUE) %>% 
  select("user_id", "feat", "val")

head(df_tuples_c2)  

# Rotación de las matriz de datos de usuario y  antiguedad /actividad
df_tuples_c3 = df_user_features %>% 
  pivot_longer(
    cols =starts_with("cat"),
    names_to = "feat", 
    values_to = "val", 
    names_prefix = "cat_",
    values_drop_na = TRUE) %>% 
  select("user_id", "feat", "val")

head(df_tuples_c3)  


# Concateno los 3 data.frames
df_tuples = rbind(df_tuples_c1,df_tuples_c2,df_tuples_c3)

# Se generan los pares TID ITEM (el TID es el user_id)
df_tuples = df_tuples %>% 
  mutate("item" = paste0(feat,"=",val)) %>% 
  select("user_id", "item")

#A cada carrito/tx que son los usuarios veo sus items que son las palabras que uso en el tweet
# Cantidad de transacciones (son los user_id únicos)
length(unique(df_tuples$user_id))

#Verifico que no quedaran items con la palabras redundantes como covid etc.
# 1ero chequeo si quedaron items con esas palabras y despues los elimino
sum(str_count(na.omit(df_tuples$item), "palabra= covid"))
sum(str_count(na.omit(df_tuples$item), "palabra= cuarentena"))
sum(str_count(na.omit(df_tuples$item), "palabra= casos"))
sum(str_count(na.omit(df_tuples$item), "palabra= covid|cuarentena|casos"))


#listar hashtags mas comunes

Ejemplo = aggregate(df_tuples$item,by=list(df_tuples$item),FUN=length)
sort(ejemplo)

ejemplo = df_tuples %>% top_n(100) 


ejemplo= aggregate(cbind(count = item) ~ item, 
          data = df_tuples, 
          FUN = function(x){NROW(x)})

#Reemplazar usando gsub

# Generamos las transacciones en formato Single
trans <- as(split(df_tuples$item, df_tuples$user_id), "transactions")

arules::inspect(trans[101])

# Buscamos reglas con min_sup=0.005 y min_conf=0.5
# Además, se limitan la cantidad de ítems (orden) entre 2 y 9- Ponemos 2 para que no busque relaciones de 1 itemset con su consecuente solamente
rules = apriori(trans, parameter=list(target="rule", support=0.005, confidence=0.5, maxlen=9, minlen=2))
print(rules)

arules::inspect(sort(rules, by="lift", decreasing = TRUE)[1:20])
arules::inspect(head(rules,20))
arules::inspect(sort(rules.filter, by="lift", decreasing = TRUE)[1:100])

plot(rules, measure = c("support", "lift"), shading = "confidence")

                     
# Scatter plot de support vs lift
plot(rules, measure = c("support", "lift"), shading = "confidence")

# Two-Key Plots: Grafica el Orden y las métricas. Order es el k itemset, con cuantos items estan hechas las reglas que encontramos.
#Las de orden 2, son las que tienen los soportes mas altos y conforme empieza a crecer el orden, el soporte baja y estan mas cerca del 0 
plot(rules, method = "two-key plot")



# A las transacciones con hashtags se les agregan los atributos del usuario. Al Carrito usuario y hashtag que usó le agrgo sus caracteristicas
#df_tuples = rbind(df_user_tuples, df_ht) # Y creo nuevas reglas con estas TX



#   Filtros de reglas
# ---------------------
# Utilizo el comando subset y su parámetro (también se llama subset :s )
# Ejemplo: 
#         7 < lift < 10 y  0.04 < support < 0.1
#   quedaría: (lift < 10 & lift > 7) & (support > 0.04 & support < 0.1)
rules.filter = arules::subset(rules, subset = (lift < 20 & lift > 2) & (support > 0.01 & support < 0.1))
print(rules.filter)

arules::inspect(head(rules.filter, 20))

# Scatter plot de support vs lift
plot(rules.filter, measure = c("support", "lift"), shading = "confidence")

# Two-Key Plots: Grafica el Orden y las métricas
plot(rules.filter, method = "two-key plot")
arules::inspect(head(rules.filter, 10))


# Referencias:
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf



### ----- Tratamiento de Textos ------------
df_text = tweets$find(query = '{}',  fields = '{"user_id" : true, "text" : true, "_id": false}')

# Se quitan caracteres no alfanuméricos (por cuestiones de errores en RStudio)
df_text$text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df_text$text)
df_text$text <- gsub("U00..", "", df_text$text)

# --- limpieza de textos
# Se quitan acentos
df_text$text = stri_trans_general(df_text$text, "Latin-ASCII")
# Se pasa a minusculas
df_text$text = tolower(df_text$text)
# Se quita puntuacion
df_text$text = removePunctuation(df_text$text)
# Se quitan números
df_text$text = removeNumbers(df_text$text)
# se quitan espacios extras
df_text$text =  stripWhitespace(df_text$text)
# se quitan espacios al principio y final de la cadena
df_text$text = str_trim(df_text$text)
# sin stop words
df_text$text = removeWords(df_text$text, stopwords("spanish"))
df_text$text = removeWords(df_text$text, ((covid|coronav|cuarentena|covid19|coronavirus|virus)))

# se separa el texto en términos
#armamos vector en donde tokenaze agarra el texto y arma un vector por usuario con todas las palabras y armamos fila por usuario por palabra
df_text$words = tokenizers::tokenize_words(df_text$text, simplify = T)
# se pasa a formato pares: user-término
df_text = df_text %>% select("user_id", "words")  %>% unnest(words) %>%  distinct()
# se agrega prefijo de tipo de ítem:
df_text$item = paste0("word=", df_text$words)

# reglas
trans <- as(split(df_text$item, df_text$user_id), "transactions")
print(trans)
rules = apriori(trans, parameter=list(target="rule", support=0.005, confidence=0.2))
print(rules)
#Abolsute Mininm SUpport COunt: 1415 - Es decir no vamos a usar reglas que no hayan sido utilizadas por 1415 usuarios diferentes (soporte del 0.005) rules =
View(arules::inspect(sort(rules, by="lift", decreasing = TRUE)[1:50]))
arules::inspect(head(rules, 20))



### ----- Filtros para ir Buscando Reglas ------------####
length(df_tuples$user_id)

#df_tuplesfiltered = df_tuples[-grep(x =  df_tuples$item, pattern = "^(followers=pocos|followers=muy pocos)"),]
#filtro para popularidad CUENTAS
#df_tuplesfiltered = df_tuples[-grep(x =  df_tuples$item, pattern = "^(followers=pocos|followers=muy pocos|followers=medio|retweet_count=bajo)"),]
#filtro para Popularidad de los TWEETS
df_tuplesfiltered = df_tuples[-grep(x =  df_tuples$item, pattern = "^(followers=pocos|followers=muy pocos|retweet_count=bajo|favourites_count=bajo)"),]
length(df_tuplesfiltered$user_id)

trans <- as(split(df_tuplesfiltered$item, df_tuplesfiltered$user_id), "transactions")

arules::inspect(trans[101])

rules = apriori(trans, parameter=list(target="rule", support=0.005, confidence=0.5, maxlen=9, minlen=2))
print(rules)
arules::inspect(rules)
arules::inspect(sort(rules, by="lift", decreasing = TRUE)[1:200])

---------------
#filter para Popularidad Cuentas
plot(rules, measure = c("support", "lift"), shading = "confidence")

  
rules.filter = arules::subset(rules, subset = (lift < 10 & lift > 3) & (support > 0.03 & support < 0.1))
print(rules.filter)

arules::inspect(head(rules.filter, 20))
arules::inspect(sort(rules.filter, by="lift", decreasing = TRUE)[1:9])

#filter para Popularidad Tweets
plot(rules, measure = c("support", "lift"), shading = "confidence")

rules.filter = arules::subset(rules, subset = (lift < 20 & lift > 1) & (support > 0.009 & support < 0.1))
print(rules.filter)

arules::inspect(head(rules.filter, 20))
arules::inspect(sort(rules.filter, by="lift", decreasing = TRUE)[1:18])


# Scatter plot de support vs lift
plot(rules, measure = c("support", "lift"), shading = "confidence")

# Two-Key Plots: Grafica el Orden y las métricas
plot(rules, method = "two-key plot")
arules::inspect(head(rules, 10))
# Two-Key Plots: Grafica el Orden y las métricas. Order es el k itemset, con cuantos items estan hechas las reglas que encontramos.
#Las de orden 2, son las que tienen los soportes mas altos y conforme empieza a crecer el orden, el soporte baja y estan mas cerca del 0 




#############################################################
# Buscar que el consecuente contenga items de tipo hashtag
# (%pin% indica matching parcial sobre el string del item)
x= subset(rules, subset = rhs  %pin% "hashtags=")
arules::inspect(x)

# Buscar que el consecuente contenga el hashtag chile
x = subset(rules, subset = rhs  %in% "hashtags=obelisco")
x = subset(rules, subset = lhs  %in% "hashtags=nasa")
arules::inspect(x)

# Buscar que el antecedente contenga el hashtag chile O cuarentena
x = subset(rules, subset = lhs  %in% c("hashtags=méxico", "hashtags=pandemia"))
arules::inspect(x)

# Buscar que el antecedente contenga el hashtag chile Y cuarentena
subset(rules, subset = lhs  %ain% c("hashtags=argentina", "hashtags=chile"))

# Buscar que el antecedente contenga algún hashtag y en el consecuente
# la cantidad de amigos discretizada
x= subset(rules, subset = lhs  %pin% "hashtags=" & rhs  %pin% "friends=")
arules::inspect(x)
arules::inspect(sort(rules.filter, by="lift", decreasing = TRUE)[1:44])


# Buscar que el antecedente contenga algún hashtag O
# en el consecuente la cantidad de amigos discretizada
x = subset(rules, subset = lhs  %pin% "hashtags=" | rhs  %pin% "friends=")
arules::inspect(head(x,20))
arules::inspect(sort(rules.filter, by="lift", decreasing = TRUE)[1:100])

# Buscar que el antecedente contenga algún hashtag y que 
# el consecuente NO contenga el hashtag coronavirus
x = subset(rules, subset = lhs  %pin% "hashtags=" & !(rhs  %in% "hashtags=nasa"))
arules::inspect(head(x,20))

# Buscar que el itemset (antecedente o concecuente) tenga un hashtag
subset(rules, subset = items  %pin% "hashtags=")
arules::inspect(head(x,20))

# Buscar que el itemset (antecedente o concecuente) tenga un hashtag Y que el lift sea mayor a 3
x= subset(rules, subset = items  %pin% "hashtags=" & lift > 3)
arules::inspect(head(x,20))

# 

rm(list = ls())


dir_root = "C:/Users/javier.gamboa/OneDrive - Accenture/Me/DS UBA/02 - Data Mining/0 - TP1/"

install.packages("mongolite")
library(mongolite)


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

class(df_hashtags_usados)



#Date Time para Campo creacion Cuenta y creacion Tweet para poder ver variaciones en el tiempo
install.packages("lubridate")
library(lubridate)


class("account_created_at") #atributo es Character/string

df_usuarios$FechaAccount_created_at = as.Date(df_usuarios$account_created_at, tz = "UTC",'%Y-%m-%d') 

#1) Antiguedad Cuentas Calculada en Años. Esto lo podemos usar para ver relacion Followers/tweets con Antiguedad
today <- as.Date(Sys.Date(),,'%Y-%m-%d')
df_usuarios$AntiguedadAccount =  (today - df_usuarios$FechaAccount_created_at)/365
print(df_usuarios$AntiguedadAccount)

#df_tweets_01[order(as.Date(df_tweets_01$FechaAccount_created_at, format="%Y-%m-%d")),]

#dplyr::arrange(df_tweets_01, AntiguedadAccount)


df_tweets_01$FechaTweet = as.Date(df_tweets_01$created_at,'%Y-%m-%d') 

#Contamos y ploteamos la Cantidad de Tweets por Día
TweetsxDia = aggregate(df_tweets_01$FechaTweet,by=list(df_tweets_01$FechaTweet),FUN=length)
TweetsxDia


# Libraries para Ploter Series de tiempo de Tweets por dia
library(ggplot2)
library(dplyr)

Graph1 <- ggplot( data = TweetsxDia, aes( Group.1, x )) +
  geom_line(colour="blue", linetype=2, size=1.2) + 
  geom_point(colour="purple", size=3.5, shape=20, fill="white")+
  scale_x_date(date_breaks="1 day") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_text(aes(label = round(x, 1)),
            vjust = 0, hjust = -0.5, size=3,
            show.legend = FALSE) +
  theme(axis.text.x=element_text(angle=90))+
  theme_bw() + labs(
  x = "Date",
  y = "Tweets", 
  title = "Cantidad de Tweets por Día"
  
) 
Graph1


?plot()
install.packages('stats')
Plot(TweetsxDia$Group.1, TweetsxDia$Group.x)

plot(TweetsxDia$Group.1, TweetsxDia$x, main="Tweets por Día", xlab = "Date", ylab = "Amount", type = "l", col="blue", linetype="dashed")
text(TweetsxDia$x, TweetsxDia$Group.1, row.names(TweetsxDia), cex=0.6, pos=4, col="red")

?ggplot








#Analisis de Palabras mas usadas en Campo Text que no fueron Hashtags del Covid


install.packages("tm")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("SnowballC")
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(tm)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("tidytext")
library(tidytext)

##Unnest the words - code via Tidy Text
hmtTable <- df_tweets_01 %>% 
  unnest_tokens(word, text)
#remove stop words - aka typically very common words such as "the", "of" etc
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

data(stop_words)
hmtTable <- hmtTable %>%
  anti_join(custom_stop_words)

#do a word count
hmtTable <- hmtTable %>%
  count(word, sort = TRUE) 
hmtTable 


#Remove other nonsense words o repetidas en Hashtags
hmtTable <-hmtTable %>%
  filter(!word %in% c('t.co', 'https', 'handmaidstale', "handmaid's", 'season', 'episode', 'de', 'handmaidsonhulu',  'tvtime', 'va','1','3','5','mientras','cada','nadie','cómo','vía','voy','mas', 
                      'watched', 'watching', 'watch', 'la', "it's", 'el', 'en', 'tv', 'después','2020','siempre','coronavirus','puede','solo','hace','hacer','hecho','años', 'vez','da','san','mil','va','año', '15','mañana',
                      'je', 'ep', 'week', 'amp', 'covid', '19', 'covid19','hoy','si','gente','ahora','nuevos','bien','2','días','quiero','toda','todas','casi','gran','tras','van','tener','cosas','dar','video','debe','decir','plena','30',
                      'medio','quiere','mal','ser','según','alguien','20','dos','caso','tan','nunca','ir','ver','parte','frente','sigue','mismo','4','24','10','hizo','manuel','así','día','mejor','ー','aquí','menos','dice','nueva','rt','aún'))

#select top 100 de las palabras más usadas en campos Text
hmtTable = hmtTable %>% top_n(100) 

print(typeof(hmtTable$n))


#Nube de Palabras, de las palabras más usadas en los tweets en el campo Text
wordcloud2(hmtTable, size=2.0, minSize = 0, gridSize = 0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

#WordCloud del Campo Hashtags
class (hmtTable)
class (Cloud2)
Cloud2 = df_hashtags_final

Cloud2 = top_n(Cloud2, 100, cant)
print(typeof(Cloud2$count))
Cloud2$count = as.integer(Cloud2$cant) 
print(typeof(Cloud2$cant))


#wordcloud2(Cloud2, size=2.0, minSize = 1, gridSize = 0) # No me anduvo, no puedo corregir el error
wordcloud(Cloud2$hashtag,Cloud2$cant,scale=c(2,.8),min.freq=3,max.words=Inf,
          random.order=TRUE, random.color=FALSE, rot.per=.1,
          colors="red",ordered.colors=TRUE,use.r.layout=FALSE,
          fixed.asp=TRUE)

par(bg="grey35")
par(mar = rep(0, 4))
wordcloud(Cloud2$hashtag, Cloud2$cant, col=terrain.colors(length(Cloud2$hashtag), alpha=0.9), random.order=FALSE, rot.per=0.5,size=10.0, font = 2,scale=c(8,1) )

#rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), and cm.colors(n)



#Otro Grafico Prueba Pajarito
#figPath1 = system.file("C:/Users/javier.gamboa/OneDrive - Accenture/Me/DS UBA/02 - Data Mining/0 - TP1/t.png",package = "wordcloud2")
#wordcloud2(Cloud2, figPath = figPath1, size = 0.1,color = "skyblue")

#Imputar Hashtags a Missing Values, Segun palabras en text tweet

gc()

library(tm)
library(mongolite)
library(stringr)
library(stringi)
library(wordcloud)

# Consulta de usuarios con información textual en el campo Text
df_tweets_HT = tweets_v01$find(query = '{"text": {"$exists": true}}', 
                      fields = '{"user_id" : true, "text" : true}')

# Se quitan caracteres no alfanuméricos
df_tweets_HT$text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df_tweets_HT$text)
df_tweets_HT$text <- gsub("U00..", "", df_tweets_HT$text)

df_tweets_HT$text_original = df_tweets_HT$text

# --- limpieza de textos
# Se quitan acentos
df_tweets_HT$text = stri_trans_general(df_tweets_HT$textt, "Latin-ASCII")
# Se pasa a minusculas
df_tweets_HT$text = tolower(df_tweets_HT$text)
# Se quita puntuacion
df_tweets_HT$text = removePunctuation(df_tweets_HT$text)
# Se quitan números
df_tweets_HT$text = removeNumbers(df_tweets_HT$text)
# se quitan espacios extras
df_tweets_HT$text =  stripWhitespace(df_tweets_HT$text)
# se quitan espacios al principio y final de la cadena
df_tweets_HT$text = str_trim(df_tweets_HT$text)

# sin stop words
df_tweets_HT$text_nosw = removeWords(df_tweets_HT$text, stopwords("spanish"))

# ¿Qué contiene el campo Text? Podemos contar frecuencias de palabras
corpus = Corpus(VectorSource(enc2utf8(df_tweets_HT$text)))
inspect(corpus[2])

dtm <- TermDocumentMatrix(corpus)
dtm

###

#armar nueva lista de Palabras mas usadas incluyendo Covid etc para imputar en columna nuevos hash
##Unnest the words - code via Tidy Text
hmtTable2 <- df_tweets_01 %>% 
  unnest_tokens(word, text)
#remove stop words - aka typically very common words such as "the", "of" etc
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

data(stop_words)
hmtTable2 <- hmtTable2 %>%
  anti_join(custom_stop_words)

#do a word count
hmtTable2 <- hmtTable2 %>%
  count(word, sort = TRUE) 
hmtTable2


#Remove other nonsense words o repetidas en Hashtags
hmtTable2 <-hmtTable2 %>%
  filter(!word %in% c('t.co', 'https', 'handmaidstale', "handmaid's", 'season', 'episode', 'de', 'handmaidsonhulu',  'tvtime', 'va','1','3','5','mientras','cada','nadie','cómo','vía','voy','mas', 
                      'watched', 'watching', 'watch', 'la', "it's", 'el', 'en', 'tv', 'después','2020','siempre','puede','solo','hace','hacer','hecho','años', 'vez','da','san','mil','va','año', '15','mañana',
                      'je', 'ep', 'week', 'amp', '19', 'hoy','si','gente','ahora','nuevos','bien','2','días','quiero','toda','todas','casi','gran','tras','van','tener','cosas','dar','video','debe','decir','plena','30',
                      'medio','quiere','mal','ser','según','alguien','20','dos','caso','tan','nunca','ir','ver','parte','frente','sigue','mismo','4','24','10','hizo','manuel','así','día','mejor','ー','aquí','menos','dice','nueva','rt','aún'))

#select top 100 de las palabras más usadas en campos Text
hmtTable2 = hmtTable2 %>% top_n(100) 



# Del listado de las palabras mas usadas en el campo TExt armo nueva columna de Hash, si encuentra esa palabra en el tweet

countries_regex = paste0("(", paste0(hmtTable2$word, collapse = '|'), ")")
# Usamos expresiones regulares para extraer el país
df_tweets_01$NewHash = str_extract(df_tweets_01$text, countries_regex)





#countries_regex2 = paste0("(", paste0(hmtTable$word, collapse = '|'), ")")
#df_tweets_01$HashUnido = df_tweets_01$hashtags
#df_tweets_01$HashUnido = str_extract(df_tweets_01$hashtags, countries_regex)

text_df = select(text_df, -HashUnido)


#creo Nueva variable con cada hashtag separado por usuario

text_df <- mutate(df_tweets_01, text = as.character(df_tweets_01$hashtags))

text_df = text_df %>% 
  unnest_tokens(UniqueTags, text)


#Armo una Atributo unico de Hash. Trae el de la columna Hash o le asigna uno, si hubiese escrito en campo "text" una de las 100 palabras mas usadas 
text_df$Combined <- ifelse(text_df$UniqueTags !='na', text_df$UniqueTags, text_df$NewHash)
text_df = subset(text_df, Combined!='c')

#pasarle limpieza de Covids
lista   = grep("covid",   tolower(df_hashtags_usados$hashtag))
text_df$Combined = replace(text_df$Combined, lista, "COVID-19")



#summary(as.factor(is.na(df_tweets_01$hashtags)))
#summary(as.factor(is.na(text_df$Combined)))
#count(text_df)





#Analizamos cuantos Hash Imputamos sobre el total comparada al original
df_tweets_01$HashOverview <- ifelse(df_tweets_01$hashtags !='NA', df_tweets_01$hashtags, df_tweets_01$NewHash)

#Original
summary(as.factor(is.na(df_tweets_01$hashtags)))

#Nuevo con Imputaciones

summary(as.factor(is.na(df_tweets_01$HashOverview)))

#plotear una torta o algo con los nulos que imputamos
#Porcentaje de Paises nulos
factor_hash_nulos <- summary(as.factor(is.na(df_tweets_01$hashtags))) 
freq_hash_nulos = table(is.na(df_tweets_01$hashtags))
df_hash_nulos = as.data.frame(freq_hash_nulos)


hnonulos   = freq_hash_nulos[1]/(freq_hash_nulos[1]+freq_hash_nulos[2])*100
hnulos = freq_hash_nulos[2]/(freq_hash_nulos[1]+freq_hash_nulos[2])*100

categorias1  <- c("Nulos", "No nulos")
porcentajes1 <- c(hnulos, hnonulos)
df10 <- data.frame(categorias1, porcentajes1)

ggplot(df10,mapping = aes(x="",y=porcentajes1, fill=categorias1))+
  geom_bar(stat = "identity",color="white")

ggplot(df10,aes(x="",y=porcentajes1, fill=categorias1))+
  geom_bar(stat = "identity",color="white")+
  coord_polar(theta="y")

#Ploteamos como queda con la nueva columna de Hashtags
factor_hash_nulos2 <- summary(as.factor(is.na(df_tweets_01$HashOverview))) 
freq_hash_nulos2 = table(is.na(df_tweets_01$HashOverview))
df_hash_nulos2 = as.data.frame(freq_hash_nulos2)


hnonulos2   = freq_hash_nulos2[1]/(freq_hash_nulos2[1]+freq_hash_nulos2[2])*100
hnulos2 = freq_hash_nulos2[2]/(freq_hash_nulos2[1]+freq_hash_nulos2[2])*100

categorias2  <- c("Nulos", "No nulos")
porcentajes2 <- c(hnulos2, hnonulos2)
df20 <- data.frame(categorias2, porcentajes2)

ggplot(df20,mapping = aes(x="",y=porcentajes2, fill=categorias2))+
  geom_bar(stat = "identity",color="white")

ggplot(df20,aes(x="",y=porcentajes1, fill=categorias1))+
  geom_bar(stat = "identity",color="white")+
  coord_polar(theta="y")

#Conclusión: vemos que invertimos proporcion

#Nube de Palabras de Hashs combinados para ver si cambia algo
hmtTable3 <- text_df %>% 
  unnest_tokens(word, Combined)
#remove stop words - aka typically very common words such as "the", "of" etc
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

data(stop_words)
hmtTable3 <- hmtTable3 %>%
  anti_join(custom_stop_words)

#do a word count
hmtTable3 <- hmtTable3 %>%
  count(word, sort = TRUE) 
hmtTable3 


#Remove other nonsense words o repetidas en Hashtags
hmtTable <-hmtTable %>%
  filter(!word %in% c('t.co', 'https', 'handmaidstale', "handmaid's", 'season', 'episode', 'de', 'handmaidsonhulu',  'tvtime', 'va','1','3','5','mientras','cada','nadie','cómo','vía','voy','mas', 
                      'watched', 'watching', 'watch', 'la', "it's", 'el', 'en', 'tv', 'después','2020','siempre','coronavirus','puede','solo','hace','hacer','hecho','años', 'vez','da','san','mil','va','año', '15','mañana',
                      'je', 'ep', 'week', 'amp', '19','hoy','si','gente','ahora','nuevos','bien','2','días','quiero','toda','todas','casi','gran','tras','van','tener','cosas','dar','video','debe','decir','plena','30',
                      'medio','quiere','mal','ser','según','alguien','20','dos','caso','tan','nunca','ir','ver','parte','frente','sigue','mismo','4','24','10','hizo','manuel','así','día','mejor','ー','aquí','menos','dice','nueva','rt','aún'))

#select top 100 de las palabras más usadas en campos Text
hmtTable3 = hmtTable3 %>% top_n(100) 

wordcloud2(hmtTable3, size=2.0, minSize = 0, gridSize = 0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)


#Plot Tweets Combined x Fecha/Location

library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
#ggplot(text_df,aes(x=FechaTweet,group=pais,fill=pais))+
 # geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

TweetsxDiaXLocation = aggregate(df_tweets_01$FechaTweet,by=list(df_tweets_01$FechaTweet,df_tweets_01$pais),FUN=length)
TweetsxDiaXLocation


TweetsxDiaXLocation = dplyr::filter(TweetsxDiaXLocation, grepl('Argentina|Colombia|España|Mexico', Group.2))

#Plot Hashs  x Fecha o Location & Combined

#Armo df HashxDia con Fecha, hash Y cantidad. Me quedo con los 100 mas frecuentes
HashxDia = aggregate(text_df$FechaTweet,by=list(text_df$FechaTweet,text_df$Combined),FUN=length)
HashxDia = subset(HashxDia, Group.2!='ー')
HashxDia = HashxDia %>% top_n(30) 

#Standarizado Covids
library(plyr)
HashxDia$Group.2 = revalue(HashxDia$Group.2, c("covid"="covid19","19"="covid19",'covid__19'="covid19", 'COVID-19'="covid19"	 ))

#Plot de hashxDia
# stacked bar chart; Barplot of the count
library(ggplot2)

class(HashxDia$Group.1)

gg = ggplot(HashxDia, aes(x = Group.1, y = x, group = Group.2, fill = Group.2)) + 
  geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_breaks="1 day") +
  labs(
    x = "Fecha",
    y = "Trending Hashtags", 
    title = "Hashtags más populares por Día",
    fill = 'Hashtags'
    
  ) 
plot(gg)


#Armo df con Pais, hash Y cantidad.Me quedo con los 100 mas frecuentes

HashxPais = aggregate(text_df$FechaTweet,by=list(text_df$pais,text_df$Combined),FUN=length)
HashxPais = dplyr::filter(HashxPais, grepl('Argentina|Colombia|España|Mexico', Group.1))
HashxPais = subset(HashxPais, Group.2!='ー')
HashxPais = subset(HashxPais, Group.2!='u')
HashxPais = HashxPais %>% top_n(30) 

#HashxPais <- ifelse(HashxPais$Group.2 =='19', 'covid19', HashxPais$Group.2)
#HashxPais$Group.2[HashxPais$Group.2 == ("19|covid")] <- "covid19"

#Standarizado Covids
library(plyr)

HashxPais$Group.2 = revalue(HashxPais$Group.2, c("covid"="covid19","19"="covid19" ))

#Ploteado HashxPais
gg = ggplot(HashxPais, aes(x = Group.1, y = x, group = Group.2, fill = Group.2)) + 
  geom_bar(stat = "identity")+
  labs(
    x = "País",
    y = "Trending Hashtags", 
    title = "Hashtags más populares por País",
    fill = 'Hashtags'
    
  ) 
plot(gg)


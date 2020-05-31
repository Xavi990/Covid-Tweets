
rm(list = ls())


dir_root = "C:/Users/javier.gamboa/OneDrive - Accenture/Me/DS UBA/02 - Data Mining/0 - TP1/"




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

#Antiguedad Cuentas Calculada en Años
today <- as.Date(Sys.Date(),,'%Y-%m-%d')
df_usuarios$AntiguedadAccount =  (today - df_usuarios$FechaAccount_created_at)/365
print(df_usuarios$AntiguedadAccount)

#df_tweets_01[order(as.Date(df_tweets_01$FechaAccount_created_at, format="%Y-%m-%d")),]

#dplyr::arrange(df_tweets_01, AntiguedadAccount)


df_tweets_01$FechaTweet = as.Date(df_tweets_01$created_at,'%Y-%m-%d') 


TweetsxDia = aggregate(df_tweets_01$FechaTweet,by=list(df_tweets_01$FechaTweet),FUN=length)
TweetsxDia


# Libraries para Ploter Series de tiempo de Tweets por dia
library(ggplot2)
library(dplyr)

Graph1 <- ggplot( data = TweetsxDia, aes( Group.1, x )) +
  theme_bw()+ geom_line() + labs(
  x = "Date",
  y = "Tweets"
  
) 
Graph1



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


#Remove other nonsense words o repetidas
hmtTable <-hmtTable %>%
  filter(!word %in% c('t.co', 'https', 'handmaidstale', "handmaid's", 'season', 'episode', 'de', 'handmaidsonhulu',  'tvtime', 'va','1','3','5','mientras','cada','nadie','cómo','vía','voy','mas', 
                      'watched', 'watching', 'watch', 'la', "it's", 'el', 'en', 'tv', 'después','2020','siempre','coronavirus','puede','solo','hace','hacer','hecho','años', 'vez','da','san','mil','va','año', '15','mañana',
                      'je', 'ep', 'week', 'amp', 'covid', '19', 'covid19','hoy','si','gente','ahora','nuevos','bien','2','días','quiero','toda','todas','casi','gran','tras','van','tener','cosas','dar','video','debe','decir','plena','30',
                      'medio','quiere','mal','ser','según','alguien','20','dos','caso','tan','nunca','ir','ver','parte','frente','sigue','mismo','4','24','10','hizo','manuel','así','día','mejor','ー','aquí','menos','dice','nueva','rt','aún'))

#select top 100
hmtTable = hmtTable %>% top_n(100) 

print(typeof(hmtTable$n))


#Create a World Cloud
wordcloud2(hmtTable, size=2.0, minSize = 0, gridSize = 0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

#WordCloud de Campo Hastags
class (hmtTable)
class (Cloud2)
Cloud2 = df_hashtags_usados

Cloud2 = top_n(Cloud2, 100, count)
print(typeof(Cloud2$count))
Cloud2$count = as.integer(Cloud2$count) 
print(typeof(Cloud2$count))


#wordcloud2(Cloud2, size=2.0, minSize = 0, gridSize = 0)# No me anduvo, no puedo corregir el error
wordcloud(Cloud2$hashtag,Cloud2$count,scale=c(2,.8),min.freq=3,max.words=Inf,
          random.order=TRUE, random.color=FALSE, rot.per=.1,
          colors="red",ordered.colors=TRUE,use.r.layout=FALSE,
          fixed.asp=TRUE)


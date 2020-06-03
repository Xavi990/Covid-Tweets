#
# Exploracion de Hashtags
#

summary(as.factor(is.na(df_tweets_01$hashtags)))
summary(as.factor(df_tweets_01$verified))

#
# Unificacion de Hashtags
#
df_hashtags_usados$OK <- df_hashtags_usados$hashtag
colnames(df_hashtags_usados) <- c("cantidad", "OLD", "hashtag")


df_hashtags_usados$hashtag <- chartr("áéíóúü","aeiouu",df_hashtags_usados$hashtag)
df_hashtags_usados$hashtag <- iconv(df_hashtags_usados$hashtag,'utf-8','ascii', sub = '')
df_hashtags_usados$hashtag <- tolower(df_hashtags_usados$hashtag)


lista   = grep("covid",   tolower(df_hashtags_usados$hashtag))
df_hashtags_usados$hashtag = replace(df_hashtags_usados$hashtag, lista, "COVID-19")

lista   = grep("corona",   tolower(df_hashtags_usados$hashtag))
df_hashtags_usados$hashtag = replace(df_hashtags_usados$hashtag, lista, "Coronavirus")

# Resumarizo los valores
install.packages("sqldf")
library(sqldf)

df_hashtags_final <- sqldf("select hashtag, sum(cantidad) cant 
         from  df_hashtags_usados 
         group by hashtag
         order by cant desc")



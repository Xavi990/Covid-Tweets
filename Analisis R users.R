####### EXPLORANDO TP01 ########
rm(list=ls())
library(mongolite)

tweets01=mongo(collection='users_mongo_covid19',db='DMUBA')
dftweets=tweets01$find('{}')

df2=data.frame(dftweets$followers_count, 
               dftweets$friends_count, 
               dftweets$statuses_count, 
               dftweets$favourites_count, 
               dftweets$protected,
               dftweets$verified,
               #dftweets$is_retweet,
               #dftweets$is_quote,
               dftweets$account_created_at)
               #dftweets$retweet_followers_count)


names(df2)=c('followers',
             'friends',
             'statuses',
             'favorites',
             'protegidas',
             'verificadas',
             #'es_retweet',
             #'es_quote',
             'creacion_cuenta')
             #'followers_retweet')

df2$verificadas2=df2$verificadas
df2$verificadas2[df2$verificadas==FALSE]='No Verificadas'
df2$verificadas2[df2$verificadas==TRUE]='Verificadas'
df2$verificadas2=as.factor(df2$verificadas2)



# Medias, Medianas y Moda para las variables:

# followers_count # number of followers of the User (seguidores)
# friends_count # number of counts the user is Following (seguidos)
# statuses_count # number of tweets issued by the user
# favourites_count # Distinta de la primera # The number of Tweets this user has liked in the account’s lifetime

sum(is.na(df2)) # no hay NA

medias=apply(df2, 2, mean, na.rm=TRUE)
medias

medianas=apply(df2, 2, median, na.rm=TRUE)
medianas


# GRAFICOS DE BARRAS

# protected

table(df2$protegidas) # son todas No protegidas las cuentas, no hago mas analisis

# Cuentas verificadas 

tabla3=as.data.frame(table(df2$verificadas))
names(tabla3)=c("Verificada", "Cantidad")
tabla3$Verificada=c('No Verificada', 'Verificada')

verificadas=round(sum(df2$verificadas==FALSE)/nrow(df2)*100,1)
no.verificadas=round(sum(df2$verificadas==TRUE)/nrow(df2)*100,1)

barplot(table(df2$verificadas)/nrow(df2)*100, 
        names.arg = c('No Verificadas', 'Verificadas'), 
        ylab='Porcentaje de las cuentas', 
        col=c('red','pink'), 
        ylim=c(0,100), 
        legend=c(verificadas, no.verificadas),
        main = 'Cuentas verificadas vs No Verificadas por Twitter')


library(dplyr)

prop3 = tabla3$Cantidad / sum(tabla3$Cantidad) *100
pos3 = cumsum(prop)- 0.5*prop

ggplot(tabla3, aes(x="", y=Cantidad, fill=Verificada)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_classic(base_size=50)+
  theme(legend.position="none") +
  geom_text(size=10, aes(label = paste0(round(prop3), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(size=10, x = NULL, y = NULL, fill = NULL, 
       title = "Cuentas Verificadas", size=10)+
  scale_fill_brewer(palette="Set1")+
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))


pie(tabla3$Cantidad, labels = paste(round(prop3,1), "%"), cex=2, main = "Cuentas Verificadas", cex.main=2, col = rainbow(length(tabla3)))
legend("topright", c("No Verificada","Verificada"), cex = 1.6,
       fill = rainbow(length(tabla3)))


# Histogramas de Followers count

min(df2$followers) #min 0
max(df2$followers) #max 18.609.108 18 millones

log(min(df2$followers)+0.1) #-2.302
log(max(df2$followers)+0.1) # 16

hist(df2$followers)
min(df2$followers) # 0
max(df2$followers) # 18.6
hist(log(df2$followers))
hist(log(df2$followers),
     main="Frecuencia de Cantidad de Seguidores de cada Cuenta",
     xlab="Logaritmo de Cantidad de Seguidores",
     col="darkmagenta",
     freq=TRUE,
     ylab='Cantidad de Usuarios'
)



boxplot(df2$followers)
boxplot(log(df2$followers))

# Boxplot de Cantidad de seguidores y agrupados por Verificadas y no Verificadas  

boxplot(log(df2$followers),
        main = "Cantidad de seguidores Cuentas Verificadas y No Verificadas",
        yaxt='n',
        ylab = "",
        xlab = "Logaritmo de Cantidad de Seguidores",
        widht=0.9,
        at = c(1),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

boxplot(log(df2$followers)~df2$verificadas,
        main = "Cantidad de seguidores Cuentas Verificadas y No Verificadas",
        yaxt='n',
        ylab = "No Verificadas                   Verificadas",
        xlab = "Logaritmo de Cantidad de Seguidores",
        widht=0.9,
        at = c(1,2),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

# Plot de cantidad de followers pintando colores por si la cuenta es verificada o no

plot(log(sort(df2$followers)))
plot(log(sort(df2$friends)))
plot(df2$followers, df2$friends)
plot(log(df2$followers), log(df2$friends))


plot((df2$followers), (df2$friends),
     log='xy',
     col=df2$verificadas2,
     pch=16,
     cex=1,
     xlab='Cantidad de Followers (log)',
     ylab='Cantidad de Amigos (log)',
     main='Followers y Amigos de las cuentas')

legend(x=1, y=1e5, legend=levels(df2$verificadas2), col=c(1:2), pch=16)


# Scatterplots para ver correlaciones

# Cantidad de seguidores vs Retweets

plot((df2$followers), (df1$retweets),
     col=df2$verificadas2,
     pch=16,
     cex=1,
     xlab='Cantidad de Followers (log)',
     ylab='Cantidad de Amigos (log)',
     main='Followers y Amigos de las cuentas')
# este plot no sirve, porque hay pocos datos respecto a los retweets


# algunas correlaciones

# Cantidad de Tweets del usuario vs Antiguedad de la cuenta
# creo una variable para determinar la antiguedad de la cuenta

library(zoo)
install.packages('lubridate')
library(lubridate)

df2$creacion_cuenta= as.Date(dftweets$account_created_at, tz = "UTC",'%Y-%m-%d') 
today <- as.Date(Sys.Date(),'%Y-%m-%d')
df2$antiguedad= (today - df2$creacion_cuenta)/30.4375 # antiguedad en meses
plot(sort(df2$antiguedad))
min(df2$antiguedad) # 0.68 meses 
max(df2$antiguedad) # 159.63 meses 

# cantidad de tweets vs antiguedad de la cuenta

plot(sort(df2$statuses))
boxplot(log(df2$statuses))
plot((df2$statuses), (df2$antiguedad),
     log='xy',
     xlim=c(1,1e7),
     pch=16,
     cex=1,
     xlab='Cantidad de Tweets en la historia del Usuario (log)',
     ylab='Antiguedad cuenta (log)',
     main='Cantidad de Twets y antiguedad de las cuentas')

# Matriz de correlacion entre estas variables
cor(df2$statuses, as.numeric(df2$antiguedad)) #0.16 bastante bajo

# QUe pasa si limpio un poco los Outliers? Los saco con metodo IQR
# primero genero un df auxiliar
dfclean=data.frame(df2$statuses, df2$antiguedad)
names(dfclean)=c('statuses', 'antiguedad')

q3=quantile(dfclean$statuses,0.75)
q1=quantile(dfclean$statuses, 0.25)
IQR=q3-q1
outliers.max=q3+1.5*IQR # Seteo limite superior
outliers.min=q1-1.5*IQR # Seteo limite inferior

# Grafaico plot quitando Outliers

plot((dfclean$statuses[dfclean$statuses<outliers.max & dfclean$statuses2>outliers.min]), 
     (dfclean$antiguedad[dfclean$statuses<outliers.max & dfclean$statuses2>outliers.min]),
     log='xy',
     xlim=c(1,1e7),
     pch=16,
     cex=1,
     xlab='Cantidad de Tweets en la historia del Usuario (log)',
     ylab='Antiguedad cuenta (log)',
     main='Cantidad de Twets y antiguedad de las cuentas')

plot(sort(ruidoso$Road_55dB[ruidoso$zscoremod<3.5 & ruidoso$zscoremod>-3.5]), xlab='', ylab='Datos', main='Sin Outliers')

# Grafico plot con y sin outliers. El limite es un z>3.5 (modulo)

# no tiene mucho sentido esto...

# cantidad de seguidores vs antiguedad de la cuenta

plot((df2$followers), (df2$antiguedad),
     log='xy',
     xlim=c(1,1e7),
     pch=16,
     cex=1,
     xlab='Cantidad de Followers del Usuario (log)',
     ylab='Antiguedad cuenta (log)',
     main='Followers y Antiguedad de las cuentas')


# cantidad de tweets agrupados por si las cuentas son verificadas o no

plot(sort(df2$statuses),
     log='x',
     xlim=c(1,1e7),
     col=df2$verificadas2,
     pch=16,
     cex=1,
     xlab='Cantidad de Tweets en la historia del Usuario (log)',
     ylab='',
     main='Cantidad de Twets en la historia de las cuentas')
# nada interesante..

# Los tweets que son retweets, tienen alto numero de retweets_followers_count? 
#(osea que el usuario retweteado tiene muchos followers?)

mean(df2$followers) # 17749

plot(sort(df2$followers_retweet[df2$es_retweet==TRUE]),
     col='red',
     pch=16,
     cex=1,
     xlab='Cantidad de Followers de los Usuarios cuyos tweets son Retweitteados',
     ylab='',
     main='Cantidad de Twets en la historia de las cuentas')
points(x=17749)

# ninguna conclusion...


######## ATRIBUTOS CORRELACIONADOS ##################


library(gplots)
library(RColorBrewer)

# Reducing Highly Correlated Columns
dev.off() #shut down de multiple Graphic devices

ds.cor=cor(df2[,c(1:4)], use="complete.obs") # hago una matriz de correlacion entre las variables
heatmap.2(ds.cor,
          cellnote = round(ds.cor,1), 
          main = "Correlación",
          notecol="black",     
          density.info="none", 
          trace="none",        
          margins =c(12,12),    
          col=brewer.pal('RdYlBu', n=5),  
          dendrogram="none",     
          Colv="NA")


############### PRUEBO OTRACOSA POR DISCRETIZACION ###################
# el heatmap me hace el grafico de los cuadrados con la gama de colores segun correlacion

# analisis de correlaciones o tablas de contingencia mediante discretizaciones de variables
# variables a discretizar
# followers_count,friends_count, statuses_count,favorites_count, antiguedad
# Luego ver si podemos agrupar por verified o algo asi...
# voy a generar un df nuevo con todas las variables discretizadas
# primero analiso distintos metodos, calculo el sesgo para cada uno y ver cual me gusta mas

library(arules)

# primero para los Followers
df.dis=data.frame(df2$followers,
                  df2$friends,
                  df2$statuses,
                  df2$favorites,
                  as.numeric(df2$antiguedad))

df.dis$followers_interval=arules::discretize(df.dis$df2.followers, method = "interval", breaks = 5, labels=c("Muy Pocos Followers", "Pocos Followers", "Followers Medio", "Altos Followers", "Muy altos Followers"))
df.dis$followers_freq=arules::discretize(df.dis$df2.followers, method = "frequency", breaks = 5, labels=c("Muy Pocos Followers", "Pocos Followers", "Followers Medio", "Altos Followers", "Muy altos Followers"))
df.dis$followers_cluster=arules::discretize(df.dis$df2.followers, method = "cluster", breaks = 5, labels=c("Muy Pocos Followers", "Pocos Followers", "Followers Medio", "Altos Followers", "Muy altos Followers"))


barplot(table(df.dis$df2.followers))
barplot(table(df.dis$followers_interval)) #una opcion
barplot(table(df.dis$followers_freq)) # no me gusta
barplot(table(df.dis$followers_cluster)) #otra opcion


####################### NO VA #####################3
# transformaciones para reducir el sesgo
# genero otro df para esto en particular
df.sesgo=data.frame(df2$followers)

# zscore
df.sesgo$followers.z = scale(df.sesgo$df2.followers, )

# min-max
df.sesgo$followers.minmax = scale(df.sesgo$df2.followers, center=min(df.sesgo$df2.followers),  scale=max(df.sesgo$df2.followers) - min(df.sesgo$df2.followers))
center=TRUE,  scale=TRUE
# escalado robusto
df.sesgo$followers_median = scale(df.sesgo$df2.followers, center=median(df.sesgo$df2.followers),  scale=TRUE)
df.sesgo$followers_median_iqr = scale(df.sesgo$df2.followers, center=median(df.sesgo$df2.followers),  scale=IQR(df.sesgo$df2.followers))

# relativos
df.sesgo$followers_rel_mean =scale(df.sesgo$df2.followers, center=mean(df.sesgo$df2.followers),  scale=mean(df.sesgo$df2.followers))

# decimal scaling
d = floor(log10(max(df.sesgo$df2.followers))) + 1
df.sesgo$followers_decimal_scaling = df.sesgo$df2.followers / 10**d 

#comparo histogramas
hist(df.sesgo$df2.followers)
hist(df.sesgo$followers.z)
hist(df.sesgo$followers.minmax)
hist(df.sesgo$followers_median)
hist(df.sesgo$followers_median_iqr)
hist(df.sesgo$followers_rel_mean)

#creo que nada vale la pena
##################################################


# sigo para los friendscount

df.dis$friends_interval=arules::discretize(df.dis$df2.friends, method = "interval", breaks = 5, labels=c("Muy Baja", "Baja", "Medio", "Alta", "Muy alta"))
df.dis$friends_freq=arules::discretize(df.dis$df2.friends, method = "frequency", breaks = 5, labels=c("Muy Baja", "Baja", "Medio", "Alta", "Muy alta"))
df.dis$friends_cluster=arules::discretize(df.dis$df2.friends, method = "cluster", breaks = 5, labels=c("Muy Pocos Friends", "Pocos Friends", "Friends Medio", "Altos Friends", "Muy altos Friends"))


barplot(table(df.dis$df2.friends))
barplot(table(df.dis$friends_interval)) # PUEDE SER
barplot(table(df.dis$friends_freq)) # no me gusta
barplot(table(df.dis$friends_cluster)) # PUEDE SER me gusta mas



# sigo para los statuses

df.dis$statuses_interval=arules::discretize(df.dis$df2.statuses, method = "interval", breaks = 5, labels=c("Muy Baja", "Baja", "Medio", "Alta", "Muy alta"))
df.dis$statuses_freq=arules::discretize(df.dis$df2.statuses, method = "frequency", breaks = 5, labels=c("Muy Baja", "Baja", "Medio", "Alta", "Muy alta"))
df.dis$statuses_cluster=arules::discretize(df.dis$df2.statuses, method = "cluster", breaks = 5, labels=c("Muy Pocos Tweets", "Pocos Tweets", "Tweets Medio", "Alto en Tweets", "Muy alto en Tweets"))


barplot(table(df.dis$df2.statuses))
barplot(table(df.dis$statuses_interval)) # PUEDE SER
barplot(table(df.dis$statuses_freq)) # no me gusta
barplot(table(df.dis$statuses_cluster)) # PUEDE SER me gusta mas


# sigo para los favorites

df.dis$favorites_interval=arules::discretize(df.dis$df2.favorites, method = "interval", breaks = 5, labels=c("Muy Baja", "Baja", "Medio", "Alta", "Muy alta"))
df.dis$favorites_freq=arules::discretize(df.dis$df2.favorites, method = "frequency", breaks = 5, labels=c("Muy Baja", "Baja", "Medio", "Alta", "Muy alta"))
df.dis$favorites_cluster=arules::discretize(df.dis$df2.favorites, method = "cluster", breaks = 5, labels=c("Muy Bajos Fav", "Bajos Fav", "Medios Fav", "Altos Fav", "Muy altos Fav"))


barplot(table(df.dis$df2.favorites))
barplot(table(df.dis$favorites_interval)) # PUEDE SER
barplot(table(df.dis$favorites_freq)) # no me gusta
barplot(table(df.dis$favorites_cluster)) # PUEDE SER me gusta mas

# Sigo para antiguedad

df.dis$antiguedad_cluster=arules::discretize(df.dis$as.numeric.df2.antiguedad., method = "cluster", breaks = 5, labels=c("Muy poca Antiguedad", "Poca Antiguedad", "Medios Antiguedad", "Alta Antiguedad", "Muy alta Antiguedad"))
barplot(table(df.dis$antiguedad_cluster)) # PUEDE SER me gusta mas


# en todos los casos me quedo con el metodo kmeans

# rearmo entonces un nuevo df quedandome con estas transformaciones + atributo de verified

df.dis.2=data.frame(df.dis$followers_cluster,
                    df.dis$friends_cluster,
                    df.dis$statuses_cluster,
                    df.dis$favorites_cluster,
                    df.dis$antiguedad_cluster,
                    df2$verificadas2)

names(df.dis.2)=c('Followers', 'Friends', 'Statuses', 'Favorites', 'Antiguedad', 'Verified')

tabla.cor.12=table(df.dis.2[,c(1,2)]) # followers y friends
tabla.cor.13=table(df.dis.2[,c(1,3)]) # followers y statuses
tabla.cor.14=table(df.dis.2[,c(1,4)]) # followers y favorites
tabla.cor.15=table(df.dis.2[,c(1,5)]) # followers y antiguedad
tabla.cor.23=table(df.dis.2[,c(2,3)]) # friends y statuses
tabla.cor.24=table(df.dis.2[,c(2,4)]) # friends y favorites
tabla.cor.25=table(df.dis.2[,c(2,5)]) # friends y antiguedad
tabla.cor.34=table(df.dis.2[,c(3,4)]) # statuses y favorites
tabla.cor.35=table(df.dis.2[,c(3,5)]) # statuses y antiguedad
tabla.cor.45=table(df.dis.2[,c(4,5)]) # favorites y antiguedad

n=nrow(df.dis.2) 

t12=scale(tabla.cor.12)
t13=scale(tabla.cor.13)
t14=scale(tabla.cor.14)
t15=scale(tabla.cor.15)
t23=scale(tabla.cor.23)
t24=scale(tabla.cor.24)
t25=scale(tabla.cor.25)
t34=scale(tabla.cor.34)
t35=scale(tabla.cor.35)
t45=scale(tabla.cor.45)

# son 10 en total

# HAGO cada HEATMAP
# voy cambiando el t## y el titulo del grafico

heatmap.2(t13,
          cellnote = round(t13,2), 
          main= "Correlación Followers y Friends %",
          notecol="black",     
          density.info="none", 
          trace="none",        
          margins =c(14,14),    
          col=brewer.pal('RdYlBu', n=5),  
          dendrogram="none",     
          Colv="NA")
# el heatmap me hace el grafico de los cuadrados con la gama de colores segun correlacion



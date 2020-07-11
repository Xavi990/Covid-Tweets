
#---------------------
# Reglas para Usuarios
#---------------------

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("scales") 
install.packages("vctrs") 

sessionInfo()

library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(arules)


#--------------------------------
# Discretización de las variables
#--------------------------------

# 1) friends_count ### Mejorar BINS en mas classificaciones
hist(df_c1$friends_count)
hist(log10(df_c1$friends_count))
df_c1$cat_friends = discretize(log10(df_c1$friends_count),method = "fixed", breaks = c(-Inf, 0.3, 2.1, 3.2, 5, Inf), labels=c("muy pocos","pocos", "medio", "muchos", "muchísimos")) #original c(-Inf, 1, 3, 4, 5, Inf)
table(df_c1$cat_friends)

# 2) followers_count
hist(log10(df_c1$followers_count))
df_c1$cat_followers = discretize(log10(df_c1$followers_count),method = "fixed", breaks = c(-Inf, 0.7, 2.2, 3.0, 5, Inf), labels=c("muy pocos","pocos", "medio", "muchos", "muchísimos")) #original c(-Inf, 1, 3, 4, 5, Inf)
table(df_c1$cat_followers)

# 3) retweet_favorite_count  #### Mehjorar bins de division en base a lo q se ve en el Hist, donde cae patrones etc
hist(df_c1$retweet_favorite_count)
hist(log10(df_c1$retweet_favorite_count))
df_c1$cat_retweet_favorite_count = discretize(log10(df_c1$retweet_favorite_count),method = "fixed", breaks = c(-Inf, 0.6, 2.5, Inf), labels=c("bajo", "medio", "alto")) # original 1.3,inf
table(df_c1$cat_retweet_favorite_count)


# 4) retweet_retweet_count
hist(log10(df_c1$retweet_retweet_count))
df_c1$cat_retweet_retweet_count = discretize(log10(df_c1$retweet_retweet_count), method = "fixed", breaks = c(-Inf, 0.6, 2.3, Inf), labels=c("bajo", "medio", "alto")) # original c(-Inf, 1.5, 3, Inf)
table(df_c1$cat_retweet_retweet_count)

#View(head(df_c1))

# 5) Antiguedad Cuenta
class(df_c4$antiguedad_cuenta)
df_c4$antiguedad_cuenta = as.integer(df_c4$antiguedad_cuenta)
hist(df_c4$antiguedad_cuenta)
df_c4$cat_antiguedad_cuenta = discretize((df_c4$antiguedad_cuenta), method = "fixed", breaks = c(-Inf, 1, 5,9, Inf), labels=c("nueva", "media", "antigua", "muy antigua")) 
table(df_c4$cat_antiguedad_cuenta)

# 6) Actividad en Twitter ( que tanto twitteo en su historia cada usuario)
hist(df_c4$statuses_count)
hist(log10(df_c4$statuses_count))
df_c4$cat_nivel_actividad = discretize(log10(df_c4$statuses_count), method = "fixed", breaks = c(-Inf, 2.5, 3.5, 4.5, Inf), labels=c("muy bajo", "bajo", "medio", "alto")) 
table(df_c4$cat_nivel_actividad)

# 7)#transformamos verified as tipo factor
df_c4$cat_verificada =  as.factor(ifelse(df_c4$verified, "si", NA))

# Rotación de las matriz de datos de usuario y las categóricas
df_tuples_c1 = df_c1 %>% 
  pivot_longer(
    cols = starts_with("cat"),
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
df_tuples_c3 = df_c4 %>% 
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



#
# Reglas para Tweets
#




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
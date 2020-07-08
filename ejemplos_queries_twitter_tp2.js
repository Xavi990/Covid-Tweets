/**
Consulta 1: Genera una nueva coleccion con: 
    - user_id, dia_semana, n_hashtags, friends_count, followers_count, retweet_favorite_count, retweet_retweet_count
    - Y filtrando por presencia de hashtags
**/

db.tweets_mongo_covid19.aggregate(
[
  { $match : {  // Se filtra por hashtags para que no tome null como un hashtag y lo cuente.
      $and: [ 
          {"hashtags.0" : { $exists : true }},
          {"hashtags.0" : { $ne : null}},
          {"hashtags.0" : { $ne : ""}},
      ]
    }},
  {
    $project: {
      user_id:1,
      dia_semana: {$dayOfWeek: "$created_at"}, // Extrae dia de la semana desde la fecha de creacion
      n_hashtags: {$size: "$hashtags"}, // Extrae la cantidad de hashtags desde la lista de hashtags usados en el tweet
      friends_count: 1,
      followers_count: 1,
      retweet_favorite_count: 1,
      retweet_retweet_count: 1,
      
      _id:0
    }
  },
  {$out: "usuarios_nhashtgs_dia"}
])
  

/**
Consulta 2: Genera una nueva coleccion con: user_id y hashtag
**/
db.tweets_mongo_covid19.aggregate(
[
  {
    $project: {
      user_id:1,
      hashtags: 1,
      _id:0
    }
  },
  {$unwind: {path: "$hashtags", preserveNullAndEmptyArrays: false}},
  {
    $match: {
      hashtags: {$ne: null}
    }
  },
  {
    $project: {
      user_id:1,
      cat_hashtags: {$toLower: "$hashtags"},
      _id:0
    }
  },
  {$out: "usuarios_hashtags"}
])
  
  
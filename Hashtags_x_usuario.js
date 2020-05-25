db.tweets_v01.aggregate(
  [
    {$project: {"hashtags": 1, "screen_name": 1, _id: 0}},
    {$unwind: "$hashtags"},
    {$match: {"hashtags": {$ne: null}}},
    {$group: {_id: {"usuarios": "$screen_name", 
                    "hashtags": "$hashtags"}, 
                    count: {$sum: 1}}},
    {$sort: {count: -1}},
    {$project: {"count": 1, "_id": 0, "hashtag": "$_id.hashtags", "usuario": "$_id.usuarios"}},
    {$out: "hashtags_usados_x_usuario"}
  ]
)
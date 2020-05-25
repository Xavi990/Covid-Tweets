db.tweets_v01.aggregate(
  [
    {$project: {"hashtags": 1, _id: 0}},
    {$unwind: "$hashtags"},
    {$match: {"hashtags": {$ne: null}}},
    {$group: {_id: {"hashtags": "$hashtags"}, 
                    count: {$sum: 1}}},
    {$sort: {count: -1}},
    {$project: {"_id": 0, "hashtag": "$_id.hashtags", "count": 1}},
    {$out: "hashtags_usados"}
  ]
)
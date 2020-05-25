db.tweets_v01.aggregate(
  [
    {$project: {"location": 1, _id: 0}},
//    {$match: {"location": {$ne: null}}},
    {$group: {_id: "$location", count: {$sum: 1}}},
    {$sort: {count: -1}},
    {$project: {_id: 0, location: "$_id", "count": 1}},
    {$out: "locations"}
  ]
)
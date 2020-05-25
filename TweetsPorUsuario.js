db.getCollection('tweets_mongo_covid19').aggregate(
    [
    
        {
            $group:
            {
                _id: "$user_id",
                cant: {$sum:1} 
            }
    
        },
        {
            $sort : {"cant" : -1}
        },
        {
            $match: {"cant": {$gte:10}}
        }
    ]
 )
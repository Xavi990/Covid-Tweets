

db.createView("tweets_v01", "tweets_mongo_covid19", [ 
    { $project: { status_id: 0, 
                  display_text_width: 0, 
                  symbols: 0, 
                  urls_url:0, 
                  urls_t_co:0, 
                  media_url: 0, 
                  media_t_co: 0, 
                  ext_media_url: 0, 
                  ext_media_t_co: 0,   
                  geo_coords: 0,
                  coords_coords: 0,
                  bbox_coords: 0,
                  listed_count: 0,
                  profile_banner_url: 0,
                  profile_background_url: 0,
                  profile_image_url: 0,
                  quote_count: 0,
                  reply_count: 0,
                  retweet_status_id: 0
        } } ])
    
    
    
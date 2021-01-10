gtfs_tripUpdates2 <- function(FeedMessage, routes){
  
  #routes <- as.character(2:4)
  #FeedMessage <- requests[[1]]$result %>% gtfs_realtime()
  
  ## validate FeedMessage
  if(!"transit_realtime.FeedMessage" %in% attributes(FeedMessage))
    stop("FeedMessage must be a FeedMesssage response")
  
  trip_updates <- FeedMessage$entity %>%
    keep(~length(.x[['trip_update']]) > 0 && .x[['trip_update']][['trip']][['route_id']] %in% routes)
  
  lst <- lapply(trip_updates, function(x){
    
    trip <- x[['trip_update']][['trip']]
    
    trip_id = trip[['trip_id']]
    start_time = trip[['start_time']]
    start_date = trip[['start_date']]
    route_id = trip[['route_id']]
    
    stop_time_update <- x[['trip_update']][['stop_time_update']]
    
    stop_time_update <- lapply(stop_time_update, function(y){
      arrival <- y[['arrival']]
      departure <- y[['departure']]
      return(data.table::data.table(
        stop_sequence = y[['stop_sequence']],
        stop_id = y[['stop_id']],
        arrival_time = arrival[['time']],
        arrival_delay = arrival[['delay']],
        departure_time = departure[['time']],
        departure_delay = departure[['delay']]
      ))
    })
    dt_stop_time_update <- data.table::rbindlist(stop_time_update, use.names = T, fill = T)
    
    dt_trip_info <- data.table::data.table(trip_id = trip_id,
                                           start_time = start_time,
                                           start_date = start_date,
                                           route_id = route_id)
    
    
    return(list(dt_trip_info = dt_trip_info,
                dt_stop_time_update = dt_stop_time_update))
    
  })
}
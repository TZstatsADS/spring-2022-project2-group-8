
latlng <- function(df){
  d <- df[c('start_loc','end_loc')]
  l1 <- d$start_loc
  l2 <- d$end_loc
  dd <- tibble(c(l1,l2))
  colnames(dd) <- c('loc')
  dd <- unique(dd)
}


new_clean <- function(X){
  x <- X %>% drop_na %>%
  mutate(date = as_date(started_at),
         time = hour(as_datetime(started_at))) %>%
  mutate(time = as.integer(time)) %>%
  mutate(week = ceiling_date(date, 'weeks')) %>%
  mutate(tripduration = as.numeric(difftime(ended_at, started_at ,units = "mins"))) %>%
  mutate(time_freg = case_when(time %in% 0:5 ~ 'midnight',
                               time %in% 6:10 ~ 'morning',
                               time %in% 11:14 ~ 'noon',
                               time %in% 15:18 ~ 'afternoon',
                               time %in% 19:23 ~ 'evening')) %>%
  filter(tripduration <= 480,
           start_lng <= -70, start_lng >= -80,
           end_lng <= -70, end_lng >= -80,
           start_lat <= 46, start_lat >= 39,
           start_lat <= 46, start_lat >= 39,) %>%
  mutate(start_lng = round(start_lng,3),
         start_lat = round(start_lat,3),
         end_lng = round(end_lng,3),
         end_lat = round(end_lat,3),) %>%
  #  rowwise %>%
  #  mutate(distance = distm (x = c(start_longitude, start_latitude), 
  #                           y = c(end_longitude, end_latitude))) %>%
  mutate(start_loc =  paste(start_lat,start_lng, sep = ',')) %>%
  mutate(end_loc =  paste(end_lat,end_lng, sep = ',')) %>%
  rename(usertype = member_casual) %>%
  dplyr::select(date,week,time_freg,tripduration, usertype,
                #start_station_name, start_station_id,
                #end_station_name, end_station_id, 
                #Latitude,Longitude,
                start_loc,end_loc) # start
                #start_lat, start_lng,end_lat,end_lng,) %>%
   
  
  return(x)
}


old_clean <- function(X){
    x <- X %>% drop_na %>%
    mutate(date = as_date(starttime),
           time = hour(as_datetime(starttime))) %>%
    mutate(time = as.integer(time)) %>%
    mutate(week = ceiling_date(date, 'weeks')) %>%
    mutate(tripduration = as.numeric(tripduration)/60) %>%
    mutate(time_freg = case_when(time %in% 0:5 ~ 'midnight',
                                 time %in% 6:10 ~ 'morning',
                                 time %in% 11:14 ~ 'noon',
                                 time %in% 15:18 ~ 'afternoon',
                                 time %in% 19:23 ~ 'evening')) %>%
    mutate(start_lng = round(start.station.longitude,digit = 3),
             start_lat = round(start.station.latitude,digit = 3),
             end_lng = round(end.station.longitude,digit = 3),
             end_lat = round(end.station.latitude,digit = 3),) %>%
    filter(tripduration <= 480,
             start_lng <= -70, start_lng >= -80,
             end_lng <= -70, end_lng >= -80,
             start_lat <= 46, start_lat >= 39,
             start_lat <= 46, start_lat >= 39,) %>%  
    #mutate(start_loc =map2(start_lat, start_lng, findzip))%>%
    #mutate(end_loc = map2(end_lat, end_lng, findzip)) %>%
    mutate(start_loc =  paste(start_lat,start_lng, sep = ',')) %>%
    mutate(end_loc =  paste(end_lat,end_lng, sep = ',')) %>%
    dplyr::select(date,week,time_freg,tripduration, usertype,start_loc, end_loc)
    
    
  return(x)
}



daily_stat <- function(df, stat_list){
  
  res_1 <- df %>%
    group_by(week) %>%
    summarise_at(c('tripduration'),sum) %>%
    arrange(week)
  
  res_2 <- df %>%
    group_by(week) %>%
    do(data.frame(totaltrip = nrow(.)))%>%
    arrange(week) 
  
  res <- cbind(res_1,res_2['totaltrip'])
  
  for (name in stat_list){
    f <- aggregate(unlist(df[name]), by=list(df$week,unlist(df[name])), FUN = length)
    f <- f %>% 
      pivot_wider(names_from = "Group.2", values_from = "x") %>%
      arrange(Group.1) %>%
      dplyr::select(-c('Group.1'))
    res <- cbind(res,f)
  }
  
  return (res)
}



new_clean_yearly <- function(X){
  x <- X %>% drop_na %>%
    mutate(date = as_date(started_at),
           time = hour(as_datetime(started_at))) %>%
    mutate(week = ceiling_date(date, 'weeks')) %>%
    mutate(time = as.integer(time)) %>%
    mutate(tripduration = as.numeric(difftime(ended_at, started_at ,units = "mins"))) %>%
    mutate(time_freg = case_when(time %in% 0:5 ~ 'midnight',
                                 time %in% 6:10 ~ 'morning',
                                 time %in% 11:14 ~ 'noon',
                                 time %in% 15:18 ~ 'afternoon',
                                 time %in% 19:23 ~ 'evening')) %>%
    #  rowwise %>%
    #  mutate(distance = distm (x = c(start_longitude, start_latitude), 
    #                           y = c(end_longitude, end_latitude))) %>%
    rename(usertype = member_casual) %>%
    dplyr::select(date,week,time_freg,tripduration, usertype) %>%
    filter(tripduration <= 480) 
  
  return(x)
}

old_clean_yearly <- function(X){
  x <- X %>% drop_na %>%
    mutate(date = as_date(starttime),
           time = hour(as_datetime(starttime))) %>%
    mutate(time = as.integer(time)) %>%
    mutate(week = ceiling_date(date, 'weeks')) %>%
    mutate(tripduration = as.numeric(tripduration)/60) %>%
    mutate(time_freg = case_when(time %in% 0:5 ~ 'midnight',
                                 time %in% 6:10 ~ 'morning',
                                 time %in% 11:14 ~ 'noon',
                                 time %in% 15:18 ~ 'afternoon',
                                 time %in% 19:23 ~ 'evening')) %>%
    dplyr::select(date,week,time_freg,tripduration, usertype) %>%
    filter(tripduration <= 480) 
  
  return(x)
}




library(dplyr)
library(time)
library(rvest)
library(xml2)
library(tidyverse)
library(ggmap)
library(lubridate)
library(remotes)
library(sf)

remotes::install_github("jjchern/zipzcta")
setwd("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned")

process_data = function(url){
  # Retrieve the data
  data = read.csv(url)
  # For each turnstile and for each we count the number of passes
  count_turnstile_per_day = aggregate(
    data$ENTRIES,
    list(data$DATE,data$C.A,data$SCP,data$UNIT),
    (function(x) sum(diff(x)))
    )
  # Rename the columns of our new data frame
  names(count_turnstile_per_day) = c("DATE","C.A","SCP","UNIT","TURNSTILE_PER_DAY")
  # Retrieve the Station corresponding to each turnstile
  processed_data = merge(
    count_turnstile_per_day,
    (data
     %>%group_by("C.A","SCP","UNIT","DATE")
     %>%select("STATION","LINENAME","C.A","SCP","UNIT","DATE")),
    by=c("C.A","SCP","UNIT","DATE")
    )
  name_file = str_split(url,"/")[[1]][8]
  write.csv(processed_data, paste0("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway_Data/Turnstile_Data/",name_file))
}
# We get all the hyperlinks for the MTA Subway
URL <-"http://web.mta.info/developers/turnstile.html"
pg <- read_html(URL)
hyperlinks = html_attr(html_nodes(pg, "a"), "href")
# We retrieve the hyperlink corresponding to the years 2019-2022
hyperlinks_for_Covid = hyperlinks[
  str_detect(hyperlinks,"data/nyct/turnstile/turnstile_(2[012]|19)")
  ]
hyperlinks_for_Covid = hyperlinks_for_Covid[-1]
# Number of links to go through
n = length(hyperlinks_for_Covid)
# We go through all the hyperlinks, scrap the data
# then process it and save the resulting dataframe
t_1 = timeDate()
for(i in n:1){
  process_data(paste0("http://web.mta.info/developers/",hyperlinks_for_Covid[i]))
}
t_2 = timeDate()
print(t_2-t_1)
# We retrieve all the dataframe we saved and fused them in one.
map = read.csv("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Stations.csv")
names(map)[2] = "complex_id"
map = map%>%select(complex_id,GTFS.Longitude, GTFS.Latitude)
missing_map = read.csv("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Location_of_tram_and_train.txt")
missing_map = missing_map%>%select(complex_id,GTFS.Longitude, GTFS.Latitude)
map = rbind(map,missing_map)
map = map%>%group_by(complex_id)%>%mutate(GTFS.Latitude = mean(GTFS.Latitude),GTFS.Longitude = mean(GTFS.Longitude))%>%unique
turnstile_id = read.csv("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/body.csv")
names(turnstile_id) = c("UNIT","C.A","complex_id","station","line_name","division")
name_file = str_split(hyperlinks_for_Covid[1],"/")[[1]][4]
data = read.csv(paste0("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Turnstile_Data/",name_file))
data = data%>%select("C.A","SCP","UNIT","DATE","TURNSTILE_PER_DAY")%>%unique
data = data%>%filter(TURNSTILE_PER_DAY>0)
data = data[!(data$TURNSTILE_PER_DAY>1e5),]
data = merge(data,turnstile_id, by=c("UNIT","C.A"))
missing_values = data[is.na(data$complex_id),]%>%select(UNIT,C.A,station)%>%unique
for(i in 1:20){
  info = missing_values[i,]
  data$complex_id[as.logical((data$UNIT == info$UNIT)*(data$C.A == info$C.A))] = 636+i
}
data = aggregate(
  data$TURNSTILE_PER_DAY,
  list(data$station,data$complex_id,data$line_name,data$DATE),
  sum
)
names(data) = c("station","complex_id","line_name","DATE","TURNSTILE_PER_DAY")
data = merge(data,map, by="complex_id")
for(url in hyperlinks_for_Covid[-1]){
  name_file = str_split(url,"/")[[1]][4]
  df = read.csv(paste0("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Turnstile_Data/",name_file))
  df = df%>%select("C.A","SCP","UNIT","DATE","TURNSTILE_PER_DAY")%>%unique
  df = df%>%filter(TURNSTILE_PER_DAY>0)
  df = df[!(df$TURNSTILE_PER_DAY>1e5),]
  df = merge(df,turnstile_id, by=c("UNIT","C.A"))
  missing_values = df[is.na(df$complex_id),]%>%select(UNIT,C.A,station)%>%unique
  for(i in 1:20){
    info = missing_values[i,]
    df$complex_id[as.logical((df$UNIT == info$UNIT)*(df$C.A == info$C.A))] = 636+i
  }
  df = aggregate(
    df$TURNSTILE_PER_DAY,
    list(df$station,df$complex_id,df$line_name,df$DATE),
    sum
  )
  names(df) = c("station","complex_id","line_name","DATE","TURNSTILE_PER_DAY")
  df = merge(df,map, by="complex_id")
  data = rbind(data,df)
}

location = data%>%select(complex_id,GTFS.Longitude,GTFS.Latitude)%>%unique
get_zip_code = function(row){
  adress = revgeocode(c(as.numeric(row[2]),as.numeric(row[3])))
}
zip_codes = apply(location,1,get_zip_code)
missing_zip_codes = zip_codes[!str_detect(zip_codes,'[0-9]{5}')]
zip_codes_processed = str_extract(zip_codes,'(?<=[A-Z]{2} )[0-9]{5}')
location = cbind(location,zip_codes_processed)
missing_zip_codes = c("11221","11221","11212","11414","10453","10034","10467","10029","10470","11372","10001","10014","10013")
location$zip_codes[is.na(location$zip_codes)] = missing_zip_codes
data = merge(data,location%>%select(zip_codes,GTFS.Longitude,GTFS.Latitude), by=c("GTFS.Longitude","GTFS.Latitude"))

crosswalk <-zipzcta::zipzcta
crosswalk<-crosswalk%>%select(zip,zcta)
names(crosswalk) = c("zip_codes","zcta")
data<-merge(data,crosswalk,by="zip_codes")

data$DATE = as.Date(data$DATE, format="%m/%d/%Y")
data<-data%>%mutate(Year_Week = paste(year(DATE),week(DATE),sep="-"))
data$Year_Week[week(data$DATE)=="53"] = paste(as.numeric(year(data$DATE[week(data$DATE)=="53"]))+1,1,sep="-")
data<-data%>%group_by(Year_Week, complex_id, station, line_name)%>%mutate(TURNSTILE_PER_WEEK = sum(TURNSTILE_PER_DAY))%>%ungroup()
data<-data%>%filter(complex_id !=640)
data<-data%>%filter(complex_id != 641)
data<-data%>%unique

shape <- read_sf("cb_2018_us_zcta510_500k.shp")
shape<-shape%>%select(ZCTA5CE10,geometry)
names(shape) = c("zcta","geometry")
data<-merge(data,shape)
data<-data%>%mutate(Weekdays = weekdays(DATE))
get_date<-function(Year_Week){
  return(as.Date(paste(Year_Week,"1",sep="-"),"%Y-%W-%u"))
}
l<-lapply(data$Year_Week, get_date)
data$Year_Week<-do.call("c",l)
data$Year_Week<-as.Date(data$Year_Week,"%Y-%W-%u")
data<-data[order(data$Year_Week),]
data_per_week_days<-data%>%group_by(station,
                                    line_name,
                                    complex_id,
                                    GTFS.Longitude,
                                    GTFS.Latitude,
                                    Year = year(DATE),
                                    Weekdays
                                    )%>%mutate(TURNSTILE_PER_WEEKDAY = mean(TURNSTILE_PER_DAY))
data_per_week_days<-data_per_week_days%>%select(station,
                           line_name,
                           complex_id,
                           GTFS.Longitude,
                           GTFS.Latitude,
                           Year,
                           Weekdays,
                           TURNSTILE_PER_WEEKDAY)%>%unique
data<-data%>%select(station,
                    complex_id,
                    line_name,
                    GTFS.Longitude,
                    GTFS.Latitude,
                    zcta,
                    geometry,
                    Year_Week,
                    TURNSTILE_PER_WEEK)%>%unique

color_station<-read.csv("C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Color_of_Station.txt",check.names = FALSE)

get_lines_logo<-function(row){
  color_station <- color_station[unlist(strsplit(row,""))]
  paste0(
    sprintf(
      "<div><span id='circle' class=%s>%s</span></div>",
      color_station, 
      names(color_station)),
    collapse=""
  )
}
color<-unlist(lapply(data$line_name,get_lines_logo))
data$html_color <-color
data<-st_as_sf(data)
names(data)[8]<-"value_per_week"

write.csv(data,"C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Subway_Data_Processed.csv")
write_rds(data,"C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Subway_Data_Processed.RDS")

write.csv(data_per_week_days,"C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Weekday_Subway_Data.csv")
write_rds(data_per_week_days,"C:/Users/aroni/Documents/STATGR5243/Project_2/spring-2022-project2-group-8/data/Subway/cleaned/Weekday_Subway_Data_Processed.RDS")


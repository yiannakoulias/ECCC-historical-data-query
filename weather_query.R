#-------------------------------
#Extract data from ECCC based on FSA and date
#N. Yiannakoulias 2022-10-22

#The code works by calculating the geometric centroid of the FSAs selected
#then it pulls the ECCC data from stations that are closest to that centroid
#-------------------------------

library(weathercan)
library(fields)

#-------------------------------
#user settings
#-------------------------------

#change the path to the FSA file below
fsa_coord <- read.csv("D:\\FSA_coord.csv")

#Important note! pick any FSAs, but ensure you either do it one at a time, or select adjacent FSAs
#Otherwise the station data will be meaningless

fsa_list <- c("T6E","T6G")

start_date <- 1950
end_date <- 2021

distance_threshold <- 10 #maximum distance you're willing to look from t he centroid

#-------------------------------
#obtain a time series placeholder based on user information
#-------------------------------
time_series <- as.data.frame(seq(1:((end_date-start_date)+1))+(start_date-1))
names(time_series) <- "Year"

#-------------------------------
#Get station data
#-------------------------------
stations <- stations()
stations <- stations[stations$interval=="day",]


#-------------------------------
#Prepare FSA data
#-------------------------------

fsa_list <- as.data.frame(fsa_list)
names(fsa_list) <- c("FSA")

fsa_coord <- merge(fsa_coord,fsa_list,by="FSA")
search_lon <- sum(fsa_coord$lon_coord)/nrow(fsa_coord)
search_lat <- sum(fsa_coord$lat_coord)/nrow(fsa_coord)

#-------------------------------
#Calculate the distance between all stations and the centroid of selected FSAs
#-------------------------------
  
stations$distance <- t(rdist.earth(as.matrix(t(c(search_lon,search_lat))),
                                   as.matrix(cbind(stations$lon,stations$lat)),
                                   miles=FALSE,R=NULL))

prospective_stations <- stations[stations$distance <= distance_threshold,]

#delete NA
prospective_stations <- prospective_stations[!is.na(prospective_stations$station_id),]

#sort
prospective_stations <- prospective_stations[order(prospective_stations$distance),]


#-------------------------
#Go through the station list and look for the closest stations
#-------------------------

#select the years for this iteration
temp <- seq(1:((prospective_stations$end[1]-prospective_stations$start[1])+1))+
  (prospective_stations$start[1]-1)

#start the running dataset to evaluate
check <- unique(intersect(temp,time_series$Year))

#take as much data as possible from the closest station
keep_stations <- prospective_stations$station_id[1]
start_stations <- prospective_stations$start[1]
end_stations <- prospective_stations$end[1]

for(i in 2:nrow(prospective_stations)){
  temp <- seq(1:((prospective_stations$end[i]-prospective_stations$start[i])+1))+
    (prospective_stations$start[i]-1)
  
  #create a subset what is in temp and not already in check
  newinfo <- setdiff(temp,check)
  
  if(length(newinfo) > 0){
    #potentially new data
    overlap <- intersect(time_series$Year,temp)
    if(length(overlap > 0)){
      keep_stations <- c(keep_stations,prospective_stations$station_id[i])
      start_stations <- c(start_stations,prospective_stations$start[i])
      end_stations <- c(end_stations,prospective_stations$end[i])
      check <- unique(c(overlap,check))
    }
  }
  if(length(check)==length(time_series)){
    break
  }
}

weather_data<- c()

for(i in 1:length(keep_stations)){
  s <- gsub(" ","",paste(start_stations[i],"-01-01"))
  e <- gsub(" ","",paste(end_stations[i],"-12-31"))
  weather_data[[i]] <- weather_dl(station_ids=keep_stations[i],
                   start=s,end=e,interval="day",
                   verbose = TRUE,quiet=TRUE)
  
  Sys.sleep(1)#avoid problems with server rejecting requests
}

#----------------------------------------
#Clean and combine the files
#----------------------------------------

df <- weather_data[[1]]

for(i in 2:length(weather_data)){
  df <- rbind(df,weather_data[[i]])
}

df <- df[order(df$date),]

df <- df[df$date >= as.Date(gsub(" ","",paste(start_date,"-01-01"))) & 
           df$date <= as.Date(gsub(" ","",paste(end_date,"-12-31"))),]

df <- merge(df,stations[,c("station_id","distance")],by="station_id")

#sort by date and distance (to get closest station data)
df <- df[order(df$date,df$distance),]

#---------------------------------
#Select the closest station when there is overlap
#---------------------------------
df$keep <- 0
for(i in 2:nrow(df)){
  if(df$date[i]!=df$date[i-1]){
    df$keep[i] <- 1
  }
}

df <- df[df$keep==1,]


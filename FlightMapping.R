# Create a map of the flight routes in our dataset
library(tidyverse)
library(ggmap)
library(readxl)

# read the data file in as a dataframe
data <- read_xlsx("./data/Satisfaction Survey(2).xlsx")


# ----------------------------------------------------------------------------------------
# Find the latitude and longitude of each Origin and Destination city.
# ----------------------------------------------------------------------------------------

# Strip out the city name to create City column for both the Origin and Destination
data$OriginCity <- sapply(strsplit(data$`Orgin City`, ","), '[',1)
data$DestinationCity <- sapply(strsplit(data$`Destination City`, ","), '[',1)

# Create a city state combination in the data to pass to the geocode function
data$OriginCityState <- paste(data$OriginCity, ", ", data$`Origin State`, sep="")
data$DestinationCityState <- paste(data$DestinationCity, ", ", data$`Destination State`, sep="")


# Get the unique Origin and destination cities to limit api calls to Google's geocode
unqOrigins <- unique(data$OriginCityState)
unqDests <- unique(data$DestinationCityState)

# Get a dataframe of lat and lon for the unique origins
# OriginLatLon <- geocode(unqOrigins)
sapply(OriginLatLon, function(x) sum(is.na(x))) # There is one NA, and based on log of geocode calls.. It's Guam

# Rplace te one NA with Guam's lon and lat
OriginLatLon$lon <- ifelse(is.na(OriginLatLon$lon), guam$lon, OriginLatLon$lon)
OriginLatLon$lat <- ifelse(is.na(OriginLatLon$lat), guam$lat, OriginLatLon$lat)
sapply(OriginLatLon, function(x) sum(is.na(x))) # no more NAs!
colnames(OriginLatLon) <- c("Orig_lon", "Orig_lat")
# combine the locations with the Origin name
OriginLatLon <- cbind(OriginLatLon, unqOrigins)

# Comment out the geocode requests to not make api calls
# DestLatLon <- geocode(unqDests)
sapply(DestLatLon, function(x) sum(is.na(x))) # Based on geocode logs, Guam is once again the only NA, luckily we have
# Replace the one NA with Guam's lon and lat
DestLatLon$lon <- ifelse(is.na(DestLatLon$lon), guam$lon, DestLatLon$lon)
DestLatLon$lat <- ifelse(is.na(DestLatLon$lat), guam$lat, DestLatLon$lat)
sapply(DestLatLon, function(x) sum(is.na(x))) # No more NAs
colnames(DestLatLon) <- c("Dest_lon", "Dest_lat")
# combine the locations with the Dest name
DestLatLon <- cbind(DestLatLon, unqDests)

# write the lat and lons of the locations so we do not need to make API calls again
write.csv(OriginLatLon, file="./data/OrigLatLon.csv")
write.csv(DestLatLon, file="./data/DestLatLon.csv")

OriginLatLon <- read.csv("./data/OrigLatLon.csv")
DestLatLon <- read.csv("./data/DestLatLon.csv")

# left join the data to bring in the Origin lat and lon
data <- merge(data, OriginLatLon, by.x="OriginCityState", by.y="unqOrigins", all.x=TRUE)

# left join the data to bring in the Destination lat and lon
data <- merge(data, DestLatLon, by.x="DestinationCityState", by.y="unqDests", all.x=TRUE)

# Drop the indicies from the LatLon files. Key is to use grave accents and not quotes
data = subset(data, select=-c(`X.x`, `X.y`))


library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(geosphere)
library(plyr)

# ----------------------------------------------------------------------------------------
# Plot the routes via code provided by the below link:
# https://weiminwang.blog/2015/06/24/use-r-to-plot-flight-routes-on-a-fancy-world-background/
# ----------------------------------------------------------------------------------------

fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}

# get the count of each route
dat_grp <- data %>% 
  dplyr::group_by(OriginCityState, DestinationCityState) %>%
  dplyr::summarise(
    count = dplyr::n()
  )

# merge to bring the lat and lon into the data
dat_grp <- merge(dat_grp, OriginLatLon, by.x="OriginCityState", by.y="unqOrigins", all.x=TRUE)
dat_grp <- merge(dat_grp, DestLatLon, by.x="DestinationCityState", by.y="unqDests", all.x=TRUE)
dat_grp = subset(dat_grp, select=-c(`X.x`, `X.y`))

# Check that the count matches the total rows in the data 
sum(dat_grp$count) == nrow(data)
head(dat_grp)
# check for any NAs
sapply(dat_grp, function(x) sum(is.na(x)))

# calculate routes for each row
routes = gcIntermediate(dat_grp[,c('Orig_lon', 'Orig_lat')], dat_grp[,c('Dest_lon', 'Dest_lat')], 200, breakAtDateLine = FALSE, addStartEnd = TRUE, sp=TRUE)
# fortify to dataframe
fortifiedroutes <- fortify.SpatialLinesDataFrame(routes)

routes_count <- data.frame('count'=dat_grp$count, 'id'=1:nrow(dat_grp), 'Location'=dat_grp$OriginCityState)
greatcircles <- merge(fortifiedroutes, routes_count, all.x=T, by='id')
library(rgdal)
urbanareasin <- readOGR("./ne_10m_urban_areas/ne_10m_urban_areas.shp")
urb <- c(geom_polygon(aes(long, lat, group = group),
                      size = 0.3,
                      color = "#ffffff",
                      fill = "#ffffff",
                      alpha = 1,
                      data = urbanareasin))

# get world map
worldmap <- map_data("world")

wrld <- c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#090D2A",
               fill="#090D2A", alpha=0.8, data=worldmap))
flightmap <- ggplot() +
  wrld +
  urb +
  geom_line(aes(long,lat,group=id, color=Location), alpha = 0.3, size=0.01, data= greatcircles) +
  theme(panel.background = element_rect(fill='#00001C',colour='#00001C'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none",
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("./images/flightmap.png")

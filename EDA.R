# Exploratory Data Analysis
library(tidyverse)
library(ggmap)
# use gdata to read in the .xls file of customer survey data
library(readxl)
data <- read_xlsx("./Satisfaction Survey(2).xlsx")

str(data)

summary(data)

# view the data
head(data)
tail(data)
View(data)

# Find the amount of null values in each column
sapply(data, function(x) sum(is.na(x)))

# Find the amount of null values in each column using dplyr
data %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  transpose()


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
OriginLatLon <- geocode(unqOrigins)
sapply(OriginLatLon, function(x) sum(is.na(x))) # There is one NA, and based on log of geocode calls.. It's Guam

# Rplace te one NA with Guam's lon and lat
OriginLatLon$lon <- ifelse(is.na(OriginLatLon$lon), guam$lon, OriginLatLon$lon)
OriginLatLon$lat <- ifelse(is.na(OriginLatLon$lat), guam$lat, OriginLatLon$lat)
sapply(OriginLatLon, function(x) sum(is.na(x))) # no more NAs!
colnames(OriginLatLon) <- c("Orig_lon", "Orig_lat")
# combine the locations with the Origin name
OriginLatLon <- cbind(OriginLatLon, unqOrigins)

DestLatLon <- geocode(unqDests)
sapply(DestLatLon, function(x) sum(is.na(x))) # Based on geocode logs, Guam is once again the only NA, luckily we have
# Replace the one NA with Guam's lon and lat
DestLatLon$lon <- ifelse(is.na(DestLatLon$lon), guam$lon, DestLatLon$lon)
DestLatLon$lat <- ifelse(is.na(DestLatLon$lat), guam$lat, DestLatLon$lat)
sapply(DestLatLon, function(x) sum(is.na(x))) # No more NAs
colnames(DestLatLon) <- c("Dest_lon", "Dest_lat")
# combine the locations with the Dest name
DestLatLon <- cbind(DestLatLon, unqDests)

# write the lat and lons of the locations so we do not need to make API calls again
write.csv(OriginLatLon, file="./OrigLatLon.csv")
write.csv(DestLatLon, file="./DestLatLon.csv")


# left join the data to bring in the Origin lat and lon
data <- merge(data, OriginLatLon, by.x="OriginCityState", by.y="unqOrigins", all.x=TRUE)

# left join the data to bring in the Destination lat and lon
data <- merge(data, DestLatLon, by.x="DestCityState", by.y="unqDests", all.x=TRUE)



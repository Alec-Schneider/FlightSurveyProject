# Vacation Destinations 
library(tidyverse)
source("./DataCleaning.R")

df <- read_xlsx("./data/Satisfaction Survey(2).xlsx")
df <- process_flight_survey(df)

head(df)

unq_dests <- unique(df$Destination_City)

# write.csv(unq_dests, file='./data/travel_dests.csv', row.names=FALSE)

# After manually mapping whether a destination was a vacation destination
dests <- read.csv('./data/travel_dests.csv')
df <- merge(df, dests, by.x="Destination_City", by,y="Destination", )

# join the data togther 
df_merged <- df %>%
  left_join(dests, by=c("Destination_City" = "Destination"))


avg_score_vac_dest <- df_merged %>%
  filter(Type_of_Travel != "Business travel") %>%
  group_by(Vacation) %>%
  summarize(avg_satisfaction = mean(Satisfaction),
            count=n(),
            std=sd(Satisfaction))
avg_score_vac_dest
vacay_dests <- unique(df_merged[df_merged$Vacation == 1, "Destination_City"])

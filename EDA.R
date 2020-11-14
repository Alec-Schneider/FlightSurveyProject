# Exploratory Data Analysis
library(tidyverse)
library(readxl)

# read the data file in as a dataframe
data <- read_xlsx("./data/Satisfaction Survey(2).xlsx")


# ----------------------------------------------------------------------------------
# Data Cleaning 
# ----------------------------------------------------------------------------------
# remove all spaces from the column names as this may cause a problem later on
for (i in 1:length(colnames(data))){
  colnames(data)[i] <- gsub(" ", "_", colnames(data)[i])
}

numeric_cols <- unlist(lapply(data, is.numeric))

# Find the amount of null values in each column
sapply(data, function(x) sum(is.na(x)))

# Find the amount of null values in each column using dplyr
nulls <- data %>%
  select(everything()) %>%
  summarise_all(list( ~ sum(is.na(.)))) %>%    # lambda function
  transpose()

nulls <- data.frame(nulls=unlist(nulls))


# Check on the nulls in the satisfaction column
# nothing out of the ordinary here. Will impute the mean as this 
nullSats <- data %>%
  filter(is.na(Satisfaction))

# Impute the NAs in Satisfaction with the mean
data$Satisfaction <- ifelse(is.na(data$Satisfaction), mean(data$Satisfaction, na.rm = TRUE), data$Satisfaction)
data %>%
  filter(is.na(Satisfaction)) # check there are none


# Create a bar chart to show the counts of each Satisfaction 
data %>%
  ggplot(mapping = aes(x=Satisfaction)) +
    geom_histogram(fill="darkblue", color="white") +
    ggtitle("Distribution of Satisfaction Scores")
ggsave("./images/SatisfactionDistribution.png")

tabulate(data$Satisfaction)

# Look at only data with null Departure Delays
nullDepDelay <- data %>%
  filter(is.na(Departure_Delay_in_Minutes))

# Look at only data with null arrivalDelays
nullArrivalDelay <- data %>%
  filter(is.na(Arrival_Delay_in_Minutes))

# Not all NA values for Arrival Delay in Minutes are cancelled, interesting
sum(nullArrivalDelay$Flight_cancelled == "Yes")
sum(nullArrivalDelay$Flight_cancelled == "No")

# get the amount of NA values that are related to Flights being cancelled
data %>%
  filter(Flight_cancelled == "Yes") %>%
  select(c(Flight_cancelled, Departure_Delay_in_Minutes, Arrival_Delay_in_Minutes , Flight_time_in_minutes)) %>%
  summarise_all(list( ~ sum(is.na(.)))) %>%    # lambda function
  transpose()

# get the amount of NA values that are not related to Flights being cancelled
View(data %>%
  filter((Flight_cancelled == "No") & is.na(Arrival_Delay_in_Minutes)))

# drop the 337 flights where the flight was not cancelled, and Arrival Delay in Minutes is NA.
data <- data[!((data$Flight_cancelled == "No") & is.na(data$Arrival_Delay_in_Minutes)),]


# ----------------------------------------------------------------------------------
# EDA Through Visualization With ggplot2
# ----------------------------------------------------------------------------------
# Look at the mean Satisfaction score by Airline
airline_mean <- data.frame(mean_satisfaction = with(data ,tapply(data$Satisfaction, data$Airline_Name, mean)))
airline_mean$Airline_Name <- rownames(airline_mean)
rownames(airline_mean) <- NULL
airline_mean$mean_satisfaction <- round(airline_mean$mean_satisfaction, 3)

airline_mean <- airline_mean[order(-airline_mean$mean_satisfaction),]
rownames(airline_mean) <- NULL

# plot the means in a bar chart
ggplot(airline_mean, aes(x=Airline_Name, y=mean_satisfaction, fill=mean_satisfaction, label=mean_satisfaction)) +
  geom_col() +
  geom_text(nudge_y=.3) +
  theme(axis.text.x = element_text(angle=90), legend.position = "None") +
  ylim(0, 5) +
  ggtitle("Average Satisfaction Score by Airline")
ggsave("./images/SatisfactionByAirline.png")


##### Distributions of other variables
# Distribution of the age of passengers
ggplot(data, aes(x=Age)) +
  geom_histogram(binwidth=4, fill="darkred", color="white") +
  ggtitle("Distribution of Passneger Ages")
ggsave("./images/AgeDistribution.png")

# Count of Genders
ggplot(data, aes(x=Gender)) +
  geom_bar(fill="darkblue", color="white") +
  ggtitle("Count of a Passenger's Gender")
ggsave("./images/GenderCount.png")

# Create a density plot of the satisfaction score by gender
# and plot the mean score for each gender
avg_gender <- data %>%
  group_by(Gender) %>%
  summarise(mean_score = mean(Satisfaction))

ggplot(data, aes(x=Satisfaction, color=Gender)) +
  geom_density() +
  geom_vline(data=avg_gender, aes(xintercept=mean_score, color=Gender),
             linetype="dashed") +
  ggtitle("Density Plot of Satisfaction Scores By Gender")
ggsave("./images/SatisfactionGenderDensity.png")

# Different Travel Types
ggplot(data, aes(x=Type_of_Travel)) +
  geom_bar() +
  ggtitle("Count of Travel Types") +
ggsave("./images/TravelTypeCount.png")

# Distribution of Satisfaction Score by Travel Type
ggplot(data, aes(x=Satisfaction, color=Type_of_Travel)) +
  geom_freqpoly(binwidth=0.1) +
  ggtitle("Distribution of Satisfaction Scores by Travel Type")
ggsave("./images/SatisfactionScoresByTravelType.png")

# Distribution of price sensitivity by Age
ggplot(data, aes(x=Age, color=as.factor(Price_Sensitivity))) +
  geom_freqpoly(binwidth=1, aes(y=stat(count) /sum(count))) +
  scale_y_continuous() +
  labs(color="Sensitivity") +
  ylab("Relative Frequency") +
  ylim(-.0005, .02) +
  ggtitle("Relative frequency of Price Sensitivty by Age")
ggsave("./images/RelFreqPriceSensitivityByAge.png")

# Count of Price Sensitivites by Gender
ggplot(data, aes(x=Gender, fill=as.factor(Price_Sensitivity))) +
  geom_bar() +
  labs(color="Sensitivity") +
  ggtitle("Count of Price Sensitivty by Gender")
ggsave("./images/PriceSensitiviesGender.png")

# heatmap of correlations between numeric variables
library(reshape2)
# Get a correlation matrix, but remove the NAs before analyzing
corr_matrix <- round(cor(na.omit(data[, numeric_cols])),2)
melted_corr <- melt(corr_matrix) # melt the matrix to be used in ggplot
# plot the melted correlation matrix with viridis scale coloring
ggplot(melted_corr, aes(x=Var1, y=Var2, fill=value, label=value)) +
  geom_tile() +
  geom_text(color="white", size=3) +
  ggtitle("Correlation Matrix of Numeric Variables") +
  scale_fill_viridis_c() +
  theme(axis.text.x=element_text(angle=90))
ggsave("./images/CorrelationMatrix.png")

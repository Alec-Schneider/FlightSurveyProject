# Density plot of age price sensitivities and satisfactom

library(tidyverse)
source("./DataCleaning.R")

df <- read_xlsx("./data/Satisfaction Survey(2).xlsx")
df <- process_flight_survey(df)

cat_age_grp <- df %>%
  group_by(cat_Age) %>%
  summarize(mean_score=mean(Price_Sensitivity))

ggplot(df, aes(x=Price_Sensitivity, color=cat_Age)) +
  geom_density(size=1) +
  geom_vline(data=cat_age_grp, aes(xintercept=mean_score, color=cat_Age),
             linetype="dashed", size=.75) +
  ggtitle("Density Plot of Price Sensitivity By Age Category")
ggsave("./images/AgePriceSensitivityDensity.png")

ggplot(df, aes(x=Price_Sensitivity, color=cat_Age)) +
  geom_density(size=1) +
  facet_grid(rows=vars(Gender)) +
  ggtitle("Density Plot of Price Sensitivity By Age Category")
ggsave("./images/AgePriceSensitivityDensityByGender.png")


cat_age_grp <- df %>%
  group_by(cat_Age) %>%
  summarize(mean_score=mean(Satisfaction))

ggplot(df, aes(x=Satisfaction, color=cat_Age)) +
  geom_density(size=1) +
  geom_vline(data=cat_age_grp, aes(xintercept=mean_score, color=cat_Age),
             linetype="dashed", size=.75) +
  ggtitle("Density Plot of Satisfaction By Age Category")
ggsave("./images/AgeSatisfactonDensity.png")


ggplot(df, aes(x=Satisfaction, color=cat_Age)) +
  geom_density(size=1) +
  facet_grid(rows=vars(Gender)) +
  ggtitle("Density Plot of Satisfaction By Age Category")
ggsave("./images/AgeSatisfactionDensityByGender.png")

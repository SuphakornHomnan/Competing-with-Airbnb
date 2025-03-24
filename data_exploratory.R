detail_data <- read.csv("listings_details.csv")
summary_data <- read.csv("listings_summary.csv")

library('Hmisc')
library('pastecs')
library('dplyr')
library('Amelia')
library(DataExplorer)
library(tidyverse)

# data diagostic
glimpse(detail_data) #glimpse at the data structure with all column names
glimpse(summary_data) #glimpse at the data structure with all column names
describe(detail_data)
describe(summary_data)
stat.desc(detail_data)
stat.desc(summary_data)

# do EDA on both of the datasets
create_report(summary_data, output_file = "summary_data_report.html")
create_report(detail_data, output_file = "detail_data_report.html")

detail_data %>% 
  nrow()

# plot the proportion of room types
detail_data %>%
  group_by(room_type) %>%
  summarise(
    Listings = n(),
    Percentage = n()/nrow(detail_data) * 100
  ) %>%
  ggplot(aes(x = reorder(room_type,-Percentage), y = Percentage, fill= room_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Room type", y = "% Listings") +
  theme_classic()

detail_data %>%
  filter(room_type %in% c("Private room", "Hotel room")) %>%
  group_by(room_type) %>%
  summarise(
    Listings = n(),
    Percentage = n()/nrow(detail_data) * 100
  ) %>%
  ggplot(aes(x = reorder(room_type,-Percentage), y = Percentage, fill= room_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Room type", y = "% Listings") +
  theme_classic()

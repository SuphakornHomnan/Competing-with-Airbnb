library(tidyverse)
library('dplyr')
library('ggplot2')

# convert chr of host_acceptance_rate to numeric
detail_data$host_response_rate <- as.numeric(gsub("%", "", detail_data$host_response_rate))
detail_data$review_score_location_range <- cut(detail_data$review_scores_location, 
                                               breaks=c(0, 2, 3, 4, 5), 
                                               labels=c("low", "middle", "good", "excellent"))

# Analyse the touristic appealing section
targeted_data <- detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room"))

write.csv(targeted_data, "targeted_data.csv")

detail_data$availability_30_range <- cut(detail_data$availability_30, 
                                        breaks=c(0, 10, 20, 30), 
                                        labels=c("0-10", "11-20", "21-30"))
detail_data$availability_60_range <- cut(detail_data$availability_60, 
                                         breaks=c(0, 20, 40, 60), 
                                         labels=c("0-20", "21-40", "41-60"))

detail_data$availability_90_range <- cut(detail_data$availability_90, 
                                         breaks=c(0, 15, 30, 45, 60, 75, 90), 
                                         labels=c("0-15", "16-30", "31-45", "46-60", "61-75", "76-90"))
            
detail_data$availability_365_range <- cut(detail_data$availability_365, 
                                         breaks=c(0, 60, 120, 180, 240, 300, 365),
                                         labels=c("0-60", "61-120", "121-180", "181-240", "241-300", "301-365"))

total_records_selected_case <- detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room")) %>%
  group_by(availability_30_range) %>%
  nrow()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room")) %>%
  group_by(availability_30_range, instant_bookable) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_records_selected_case) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = availability_30_range, y = Percentage, fill = instant_bookable)) +
  geom_bar(stat = "identity") +
  labs(x = "Availability lists until next month", y = "% Listings") +
  #geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("t" = "#74FE02", "f" = "#FE0202")) +  # Customize colors
  theme_classic()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room")) %>%
  group_by(availability_60_range, instant_bookable) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_records_selected_case) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = availability_60_range, y = Percentage, fill = instant_bookable)) +
  geom_bar(stat = "identity") +
  labs(x = "Availability lists until next two months", y = "% Listings") +
  #geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("t" = "#74FE02", "f" = "#FE0202")) +  # Customize colors
  theme_classic()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room")) %>%
  group_by(availability_90_range, instant_bookable) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_records_selected_case) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = availability_90_range, y = Percentage, fill = instant_bookable)) +
  geom_bar(stat = "identity") +
  labs(x = "Availability lists until next quarter", y = "% Listings") +
  #geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("t" = "#74FE02", "f" = "#FE0202")) +  # Customize colors
  theme_classic()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room")) %>%
  group_by(availability_365_range, instant_bookable) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_records_selected_case) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = availability_365_range, y = Percentage, fill = instant_bookable)) +
  geom_bar(stat = "identity") +
  labs(x = "Availability lists until next year", y = "% Listings") +
  #geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("t" = "#74FE02", "f" = "#FE0202")) +  # Customize colors
  theme_classic()
################################################################################
# Analyse the host section
# convert host response, acceptance rates to numeric
detail_data$host_response_rate <- as.numeric(gsub("%", "", detail_data$host_response_rate))
detail_data$host_acceptance_rate <- as.numeric(gsub("%", "", detail_data$host_acceptance_rate))

detail_data$host_response_rate_group <- cut(detail_data$host_response_rate, 
                                            breaks=c(0, 25, 50, 75, 100), 
                                            labels=c("low", "medium", "high", "very high"))
detail_data$host_acceptance_rate_group <- cut(detail_data$host_acceptance_rate, 
                                              breaks=c(0, 25, 50, 75, 100), 
                                              labels=c("low", "medium", "high", "very high"))
total_record_selected_case <- detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room")) %>%
  group_by(host_response_rate_group) %>% nrow()

total_record_selected_case2 <- detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room") & host_is_superhost != "") %>%
  group_by(host_response_rate_group) %>% nrow()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room") & host_is_superhost != "") %>%
  group_by(host_response_rate_group, host_is_superhost) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case2) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = reorder(host_response_rate_group, Percentage), y = Percentage, fill = host_is_superhost)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Host Response Rates", y = "% Listings") +
  scale_fill_manual(values = c("t" = "#F7E00A", "f" = "#1AECFF")) +  # Customize colors
  theme_classic()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room") & host_is_superhost != "") %>%
  group_by(host_acceptance_rate_group, host_is_superhost) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case2) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = reorder(host_acceptance_rate_group, Percentage), y = Percentage, fill = host_is_superhost)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Host Acceptance Rates", y = "% Listings") +
  scale_fill_manual(values = c("t" = "#F7E00A", "f" = "#1AECFF")) +  # Customize colors
  theme_classic()

detail_data$review_scores_checkin_group <- cut(detail_data$review_scores_checkin, 
                                               breaks=c(0, 2, 3, 4, 5), 
                                               labels=c("low", "middle", "good", "excellent"))
detail_data$review_scores_communication_group <- cut(detail_data$review_scores_communication, 
                                               breaks=c(0, 2, 3, 4, 5), 
                                               labels=c("low", "middle", "good", "excellent"))

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room") & host_is_superhost != "") %>%
  group_by(review_scores_checkin_group, host_is_superhost) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case2) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = reorder(review_scores_checkin_group, Percentage), y = Percentage, fill = host_is_superhost)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Review Score of Checkin", y = "% Listings") +
  scale_fill_manual(values = c("t" = "#F7E00A", "f" = "#1AECFF")) +  # Customize colors
  theme_classic()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room") & host_is_superhost != "") %>%
  group_by(review_scores_communication_group, host_is_superhost) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case2) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = reorder(review_scores_communication_group, Percentage), y = Percentage, fill = host_is_superhost)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Review Score of Communication Service", y = "% Listings") +
  scale_fill_manual(values = c("t" = "#F7E00A", "f" = "#1AECFF")) +  # Customize colors
  theme_classic()

detail_data %>%
  filter(room_type %in% c("Hotel room", "Private room") & host_is_superhost != "", 
         host_identity_verified != "", host_has_profile_pic != "") %>%
  group_by(host_identity_verified, host_has_profile_pic) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case2) * 100  # Calculate percentage
  ) %>%
  ggplot(aes(x = host_identity_verified, y = Percentage, fill = host_has_profile_pic)) +
  geom_bar(stat = "identity") +
  labs(x = "Identity Verified Host", y = "% Listings") +
  scale_fill_manual(values = c("t" = "#F7E00A", "f" = "#1AECFF")) +  # Customize colors
  theme_classic()
################################################################################
# Analyse the estimate market share section
summary_data$price_range <- cut(summary_data$price,
                                breaks= c(0, 50, 100, 300, 500, Inf),
                                labels = c("0-50", "51-100", "101-300", "301-500", "501+"))
summary_data %>%
  filter(room_type %in% c("Hotel room", "Private room")) %>%
  group_by(price_range) %>%
  summarise(
    Amount = n()
  ) %>% arrange(Amount) %>%
  ggplot(aes(x = price_range, y = Amount, fill= Amount)) + geom_bar(stat = "identity") +
  labs(x = "Price range", y = "Amount") +
  geom_text(aes(label = Amount), vjust = -0.3, size = 2) +
  theme_classic()

# Estimate average nights booked (all, last 12 months)
targeted_data$minimum_nights_range <- cut(targeted_data$minimum_nights,
                                breaks= c(0, 15, 30, 45, 60, Inf),
                                labels = c("0-15", "16-30", "31-45", "46-60", "61+"))
targeted_data$maximum_nights_range <- cut(targeted_data$maximum_nights,
                                          breaks= c(1, 90, 180, 270, 365, Inf),
                                          labels = c("1-90", "91-180", "181-270", "271-365", "366+"))
                                          
targeted_data$price <- as.numeric(targeted_data$price)
targeted_data_summary$price_range <- cut(targeted_data_summary$price,
                                          breaks= c(1, 30, 60, 90, 120, 150, 180, 210, Inf),
                                          labels = c("1-30", "31-60", "61-90", "91-120", "121-150", "151-180", "181-210", "211+"))
targeted_data_summary %>%
  filter(!is.na(price_range)) %>%
  group_by(price_range) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case) * 100  # Calculate percentage
  ) %>% 
  ggplot(aes(x = price_range, y = Percentage, fill= price_range)) + geom_bar(stat = "identity") +
  labs(x = "Price range", y = "% Listings") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees

targeted_data %>%
  group_by(minimum_nights_range) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case) * 100  # Calculate percentage
  ) %>% 
  ggplot(aes(x = minimum_nights_range, y = Percentage, fill= minimum_nights_range)) + geom_bar(stat = "identity") +
  labs(x = "Minimum nights range", y = "% Listings") +
  theme_classic()

targeted_data %>%
  group_by(name) %>%
  summarise(
    Listings = n(),
  ) %>% arrange(desc(Listings))

targeted_data$number_of_reviews_ltm_group <- cut(targeted_data$number_of_reviews_ltm,
                                          breaks= c(1, 25, 50, 75, 100, Inf),
                                          labels = c("1-25", "26-50", "51-75", "76-100", "101+"))

targeted_data %>%
  filter(!is.na(number_of_reviews_ltm_group)) %>%
  group_by(number_of_reviews_ltm_group) %>%
  summarise(
    Listings = n(),
    Percentage = (n() / total_record_selected_case) * 100  # Calculate percentage
  ) %>% 
  ggplot(aes(x = number_of_reviews_ltm_group, y = Percentage, fill= number_of_reviews_ltm_group)) + geom_bar(stat = "identity") +
  labs(x = "Review numbers (last 12 months)", y = "% Amount") +
  theme_classic()

average_minimum_nights <- targeted_data$minimum_nights %>% mean() %>% ceiling()
average_maximum_nights <- targeted_data$maximum_nights %>% mean() %>% ceiling()

targeted_data_summary <- summary_data %>%
  filter(room_type %in% c("Hotel room", "Private room"))
average_num_reviews_ltm <- targeted_data_summary$number_of_reviews_ltm %>% mean() %>% ceiling()
average_num_reviews_30d <- targeted_data$number_of_reviews_l30d %>% mean() %>% ceiling()

# Assuming you have a numeric vector named targeted_data_summary$price
average_price_per_nights <- targeted_data_summary$price[!is.na(targeted_data_summary$price)] %>% mean() %>% ceiling()

# nights booked and peak nights booked for last 12 months
average_nights_booked_ltm <- average_minimum_nights * average_num_reviews_ltm

# The estimated income for the last 12 months
average_income_regular <- average_price_per_nights * average_nights_booked_ltm

# The estimated market cap for the last 12 months
estimated_market_cap_regular <- average_income_regular * total_record_selected_case
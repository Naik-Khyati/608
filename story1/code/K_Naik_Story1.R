# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data from the GitHub URL
url <- "https://raw.githubusercontent.com/Naik-Khyati/608/main/story1/input/story1_input_data.csv"
data <- read.csv(url)


data <- data %>% mutate(party = ifelse(party == "D", "Democratic",
                      ifelse(party == "R", "Republic", "Missing"))) %>%
  mutate(party = ifelse(is.na(party), "Missing", party))

# Reorder the levels in the "party" column
data$party <- factor(data$party, levels = c("Democratic", "Republic", "Missing"))


# 1. Political Party vs. Budget Allocation
party_allocation <- data %>%
  group_by(party) %>%
  summarise(Total_Billions = sum(total_billions, na.rm = TRUE))


# Calculate the total budget allocation across all parties
total_budget <- sum(party_allocation$Total_Billions, na.rm = TRUE)

# Calculate the share of budget allocation by party
party_allocation <- party_allocation %>%
  mutate(Share = Total_Billions / total_budget)



# Create the side-by-side bar chart
ggplot(party_allocation, aes(x = "", y = Total_Billions, fill = party)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = scales::percent(Share)), position = position_dodge(width = 0.9), vjust = -0.5) +  # Adjust label positioning
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("Democratic" = "blue", "Republic" = "red", "Missing" = "gray")) +
  labs(title = "Budget Allocation by Political Party",
       x = "",
       y = "Total Budget Allocation (billions)",
       fill = "party") +
  guides(fill = guide_legend(title = "Party"))

# State Budget Allocation vs. Population
ggplot(data, aes(x = reorder(state, -`pop_est`), y = total_billions, fill = `party`)) +
  geom_bar(stat = "identity") +
  labs(title = "State Budget Allocation vs. Population",
       x = "State",
       y = "Total Budget (Billions)",
       fill = "Political Party") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Republic" = "red", "Democratic" = "blue", "Missing" = "grey")) +
    guides(fill = guide_legend(title = "Party"))


# Per Capita Budget Allocation
data$Per_Capita_Budget <- data$total_billions/data$pop_est

ggplot(data, aes(x = reorder(state, -Per_Capita_Budget), y = Per_Capita_Budget, fill = `party`)) +
  geom_bar(stat = "identity") +
  labs(title = "Per Capita Budget Allocation",
       x = "State",
       y = "Per Capita Budget (Billions)",
       fill = "Political Party") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Republic" = "red", "Democratic" = "blue","Missing"="grey"))

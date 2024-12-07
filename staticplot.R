# Install and load required packages
library(cricketdata)
library(dplyr)
library(ggplot2)
## Career ODI Average ---------------------------------------------------------
# Filtering the data for ODI matches only at Home
babar_odi_data <- babar_data %>%
  mutate(
    Home_Away = ifelse(Ground %in% c("Karachi", "Lahore", "Rawalpindi"), "Home", "Away"),
    Home_Away = as.factor(Home_Away)
  ) %>%
  filter(!is.na(Runs)) # Removing rows with missing Runs

# Structuring the date
babar_odi_career <- babar_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Checking for missing values in Date or Runs
babar_odi_career %>%
  filter(is.na(Date) | is.na(Runs)) %>%
  print()

# Calculating the average runs over the career at each match
babar_odi_career <- babar_odi_career %>%
  arrange(Date) %>% # Ensure data is sorted by date
  mutate(Cumulative_Runs = cumsum(as.numeric(Runs)),
         Matches_Played = row_number(), # Count the number of matches played up to each point
         Average_Runs = Cumulative_Runs / Matches_Played) # Calculate the average runs

#Plot
ggplot(babar_odi_career, aes(x = Date, y = Average_Runs)) +
  geom_line(color = "blue", linewidth = 1) +  # Line for career average runs
  geom_vline(xintercept = as.Date("2020-10-30"), linetype = "dashed", color = "red", linewidth = 1) +  # Mark captaincy start
  theme_minimal() +
  labs(
    title = "Babar Azam's Average Runs in ODIs Over His Career",
    x = "Date",
    y = "Average Runs",
    caption = "Data includes all ODIs played by Babar Azam"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10)
  )


## Against Opponents -------------------------------------------------------

# Filtering data for ODI matches against specific opponents
babar_vs_opponents <- babar_data %>%
  filter(Opposition %in% c("Australia", "India", "England", "New Zealand", "South Africa")) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Captaincy_Status = ifelse(Date < as.Date("2020-10-30"), "Before Captaincy", "After Captaincy"),
         Runs = as.numeric(Runs)) %>% 
  group_by(Opposition, Captaincy_Status) %>%
  summarise(Average_Runs = mean(Runs, na.rm = TRUE)) # Calculating average runs against each opponent by captaincy status

ggplot(babar_vs_opponents, aes(x = Opposition, y = Average_Runs, fill = Captaincy_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  theme_minimal() +
  labs(
    title = "Babar Azam's Average ODI Runs Against Major Opponents (Before vs After Captaincy)",
    x = "Opponent",
    y = "Average Runs",
    fill = "Captaincy Status"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = round(Average_Runs, 1)), vjust = -0.5, size = 4,
            position = position_dodge(width = 0.8)) 











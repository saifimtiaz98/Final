
# Data for regression analysis
babar_model_data <- babar_data %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),  
    Captaincy_Status = ifelse(Date >= as.Date("2020-10-30"), "Captain", "Not_Captain"),  #Captaincy status based on captaincy start date
    Home_Away = ifelse(Ground %in% c("Karachi", "Lahore", "Rawalpindi", "Multan"), "Home", "Away"),  #Classifying  matches as Home or Away
    Captaincy_Status = as.factor(Captaincy_Status),  #Captaincy_Status to factor
    Home_Away = as.factor(Home_Away)  # Home_Away to factor
  ) %>%
  filter(!is.na(Runs))  

# Regression model to evaluate performance before and during captaincy
babar_captain_model <- 
  lm(Runs ~ Captaincy_Status + Home_Away + Captaincy_Status:Home_Away, data = babar_model_data)

summary(babar_captain_model)

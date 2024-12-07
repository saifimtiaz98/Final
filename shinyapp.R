library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

# Shiny App Wins Against Oppositions and Captaincy ------------------------

odi_result <- read.csv("/Users/saifimtiaz/Desktop/Final Project/data/odi_result.csv", stringsAsFactors = FALSE)

# Converting Start.Date from text to proper Date format
odi_result$Start.Date <- as.Date(odi_result$Start.Date, format = "%d/%m/%Y")

# Adding a new column to indicate Babar Captaincy Status
odi_result <- odi_result %>%
  mutate(
    Babar_Captaincy_Status = ifelse(Start.Date >= as.Date("2020-10-30"), "Captain", "Not Captain")
  )

# UI for the Shiny app
ui <- fluidPage(
  # Add image header
  tags$div(
    tags$img(src = "https://images.icc-cricket.com/image/upload/t_ratio16_9-size40/v1730028995/prd/ye7d2yhcgnztlejgmklm", 
             style = "width: 100%; max-height: 300px;"),
    align = "center"
  ),
  
  titlePanel("Pakistan Cricket Team Winning Percentage Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "opposition", 
        "Select Opposition Team:", 
        choices = c("All Oppositions", unique(odi_result$Opposition))
      )
    ),
    
    mainPanel(
      tableOutput("summaryTable"),
      plotOutput("winPercentagePlot")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$opposition == "All Oppositions") {
      odi_result
    } else {
      odi_result %>%
        filter(Opposition == input$opposition)
    }
  })
  
  # Generating a summary table
  output$summaryTable <- renderTable({
    total_matches <- nrow(filtered_data())
    
    # Filtering matches where Babar Azam played
    babar_played <- filtered_data() %>%
      filter(!is.na(Babar_Captaincy_Status)) # Ensure Babar is accounted for
    
    # Matches where Babar was Captain
    captain_matches <- nrow(babar_played %>% filter(Babar_Captaincy_Status == "Captain"))
    captain_wins <- nrow(babar_played %>% filter(Result == "won" & Babar_Captaincy_Status == "Captain"))
    
    # Matches where Babar was Not Captain
    not_captain_matches <- nrow(babar_played %>% filter(Babar_Captaincy_Status == "Not Captain"))
    not_captain_wins <- nrow(babar_played %>% filter(Result == "won" & Babar_Captaincy_Status == "Not Captain"))
    
    data.frame(
      "Category" = c(
        "Total Matches Against Opposition",
        "Matches Where Babar Played",
        "Wins When Babar Was Captain",
        "Wins When Babar Was Not Captain"
      ),
      "Count" = c(
        total_matches,
        nrow(babar_played),
        paste(captain_wins, "out of", captain_matches, "matches"),
        paste(not_captain_wins, "out of", not_captain_matches, "matches")
      )
    )
  })
  
  # Plot for win percentage before and after Babar's captaincy
  output$winPercentagePlot <- renderPlot({
    win_percentage_data <- filtered_data() %>%
      group_by(Babar_Captaincy_Status) %>%
      summarise(
        Total_Matches = n(),
        Wins = sum(Result == "won"),
        Win_Percentage = (Wins / Total_Matches) * 100
      )
    
    # Ensuring both categories (Captain, Not Captain) are present
    all_statuses <- data.frame(Babar_Captaincy_Status = c("Captain", "Not Captain"))
    win_percentage_data <- full_join(all_statuses, win_percentage_data, by = "Babar_Captaincy_Status") %>%
      mutate(
        Total_Matches = ifelse(is.na(Total_Matches), 0, Total_Matches),
        Wins = ifelse(is.na(Wins), 0, Wins),
        Win_Percentage = ifelse(is.na(Win_Percentage), 0, Win_Percentage)
      )
    
    ggplot(win_percentage_data, aes(x = Babar_Captaincy_Status, y = Win_Percentage, fill = Babar_Captaincy_Status)) +
      geom_bar(stat = "identity", color = "black") +
      theme_minimal() +
      labs(
        title = paste("Win Percentage", 
                      ifelse(input$opposition == "All Oppositions", "Against All Teams", paste("Against", input$opposition))),
        x = "Captaincy Status",
        y = "Win Percentage (%)"
      ) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)


# Map for Toss and Winning  ---------------------------------------------------------------------

# Converting Start.Date to Date format
odi_result$Start.Date <- as.Date(odi_result$Start.Date, format = "%d/%m/%Y")

# Adding a column for Captaincy and Toss Outcome
odi_result <- odi_result %>%
  mutate(
    Captaincy_Status = ifelse(Start.Date >= as.Date("2020-10-30"), "Captain", "Not Captain"),
    Toss_Outcome = ifelse(Toss == "won", "Won Toss", "Lost Toss"),
    Toss_Outcome = factor(Toss_Outcome, levels = c("Won Toss", "Lost Toss"))
  )

# UI for Shiny App
ui <- fluidPage(
  titlePanel("Impact of Toss Wins on Match Results During Babar Azam's Captaincy"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "captaincy_filter", 
        "Select Captaincy Status:", 
        choices = c("Captain", "Not Captain"), 
        selected = "Captain"
      )
    ),
    
    mainPanel(
      plotOutput("tossImpactPlot"),
      tableOutput("tossImpactTable")
    )
  )
)

server <- function(input, output) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    odi_result %>% filter(Captaincy_Status == input$captaincy_filter)
  })
  
  # Generate the toss impact plot
  output$tossImpactPlot <- renderPlot({
    toss_data <- filtered_data() %>%
      group_by(Toss_Outcome) %>%
      summarise(
        Matches = n(),
        Wins = sum(Result == "won"),
        Win_Percentage = (Wins / Matches) * 100
      )
    
    ggplot(toss_data, aes(x = Toss_Outcome, y = Win_Percentage, fill = Toss_Outcome)) +
      geom_bar(stat = "identity", color = "black") +
      theme_minimal() +
      labs(
        title = paste("Win Percentage Based on Toss Outcome During", input$captaincy_filter),
        x = "Toss Outcome",
        y = "Win Percentage (%)",
        fill = "Toss Outcome"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
  })
  
  # Generating a summary table
  output$tossImpactTable <- renderTable({
    toss_summary <- filtered_data() %>%
      group_by(Toss_Outcome) %>%
      summarise(
        Total_Matches = n(),
        Wins = sum(Result == "won"),
        Losses = sum(Result == "lost"),
        No_Result = sum(Result == "n/r"),
        Ties = sum(Result == "tied")
      )
    
    toss_summary
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

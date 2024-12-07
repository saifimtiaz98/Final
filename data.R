# Load Required Libraries ------------------------------------------------------
# Install necessary packages if not already installed
# install.packages("cricketdata")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("rvest")

library(cricketdata)
library(dplyr)
library(tidyverse)
library(rvest)


# Setting Working Directory -------------------------------------------------------
setwd('/Users/saifimtiaz/Desktop/Final Project')



# For Static Plots --------------------------------------------------------
## Data Retrieval and Cleaning --------------------------------------------------
##Fetching Babar Azam's Match Data from Cricketdata -----------------------

babar_data <- fetch_player_data(playerid = 348144, matchtype = "odi") # Fetching data for Babar Azam using his player ID (348144) for ODIs
babar_data <- babar_data %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),  
    Home_Away = ifelse(Ground %in% c("Karachi", "Lahore", "Rawalpindi", "Multan"), "Home", "Away"),
    Captaincy_Status = ifelse(Date >= as.Date("2020-10-30"), "Captain", "Not_Captain")
  ) %>%
  filter(!is.na(Runs))  # Removing rows with missing Runs

# Saving the data for future use
write_csv(babar_data, "data/babar_data.csv")



# For Text Processing -----------------------------------------------------

# Extracting Articles Related to Babar Azam --------------------------------
# Extracting article links from ICC website
base_url <- 'https://www.icc-cricket.com/news'
url <- 'https://www.icc-cricket.com/news/team/6'
webpage <- read_html(url)

# Extracting article links
article_nodes <- html_nodes(webpage, 'a')
article_links <- html_attr(article_nodes, 'href')

# Filtering links to keep only the ones that are news articles
icc_articles <- article_links[grepl('/news/', article_links) & !grepl('/category|/team/', article_links)]

# Converting relative URLs to absolute URLs
icc_articles <- ifelse(startsWith(icc_articles, "/"), paste0(base_url, icc_articles), icc_articles)

# Extracting article titles
titles <- html_text(article_nodes[grepl('/news/', article_links) & !grepl('/category|/team/', article_links)])

# Option to use archived data or re-scrape articles
use_local <- TRUE

if (use_local && file.exists("data/articles_raw.txt")) {
  # Loading archived raw data
  article_texts <- read_lines("data/articles_raw.txt")
} else {
  # Fetching articles from the website
  get_article_text <- function(url) {
    tryCatch({
      article_page <- read_html(url)
      article_content <- html_nodes(article_page, 'p') %>% html_text() %>% paste(collapse = " ")
      return(article_content)
    }, error = function(e) {
      return(NA)
    })
  }
  article_texts <- sapply(icc_articles, get_article_text)
  
  # Removing any NA entries caused by errors in fetching articles
  article_texts <- article_texts[!is.na(article_texts)]
  
  write_lines(article_texts, "data/articles_raw.txt")
}

# Step 3: Sentiment Analysis Preparation ---------------------------------------
# Extract articles mentioning Babar Azam
babar_azam_articles <- grepl("babar|azam|captain", article_texts, ignore.case = TRUE)

# Filtering the articles and titles based on the relevant ones for Babar Azam
filtered_articles <- article_texts[babar_azam_articles]
filtered_titles <- titles[babar_azam_articles]

# Save the filtered articles
write_lines(filtered_articles, "data/filtered_articles.txt")

# Save additional metadata if needed (e.g., filtered titles)
write_lines(filtered_titles, "data/filtered_titles.txt")

# Notes for Users --------------------------------------------------------------
# If you wish to run the analysis using these pre-processed datasets,
# you can replace any web-scraping or API calls in their respective scripts
# by reading these saved files.

# Example:
# babar_data <- read_csv("data/babar_data.csv")
# filtered_articles <- read_lines("data/filtered_articles.txt")




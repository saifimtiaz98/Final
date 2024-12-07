

library(tidyverse)
library(tidytext)
library(udpipe)
library(ggraph)
library(igraph)
library(rvest)
library(SnowballC)
library(syuzhet)
library(plotly)

# Sentiment Analysis: Using AFINN Dictionary
sentiment_afinn <- get_sentiments("afinn") %>% rename(afinn = value)
# Assuming the rest of the code for data extraction and sentiment analysis is the same...

# If filtered_articles is empty, skip further processing
if (length(filtered_articles) > 0) {
  # Text Preprocessing for Sentiment Analysis
  dat <- udpipe(filtered_articles, "english")
  
  # Joining Sentiments using AFINN, Bing, and NRC lexicons
  dat <- left_join(dat, sentiment_afinn, by = c("lemma" = "word"))
  dat <- left_join(dat, sentiment_bing, by = c("lemma" = "word"))
  dat <- left_join(dat, sentiment_nrc, by = c("lemma" = "word"), relationship = "many-to-many")
  
  # Average Sentiment per Document Using Multiple Lexicons
  overall_sentiment <- dat %>%
    group_by(doc_id) %>%
    summarise(
      afinn = mean(afinn, na.rm = TRUE),
      bing_positive = sum(bing == "positive", na.rm = TRUE),
      bing_negative = sum(bing == "negative", na.rm = TRUE),
      nrc_positive = sum(nrc == "positive", na.rm = TRUE),
      nrc_negative = sum(nrc == "negative", na.rm = TRUE)
    ) %>%
    mutate(title = filtered_titles)
  
  # Filtering out any articles without sentiment data
  overall_sentiment <- overall_sentiment %>%
    filter(!is.na(afinn))
  
  # Assigning simpler labels for the x-axis
  overall_sentiment$short_title <- paste("Article", seq_len(nrow(overall_sentiment)))
  
  # Average Sentiment for Each Article Using AFINN with Shorter Labels
  p <- ggplot(data = overall_sentiment) +
    geom_col(aes(x = reorder(short_title, afinn), y = afinn, fill = afinn > 0), show.legend = FALSE) +
    labs(x = "Article", y = "Sentiment", title = "Average Sentiment (AFINN) for Babar Azam Articles") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  
  # Adding a tooltip for the full title (if using ggplotly)
  ggplotly(p, tooltip = c("y", "x", "title"))
} else {
  print("No articles found mentioning Babar Azam.")
}



# Extracting Words Most Associated with Sentiment Lexicons
affin_associated_words <- dat %>%
  filter(!is.na(afinn)) %>%
  group_by(lemma) %>%
  summarise(mean_afinn = mean(afinn, na.rm = TRUE)) %>%
  arrange(desc(mean_afinn))

bing_associated_words <- dat %>%
  filter(!is.na(bing)) %>%
  count(lemma, bing, sort = TRUE)

nrc_associated_words <- dat %>%
  filter(!is.na(nrc)) %>%
  count(lemma, nrc, sort = TRUE)

# Top Words Associated with Each Sentiment Lexicon
print("Top Words Associated with AFINN Sentiment")
print(head(affin_associated_words, 10))

print("Top Words Associated with Bing Sentiment")
print(head(bing_associated_words, 10))

print("Top Words Associated with NRC Sentiment")
print(head(nrc_associated_words, 10))

summary_stats <- overall_sentiment %>% 
  summarise(
    avg_afinn = mean(afinn, na.rm = TRUE),
    median_afinn = median(afinn, na.rm = TRUE),
    min_afinn = min(afinn, na.rm = TRUE),
    max_afinn = max(afinn, na.rm = TRUE),
    total_bing_positive = sum(bing_positive, na.rm = TRUE),
    total_bing_negative = sum(bing_negative, na.rm = TRUE),
    total_nrc_positive = sum(nrc_positive, na.rm = TRUE),
    total_nrc_negative = sum(nrc_negative, na.rm = TRUE)
  )

print(summary_stats)


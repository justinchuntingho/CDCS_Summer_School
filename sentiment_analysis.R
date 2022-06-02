# Installing the packages
install.packages("quanteda")
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
install.packages("dplyr")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("lubridate")


# Loading the necessary packages
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(dplyr)
library(lubridate)

# =================================== Basic Text Analysis =======================================

# Loading the documents
df <-  read.csv("data/news.csv")
df$datetime <- as.POSIXct(df$date, format = "%d %B %Y %H:%M")
df <- filter(df, datetime > "2016-01-01")

news_corpus <- corpus(df, text_field = "text")

# Defining custom stopwords
customstopwords <- c("s", "http", "£")

# Creating DFM
news_tokens <- tokens(news_corpus, remove_punct = TRUE, remove_numbers = TRUE, verbose = TRUE, remove_url = TRUE)
news_dfm <- dfm(news_tokens)
news_dfm <- dfm_remove(news_dfm, c(stopwords('english'), customstopwords))


# Inspecting the results
topfeatures(news_dfm, 30) 

# Wordcloud
textplot_wordcloud(news_dfm)

# =================================== Sentiment Analysis =======================================

# We will use the Lexicoder Sentiment Dictionary (2015)
data_dictionary_LSD2015

# Passing the dictionary to the dictionary function, you can also define your own dictionary
sentiment <- tokens_lookup(news_tokens, data_dictionary_LSD2015) %>% 
  dfm()
sentiment <- convert(sentiment, to = "data.frame")
sentiment["sentiment"] <- sentiment$positive + sentiment$neg_negative - sentiment$negative - sentiment$neg_positive

# Merging the result with the original dataset
df_wsentiment <- cbind(df, sentiment)

# Basic Exploration: mean sentiment score
mean(df_wsentiment$sentiment)

# What are the happiest articles?
df_wsentiment %>% slice_max(order_by = sentiment, n = 10) %>% View()

# What are the most depressing articles?
df_wsentiment %>% slice_min(order_by = sentiment, n = 10) %>% View()







# Sentiment by hour?
df_wsentiment$hour <- hour(df_wsentiment$datetime)


ggplot(df_wsentiment, aes(hour, sentiment, color = as.factor(hour))) +
  geom_jitter(alpha = 0.3)

plot_df <- df_wsentiment %>% 
  group_by(hour) %>% 
  summarise(sentiment = mean(sentiment))
ggplot(plot_df, aes(hour, sentiment)) +
  geom_col()

# Here we are going to use a function to aggregate by week/month
plot_df <- df_wsentiment %>% 
  mutate(date = floor_date(datetime, "week")) %>% 
  group_by(date) %>% 
  summarise(sentiment = mean(sentiment))
ggplot(plot_df, aes(date, sentiment)) +
  geom_line()



# =============================== Analysis with NRC Lexicon ===================================

install.packages("tidytext")
install.packages("textdata")

# Tidytext is another package that do text analsis, there is a nice function that helps you to download other sentiment lexica
# NRC is another commonly used lexicon, it has eight basic emotions on top of sentiments
nrc <- tidytext::get_sentiments("nrc")
unique(nrc$sentiment)

# We need to talk the words and put them into separated vectors
nrc_pos <- nrc$word[nrc$sentiment == "positive"]
nrc_neg <- nrc$word[nrc$sentiment == "negative"]
nrc_trust <- nrc$word[nrc$sentiment == "trust"]
nrc_fear <- nrc$word[nrc$sentiment == "fear"]
nrc_sadness <- nrc$word[nrc$sentiment == "sadness"]
nrc_anger <- nrc$word[nrc$sentiment == "anger"]
nrc_surprise <- nrc$word[nrc$sentiment == "surprise"]
nrc_disgust <- nrc$word[nrc$sentiment == "disgust"]
nrc_joy <- nrc$word[nrc$sentiment == "joy"]
nrc_anticipation <- nrc$word[nrc$sentiment == "anticipation"]

# This is how we define our own dictionary: supplying a labelled list of vectors
nrc_dict <- dictionary(list(nrc_positive = nrc_pos,
                            nrc_negative = nrc_neg,
                            trust = nrc_trust,
                            fear = nrc_fear,
                            sadness = nrc_sadness,
                            anger = nrc_anger,
                            surprise = nrc_surprise,
                            disgust = nrc_disgust,
                            joy = nrc_joy,
                            aniticipation = nrc_anticipation))

# Passing the dictionary to the dictionary function
sentiment <- tokens_lookup(news_tokens, nrc_dict) %>% 
  dfm()
sentiment <- convert(sentiment, to = "data.frame")
sentiment["nrc_sentiment"] <- sentiment$nrc_positive - sentiment$nrc_negative

# Merging the result with the original dataset
df_wsentiment_wnrc <- cbind(df_wsentiment, sentiment[-1])

# Let's compare mean sentiment score
mean(df_wsentiment_wnrc$sentiment)
mean(df_wsentiment_wnrc$nrc_sentiment)

# What are the happiest articles?
df_wsentiment_wnrc %>% slice_max(order_by = nrc_sentiment, n = 10) %>% View()

# What are the most depressing articles?
df_wsentiment_wnrc %>% slice_min(order_by = nrc_sentiment, n = 10) %>% View()

# Sentiment by hour?
df_wsentiment_wnrc$hour <- hour(df_wsentiment_wnrc$datetime)

ggplot(df_wsentiment_wnrc, aes(hour, nrc_sentiment, color = as.factor(hour))) +
  geom_jitter(alpha = 0.3)

# Another visualization
plot_df <- df_wsentiment_wnrc %>% 
  group_by(hour) %>% 
  summarise(nrc_sentiment = mean(nrc_sentiment))
ggplot(plot_df, aes(hour, nrc_sentiment)) +
  geom_col()

# Time trend again
plot_df <- df_wsentiment_wnrc %>% 
  mutate(date = floor_date(datetime, "week")) %>% 
  group_by(date) %>% 
  summarise(nrc_sentiment = mean(nrc_sentiment))
ggplot(plot_df, aes(date, nrc_sentiment)) +
  geom_line()

# Other emotions?
library(tidyr)

# Quite a bit of data wrangling here, to learn more, sign up for the next training on tidyverse!
plot_df <- df_wsentiment_wnrc %>% 
  mutate(date = floor_date(datetime, "month")) %>% 
  select(date, trust:aniticipation) %>% 
  pivot_longer(trust:aniticipation, names_to = "emotion") %>%
  group_by(emotion, date) %>% 
  summarise(value = mean(value))

ggplot(plot_df, aes(date, value, color = emotion, group = emotion)) +
  geom_line() +
  facet_wrap(~emotion)

# We can also use more advanced technique to explore the time series data
df_wsentiment_wnrc %>% 
  mutate(date = floor_date(datetime, "month")) %>% 
  select(date, trust:aniticipation) %>% 
  pivot_longer(trust:aniticipation, names_to = "emotion") %>% 
  ggplot(aes(date, value, color = emotion, group = emotion)) +
  geom_smooth(method = "loess")


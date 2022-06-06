# SENTIMENT ANALYSIS ##########

## Install & Load Required Packages =====

# Install 
install.packages("tidyverse")
install.packages("tm")
install.packages("quanteda")
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("lubridate")
install.packages('quanteda.textmodels')
install.packages('quanteda.sentiment')

# if the installation of quanteda.sentiment fails you will need to do it via devtools
install.packages("devtools")
library(devtools)
devtools::install_github("quanteda/quanteda.sentiment")
devtools::install_github("quanteda/quanteda.textmodels") 


# Load
library(tidyverse)
library(tm)
library(quanteda)
library(wordcloud)
library(syuzhet)
library(tidyr)
library(tidytext)
library(lubridate)
library(quanteda.textmodels)
library(quanteda.sentiment)


# Load our data =====
news_df <- read.csv("data/news.csv")
news_df$datetime <- as.POSIXct(news_df$date, format = "%d %B %Y %H:%M")
news_df <- filter(news_df, datetime > "2016-01-01")

# View
news_df 

## The quanteda approach =====

# First we need to tokenize our object 
news_tokens <- news_df %>% 
  corpus() %>%
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    verbose = TRUE
  ) %>% 
  tokens_remove(pattern = stopwords("en"))

head(news_tokens)

# Quanteda comes with a dictionary of positive, negative and neutral words built in
# Let's have one of these
lengths(data_dictionary_LSD2015)
data_dictionary_LSD2015

# We set the positive and negative words, so we can calculate polarity scores
polarity(data_dictionary_LSD2015) <- 
  list(pos = c("positive", "neg_negative"), neg = c("negative", "neg_positive"))


# Now we can calculate the polarity of each text in toks, to get a sense of how positive vs negative each text is
news_polarity <- textstat_polarity(news_tokens, data_dictionary_LSD2015)
news_polarity

# And plot it
ggplot(data = news_polarity, aes(x = sentiment, y = doc_id, colour=sentiment)) +
  geom_point()+
  theme_bw()+
  geom_vline(xintercept = 0, colour="red", size=1)+
  scale_colour_gradientn(colors=rainbow(7))

# We can combine this with the original dataset to do more analysis
df_wsentiment <- cbind(news_df, news_polarity)

# What are the happiest articles?
df_wsentiment %>% slice_max(order_by = sentiment, n = 10) %>% View()

# What are the most depressing articles?
df_wsentiment %>% slice_min(order_by = sentiment, n = 10) %>% View()


# Sentiment by hour?
df_wsentiment$hour <- hour(df_wsentiment$datetime)

ggplot(df_wsentiment, aes(hour, sentiment, color = as.factor(hour))) +
  geom_jitter(alpha = 0.3)


plot_df <- df_wsentiment %>% 
  mutate(date = floor_date(datetime, "week")) %>% 
  group_by(date) %>% 
  summarise(sentiment = mean(sentiment))
ggplot(plot_df, aes(date, sentiment)) +
  geom_line()


# We can also look at the valence rather than the polarity of the text
# Let's look at the ANEW dictionary (effective Norms for English Words), which provides a lexicon of 2,471 distinct fixed word matches that are associated with three valences categories: pleasure, arousal, and dominance

data_dictionary_ANEW

head(valence(data_dictionary_ANEW)$pleasure)
head(valence(data_dictionary_ANEW)$arousal)
head(valence(data_dictionary_ANEW)$dominance)


# The best approach here is to look at the score for each valuence category
pleasure <- textstat_valence(news_tokens, data_dictionary_ANEW["pleasure"])
pleasure
names(pleasure)[2] <- "pleasure"

arousal <- textstat_valence(news_tokens, data_dictionary_ANEW["arousal"])
arousal
names(arousal)[2] <- "arousal"

dominance <- textstat_valence(news_tokens, data_dictionary_ANEW["dominance"])
dominance
names(dominance)[2] <- "dominance"

df_wsentiment <- left_join(df_wsentiment, pleasure, by = "doc_id")
df_wsentiment <- left_join(df_wsentiment, arousal, by = "doc_id")
df_wsentiment <- left_join(df_wsentiment, dominance, by = "doc_id")


ggplot(df_wsentiment, aes(datetime, pleasure, color = pleasure)) +
  geom_jitter(alpha = 0.3)
ggplot(df_wsentiment, aes(datetime, arousal, color = pleasure)) +
  geom_jitter(alpha = 0.3)
ggplot(df_wsentiment, aes(datetime, dominance, color = pleasure)) +
  geom_jitter(alpha = 0.3)

## Final exercise for text analysis =====
# In groups of 3+ 
#1. Collect any text data from anywhere on the internet
#2. Use any of the techniques from text analysis to learn about the content of that text
#3. Use one of the sentiment analysis approaches from this afternoon to learn something about the sentiment of the text
#4. Share your results with the group and in the Slack Channel Day 2

#### THE END ######

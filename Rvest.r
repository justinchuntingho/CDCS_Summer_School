# INTRODUCTION TO WEB SCRAPING WITH R (RVest) 
# Day 2: CDCS Data and Text Analysis Summer School 2022 ############

## What is Web Scraping? =============

# The process of extracting element from a website's structure and downloading them for analysis. Basically, we work with the structure of a website to navigate around it and pick out the parts we want. 

# This is really messy, since every website is different, their structure may not be perfectly consistent, and sometimes they will intentionally make things hard for scrapers. Additionally, a common problem (one I even faced while writing this course and my previous course on web scraping) is that websites change all the time, what you do at t1 may not work at t2. So web scraping requires a lot of tinkering and workarounds. It's way less clean than a lot of coding. 

# There are generally three kinds of data scraping from online sources:
#    1. Requesting content from an API (this is what most Twitter researchers do)
#    2. Web crawling and scraping
#    3. Web browser automation (ie. Selenium or Pupeteer; useful in adversarial scraping)

# Today, we will cover the second technique.

# Note about ethical issues: Just because you can scrape, doesn't mean you should.

# Every time you scrape a website, ask yourself:
#    1. Do you need to inform and get consent from the website owners/creators of the content you're scraping?
#    2. Who stands to benefit from your research?
#    3. What will you do with the data? How will you present it? Will you share it, and how/with whom?


## Let's Get Started============
# If you have forgotten to install the packages needed run the next line
install.packages("tidyverse")
install.packages("rvest")

### Load requested packages
library(tidyverse)
library(rvest)

## The Basics of Rvest ===================

# The first part of web scraping with Rvest is usually to download the HTML from your desired webpage. This can be done using the read_html function, with using the website's URL as the first parameter

wpage <- read_html('https://www.r-bloggers.com/')

# We can now look at this object
wpage

# The next step is to extract information from this html object. 

# RVest involves using tidyverse-style programming (functions chained together using pipes) to identify
#    1. the CSS selector of the element you want
#    2. their attributes
# and then feed this into a variable. 

# To make life easier, we recommend using SelectorGadget (you need chrome to use this): https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb?hl=en

# Here is a simple example
# We identify the css selector of the logo for our html page, identify it as text, and create a variable
LogoDesc <- wpage %>% 
  html_node('.logo-desc') %>% 
  html_text()
# n.b.this is a pipe if you want to discover more about pipes in R have a look here https://cfss.uchicago.edu/notes/pipes/

# Now quick print the result
LogoDesc

# Changing node to nodes will grab all of the elements with this attribute and create a list object with them

titles <- wpage %>% 
  html_nodes('.loop-title a') %>% 
  html_text()
titles

# We can also grab the images from a website by choosing their html selectors and using the 'src' attribute. 

images <- wpage %>% 
  html_nodes('img') %>% 
  html_attr('src')

# Let's look at the first two elements 
images[1:2]

# And now download the first two of these images as jpg files
download.file(images[2], c('z.jpg'), mode = 'wb')

# We can also do this for all the image links we have selected 

# R doesn't have a good built-in way to do this, but we can write a function for it
download.images <- function(urls){ 
  for (urlNr in 1:length(urls)){ # Simple for loop to iterate over our list
    download.file(urls[urlNr], destfile = paste0(urlNr,'.jpg'), # Download each of these using our function from earlier, pasting the number in the list to .jpg. You can also specify the dir as the first argument of paste0
                  mode="wb")
  }
}

# And now run the function
download.images(images)

# The other vital thing to know is how to select links from a website. This allows us to navigate through a webpage by moving through its URLS

# The attribute for links is going to be 'href'

# Here we can select the first link to an article on our webpage, and then grab the html for this sub-page

article <- wpage %>% 
  html_node('.loop-title a') %>% 
  html_attr('href') 
article

article_html <- read_html(article)
article_html %>%
  html_text()

### Exercise 1-------------

#1. Pick any website
#2. Download the website's html
#3. Select any element on the website and create an object out of it
#4. Share it with the group

# Space for your work
# When you are done paste your code on the Slack channel Day 2 




# ########################################

## Scrape Your first webpage ===========

# Moving on to the Scottish Government website: https://www.gov.scot/news/
# In the previous example, we found what we need by locating a page that contains a list of links
# This example will introduce another approach: manipulating the url: https://www.gov.scot/news/?page=1

# Here we need to do two things:
# 1. Iterate through all the result pages to get all the links to the news item
# 2. Iterate through all news item and get the information we need

# Let's start with one page (always a good idea to start small)
read_html("https://www.gov.scot/news/?page=1") %>% 
  html_nodes(".ds_search-result__link") %>% 
  html_attr("href")

# Now we can get the links on page 1, what about other pages?
# Note we could change the page number in the url

all_pages <- paste0("https://www.gov.scot/news/?page=",1:641)

links <- c()
for (i in all_pages[1:3]){ # Here we only scrape page 1 to 3 (to save time)
  cat("Page: ", i, "\n") # Always a good idea to know the progress
  new_link <- read_html(i) %>% 
    html_nodes(".ds_search-result__link") %>% 
    html_attr("href")
  links <- c(links, new_link)
}

# Note these are "relative links", ie they are relative to the page we were at. 
# We need to add back the first half of the url
links <- paste0("https://www.gov.scot",links)

# Let's start with one page again
read_html(links[1]) %>% 
  html_nodes(".ds_layout__content") %>% 
  html_text()

# When it works on one page, it is time for loop
# Note we are getting multiple nodes in the loop and saving it to a dataframe (ie spreadsheet format)
news <- c()
for (i in links){ 
  cat("Page: ", i, "\n") # Always a good idea to know the progress
  html <- read_html(i)
  title <- html %>% html_nodes(".ds_page-header__title") %>% html_text()
  meta <- html %>% html_nodes(".ds_metadata__value") %>% html_text()
  content <- html %>% html_nodes(".ds_layout__content") %>% html_text()
  
  new_row <- c(i, title, meta, content) # Constructing a new row
  news <- rbind(news, new_row) # Bind the new row with the existing dataframe
}

colnames(news) <- c("url", "title", "date", "theme", "text") # Renaming the column names
write.csv(news, "news.csv", row.names = FALSE) # Saving as a csv file


## Web scraping in other languages ======

# You can also use RVest to scrape multiple languages. 

### Using a German website ------
# Here, we can scrape the web page of the German Federal Office for Migration and Refugees, and the website of the right-wing ruling part of Poland, Law and Justice

#RVest doesn't care what language a web page is in, so we can use the same process as above.

# First the BAMF website
GermanMainpage <- read_html('https://www.bamf.de/DE/Startseite/startseite_node.html')
GermanMainpage

GermanPages <- GermanMainpage %>% 
  html_nodes('.c-sitemap__link') %>% 
  html_attr('href')

GermanPages

GermanPageLinks <- paste0('https://www.bamf.de/', GermanPages)


GermanDesc <- c()
for (i in GermanPageLinks[1:4]){ # Here we only scrape page 1 to 4 (to save time)
  cat("Page: ", i, "\n") # Always a good idea to know the progress
  new_text <- read_html(i) %>% 
    html_nodes(".c-page-header__text p") %>% 
    html_text()
  GermanDesc <- c(GermanDesc, new_text)
}

GermanTopics <- c()
for (i in GermanPageLinks[1:4]){ # Here we only scrape page 1 to 4 (to save time)
  cat("Page: ", i, "\n") # Always a good idea to know the progress
  new_text <- read_html(i) %>% 
    html_nodes(".c-page-header__heading") %>% 
    html_text()
  GermanTopics <- c(GermanTopics, new_text)
}


### Exercise 2:-----------

# 1. Pick another website, in any other language than English.
# 2. Using the methods we just learned, scrape any elements from at least two pages on this website.
# 3. Share it with the group.


### Space for your work. Once you are done share your code on the Day 2 Channel on slack




##### THE END ################




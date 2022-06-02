install.packages("rvest")
install.packages("magrittr")
install.packages("stringr")

library(rvest)
library(magrittr)
library(stringr)

#################################################### SPS Website #################################################### 

# For every webscraping task, you need do three things:
# 1. Download the html
# 2. Select the node that contains the information you want
# 3. Parse the html and get either: A. the text or B. more links

# Consider Scenerio A: you have a link that contains all the information you need
# For example, if you would like to scrape the name of all SPS people from the website: http://www.sps.ed.ac.uk/people

# Step 1: Download the html
html <- read_html("https://www.sps.ed.ac.uk/people")

# Step 2: Identify the node that contains the information you need
# There are many ways to do it, you could read the raw html to see the "class" or "id", or use SelectorGadget
# In this case, you could identify the texts by CSS "#main .h5" 

# Step 3: Parse the codes
# Selecting the texts and extract them
html %>% html_nodes("#main .h5") %>% html_text()

# To tidy up the texts, you could pass the result to the trimws() function, which removes all space and \n tags
html %>% html_nodes("#main .h5") %>% html_text() %>% trimws()

# Scenerio B: what if you are interested in what they are saying in their staff profile? Eg email address?
# You need to get the links. To get links, use html_attr("href") instead

# Second approach, selecting the links by adding "a" after a space, it means selecting only the <a> tags.
# To get links, use html_attr("href") instead

html %>% html_nodes(".bc-mandarine") %>% html_attr("href")

# To be able to further use the link, you need to assign it to an object
people_links <- html %>% html_nodes(".bc-mandarine") %>% html_attr("href") %>% na.omit()

# Note these are "relative links", ie they are relative to the page we were at. We need to add back the first half of the url
people_links <- paste0("https://www.sps.ed.ac.uk",people_links)

# Now we have the links ready, let's scrape one of them, we use [1] (slicing operator) to take the first one
people_links[1]

profile_html <- read_html(people_links[1])

# Now we have identify the node for the email: ".field_staff_email"
profile_html %>% html_nodes(".field_staff_email") %>% html_text()


# It works, but it would take forever to do that one by one
# We need to write a "loop" to do step 1 to 3 for EVERY link
# Below shows how a loop works: 
# `i` is the iterator variable, it will be replaced by the elements in `people_links` when the loop runs
for (i in people_links){ 
  print(i)
}

emails <- c()
for (i in people_links[1:5]){ 
  new_email <- read_html(i) %>% 
    html_nodes(".field_staff_email") %>% 
    html_text()
  emails <- c(emails, new_email)
}

titles <- c()
for (i in people_links[1:5]){ 
  new_title <- read_html(i) %>% 
    html_nodes(".h4") %>% 
    html_text()
  titles <- c(titles, new_title)
}

data.frame(emails, title)

#################################################### Scottish Government #################################################### 

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

# Again, relative links
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


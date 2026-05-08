# load libraries
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(dplyr)
library(tidyr)

# get a list of the files in the input directory
input_folder <- "State of the Union Corpus (1790 - 2018)/sotu"
files <- list.files(input_folder)

# stick together the path to the file & 1st file name
fileName <- glue("{input_folder}/{files[1]}")
# get rid of any sneaky trailing spaces
fileName <- trimws(fileName)

# read in the new file
fileText <- glue(read_file(fileName))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 


# tokenize
# got a data_frame() warning so fixed the code
tokens <- tibble(text = fileText) %>% unnest_tokens(word, text)
sentiments <- tibble()

# get the sentiment from the first text: 
tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words

# write a function that takes the name of a file and returns the # of positive
# sentiment words, negative sentiment words, the difference & the normalized difference
GetSentiment <- function(file) {
  fileName <- file.path(input_folder, file)
  
  fileText <- read_file(fileName) %>%
    gsub("\\$", "", .) %>%
    gsub("[^[:alnum:]\\s]", " ", .) %>%
    gsub("\\s+", " ", .)
  
  tokens <- tibble(text = fileText) %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_to_lower(word))
  
  sentiment_counts <- tokens %>%
    inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  
  if (!"positive" %in% colnames(sentiment_counts)) sentiment_counts$positive <- 0
  if (!"negative" %in% colnames(sentiment_counts)) sentiment_counts$negative <- 0
  
  sentiment_counts %>%
    mutate(
      sentiment = positive - negative,
      file = file,
      year = as.numeric(str_extract(file, "\\d{4}")),
      president = str_match(file, "(.*?)_")[2]
    )
}

# test: should return
# negative	positive	sentiment	file	year	president
# 117	240	123	Bush_1989.txt	1989	Bush
# Note** it's returning Adams_1797 because it is the first .txt in folder
GetSentiment(files[1])

# file to put our output in
sentiments <- data_frame()

# get the sentiments for each file in our dataset
for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

# disambiguate Bush Sr. and George W. Bush 
# correct president in applicable rows
bushSr <- sentiments %>% 
  filter(president == "Bush") %>% # get rows where the president is named "Bush"...
  filter(year < 2000) %>% # ...and the year is before 200
  mutate(president = "Bush Sr.") # and change "Bush" to "Bush Sr."

# remove incorrect rows
sentiments <- anti_join(sentiments, sentiments[sentiments$president == "Bush" & sentiments$year < 2000, ])

# add corrected rows to data_frame 
sentiments <- full_join(sentiments, bushSr)

# summarize the sentiment measures
summary(sentiments)

# plot of sentiment over time & automatically choose a method to model the change
ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model

# plot of sentiment by president
ggplot(sentiments, aes(x = president, y = sentiment, color = president)) + 
  geom_boxplot() # draw a boxplot for each president

# is the difference between parties significant?
# get democratic presidents & add party affiliation
democrats <- sentiments %>%
  filter(president %in% c("Clinton","Obama")) %>%
  mutate(party = "D")

# get democratic presidents & party add affiliation
republicans <- sentiments %>%
  filter(!president %in% c("Clinton","Obama")) %>%
  mutate(party = "R")

# join both
byParty <- full_join(democrats, republicans)

# the difference between the parties is significant
t.test(democrats$sentiment, republicans$sentiment)

# plot sentiment by party
ggplot(byParty, aes(x = party, y = sentiment, color = party)) + geom_boxplot() + geom_point()


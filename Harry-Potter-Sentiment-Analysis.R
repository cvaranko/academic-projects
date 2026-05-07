# Download package from Github
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")

# Load libraries
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(harrypotter)    # provides the first seven novels of the Harry Potter series



# First two chapters
philosophers_stone[1:2]

# Sentiment data sets
sentiments

# to see the individual lexicons try
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# Basic Sentiment Analysis
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))

series

# NRC sentiment data set 
series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

# Index that breaks up each book by 500 words
# Assesses the positive vs negative sentiment of each word
# Counts how many positive and negative words there are for every two pages
# Calculate nt sentiment
# Plots data
series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")

# Comparing sentiments
afinn <- series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(book, index) %>%
  summarise(sentiment = sum(value)) %>%  # had to change 'score' to 'value' from the tutorial for code to work
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(series %>%
                            group_by(book) %>% 
                            mutate(word_count = 1:n(),
                                   index = word_count %/% 500 + 1) %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),
                          series %>%
                            group_by(book) %>% 
                            mutate(word_count = 1:n(),
                                   index = word_count %/% 500 + 1) %>%
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentiment %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(book, method, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  select(book, index, method, sentiment)

# Bind the net sentiments together and plot
bind_rows(afinn, 
          bing_and_nrc) %>%
  ungroup() %>%
  mutate(book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_grid(book ~ method)

# Common Sentiment Words
bing_word_counts <- series %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# View top n words for each sentiment visually
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# Sentiment analysis with larger units
tibble(text = philosophers_stone) %>% 
  unnest_tokens(sentence, text, token = "sentences")

# breaking up philosophers_stone text by chapter and sentence
ps_sentences <- tibble(chapter = 1:length(philosophers_stone),
                       text = philosophers_stone) %>% 
  unnest_tokens(sentence, text, token = "sentences")

# sentiments by chapter
book_sent <- ps_sentences %>%
  unnest_tokens(word, sentence) %>%          # had tokenize first, kept getting an error in 'group_by()'
  group_by(chapter) %>%
  mutate(sentence_num = row_number(),
    index = round(sentence_num / n(), 2)) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter, index) %>%
  summarise(sentiment = sum(value, na.rm = TRUE), .groups = "drop") %>% # again had to change 'score' to 'value'
  arrange(desc(sentiment))

book_sent

# visualization (heatmap) of sentiment as we progress through each chapter
ggplot(book_sent, aes(index, factor(chapter, levels = sort(unique(chapter), decreasing = TRUE)), fill = sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2() +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Chapter Progression", y = "Chapter") +
  ggtitle("Sentiment of Harry Potter and the Philosopher's Stone",
          subtitle = "Summary of the net sentiment score as you progress through each chapter") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")
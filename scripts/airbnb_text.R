## File name: airbnb_text.R
## Project: Analysis of Airbnb dataset (listing prices neighborhood characteristics in NYC)
## Course: Programming for Data Analytics (Winter Semester 2026), Uni zu Köln

## Tasks: text analysis of Airbnb listing names and host names

library(here)
library(tidyverse)
library(tidytext)
library(stopwords)
library(kableExtra)
library(ggwordcloud)
library(conflicted)
conflict_prefer_all("dplyr") 

# load data
airbnb_merged <- read_csv(here("learning/data_output/airbnb_merged.csv"))

############################## TEXT CLEANING ################################

# create a df with the names and relevant variables, clean the text
listings_text <- airbnb_merged %>%
  select(id, name, price, log_price, neighb_borough, property_type) %>% 
  filter(name != "empty") %>%  # missing names were replaced by "empty" while merging
  mutate(
    name_clean = name %>%
      str_to_lower() %>% # 
      str_replace_all("[^a-z0-9 ]", " ") %>% # remove punctuation, emojis, symbols
      str_squish() # remove double spaces
  )

###### Summary stats
# add a column with number of words per listing
listings_text <- listings_text %>% 
  mutate(n_words = str_count(name_clean, "\\S+")) # count 1+ characters

summary(listings_text$n_words) ## median - 6, mean - 6.1, max - 30
cor(listings_text$n_words, listings_text$log_price) # corr. with log price = 0.07


############################### TOKENIZATION #################################

#stop words (added irrelevant that are typical signals of the location)
stop_en <- tibble(word = c(stopwords::stopwords("en"),
                           "w", "one", "side", "upper", "east", "west"))

# tokenize text (1 word per column)
tokens <- listings_text %>%
  unnest_tokens(word, name_clean) %>% 
  anti_join(stop_en, by = "word") %>%  # drop stop words
  filter(!str_detect(word, "^[0-9]+$")) # drop tokens consisting out of numbers

############################### FREQUENT WORDS ###############################
# check top 20 words by frequency
top_words <- tokens %>%
  count(word, sort = TRUE) %>%
  head(20)
top_words

# check top words across boroughs
top_words_borough <- tokens %>%
  count(neighb_borough, word, sort = TRUE) %>%
  group_by(neighb_borough)
top_words_borough

# create a vector with common words to highlight them in a plot
common_words <- c("room", "private", "bedroom", "apartment", "cozy")
# plot top 10 words per borough + highlight common ones
top_words_borough_plot <- tokens %>%
  count(neighb_borough, word) %>%
  group_by(neighb_borough) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(common = word %in% common_words) %>%  # TRUE if the word is in the vector
  mutate(word = reorder_within(word, n, neighb_borough)) %>%
  ggplot(aes(x = n, y = word, fill = common)) +
  geom_col() +
  scale_y_reordered() +
  scale_fill_manual(values = c("TRUE" = "steelblue3", "FALSE" = "grey78")) +
  facet_wrap(~ neighb_borough, scales = "free") +
  theme_airbnb +
  theme(
    legend.position = "none") +
  labs(title = "Most frequent words per borough",
       subtitle = "Words in blue are in top 10 of all boroughs",
       x = "Number of occurrences", y = NULL)
top_words_borough_plot
ggsave("learning/fig_output/top_words_borough_plot.png", plot = top_words_borough_plot,
       bg = "white", dpi = 300, width = 8, height = 6)


###################################### TF-IDF #################################

# compute tf, idf, tf-idf
    ## tf - share of a word among all words in a borough
    ## idf - how rare a word is across boroughs
# bind_tf_idf(word column, document column, count column)
tfidf_borough <- tokens %>%
  count(neighb_borough, word, sort = TRUE) %>%
  bind_tf_idf(word, neighb_borough, n) %>% 
  arrange(desc(tf_idf))
tfidf_borough

tfidf_plot <-  tfidf_borough %>%
  group_by(neighb_borough) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, neighb_borough)) %>%
  ggplot(aes(x = tf_idf, y = word)) +
  geom_col(fill = "steelblue3") +
  scale_y_reordered() +
  facet_wrap(~ neighb_borough, scales = "free") +
  theme_airbnb +
  theme(
    legend.position = "none") +
  labs(title = "Most distinctive words per borough (tf-idf)", x = "tf-idf", y = NULL)
tfidf_plot
ggsave("learning/fig_output/tfidf_plot.png", plot = tfidf_plot,
       bg = "white", dpi = 300, width = 8, height = 6)

## descriptive statistics for the listings names
name_length_table <- listings_text %>%
  filter(n_words > 0) %>%
  summarise(
    `N of listings` = n(),
    `min` = min(n_words),
    `median` = median(n_words),
    `mean` = round(mean(n_words), 1),
    `max` = max(n_words),
    `corr. with log price` = round(cor(n_words, log_price), 2)
  )
name_length_table

# save in latex format
name_length_table %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = "c",
    caption = "Number of words in listing names",
    label = "name_length"
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("learning/documents/name_length_table.tex")

######################## WORDCLOUD WITH HOST NAMES ##############################

# number of listings per host name (counts listings)
host_names <- airbnb_merged %>%
  filter(!is.na(host_name)) %>%
  count(host_name, sort = TRUE) %>%
  head(500) 

set.seed(4)
host_wordcloud <- host_names %>%
  ggplot(aes(label = host_name, size = n, color = n, alpha = n)) +
  geom_text_wordcloud(rm_outside = TRUE, grid_margin = 0.4) +  
  scale_radius(range = c(1, 17)) + 
  scale_alpha(range = c(0.5, 1)) + 
  scale_color_gradient(low = "dodgerblue4", high = "firebrick2") +
  theme_airbnb +
  theme(legend.position = "none") +
  labs(title = "Most common host names in Airbnb listings")
host_wordcloud
ggsave("learning/fig_output/host_wordcloud.png", plot = host_wordcloud,
       bg = "white", dpi = 300, width = 6, height = 3)

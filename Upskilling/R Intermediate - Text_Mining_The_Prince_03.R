####################################################################
# Text Mining & Sentient Analysis
# The Prince - Analysis
# by: Luis Urso                                                    #
# version: 1.0 - Feb, 17th 2022
# Credit to: Gutenberger open books database
####################################################################


###
# Packages Installations
###

pacotes <- c('dplyr',
             'tidytext',
             'gutenbergr',
             'ggplot2',
             'ggthemes',
             'stringr',
             'tidyr',
             'wordcloud',
             'reshape2'
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

###
# Creates the The Prince book from Gutenberg Project free books database
##

prince <- gutenberg_download(c(1232))

# Removes the front and back matter 
prince


prince <- prince[-c(1:88), ]

###
# Tokenize the Dataset 1 work per row making the reference to the row
# and removing the signals and pontuations, lowercasing.
###


tidy_data <- prince %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

tidy_data

# Remove the Stop Words

data("stop_words")

tidy_data <- tidy_data %>%
  anti_join(stop_words)

###
# Visualize the word usage count / ranking
##

tidy_data %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkred") +
  theme_fivethirtyeight() +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Word Usage in Data")

###
# Extract the sentiments from the tokenized dataframe
# Method: BING (Bing Liu et. Al) - categorize words by Positive & Negative
#         sentiments
##

data_sent <- tidy_data %>%
  inner_join(get_sentiments("bing"))

###
# Add AFINN  (Finn ?rup Nielsen), values - scores sentiments from -5 to 5
##

data_sent <- data_sent %>%
  inner_join(get_sentiments("afinn"))

# Shows/inspect the list of Sentiments / Lexicon

data_sent

table(data_sent$value,data_sent$sentiment)

# Plot the Sentiments 

data_sent %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  theme_fivethirtyeight() +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ylab("Word Count") +
  ggtitle("Word Usage in Data", subtitle = "Sentiment Analysis Using
Bing et al.")

ggplot(data_sent, aes(value, sentiment,color="purple")) +
  geom_bar(stat = "identity", show.legend = FALSE)

##
# Makes a WORDCLOUD Chart (all words)
##  


tidy_data %>% count(word) %>%
  with(wordcloud(word, n, scale=c(.9,.5),min.freq=10,max.words = 50),random.color=FALSE)


tidy_data %>%
  inner_join(data_sent) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),scale=c(.9,.5),
                   max.words = 50)

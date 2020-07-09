#loading required packages
library(gutenbergr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(viridis)
library(ggthemes)

#downloading UTF-8 text file from project gutenberg using ID number
Emily_Bronte <- gutenberg_download(768) 

#converting each row into tokens 
Tidy_Bronte <- Emily_Bronte %>%
  unnest_tokens(word,text) %>% #split into individual words
  anti_join(stop_words) # discarding stop words such as the, of, if so on

#visualizing word frequencies 
Tidy_Bronte %>%
  count(word, sort = TRUE) %>% # counting each word frequency
  filter(n > 100) %>% # filter with frequency > 100
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n),fill()) + #plot
  geom_col(aes(fill=word), color = "black", size = .7, 
           show.legend = FALSE) + # each bar
  xlab(NULL) + #xtitle
  ylab("Frequency")+ #ytitle
  coord_flip()+ #flip x and y
  labs(title = "Word frequency in \"Wuthering Heights\"")+ #plot title
  scale_fill_viridis(option = "magma", discrete = TRUE) + #custom theme and coloring
  theme_pander(base_size = 18, pc = "white") 

#using nrc lexicon for sentiment analysis
nrc_emotions <- get_sentiments("nrc") %>% # filtering only 4 out of all the available emotions
  filter(sentiment == "joy" |
           sentiment == "anger" |
           sentiment == "fear" |
           sentiment == "sadness")

TB_emotions <- Tidy_Bronte %>%
  inner_join(nrc_emotions) %>% # matching words with sentiments
  count(word, sentiment) %>% # count the words associated with sentiments
  arrange(sentiment)  #order by sentiments

#visualizing using nrc lexicon 
TB_emotions %>% 
  group_by(sentiment) %>% #grouping words with emotions
  top_n(10) %>% # filter the top 10
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + #plot
  geom_col(color = "black", size = .7, show.legend = FALSE)+ #bars
  labs(
    title = "Top 10 words clustered by sentiment", 
    x = NULL, 
    y = "number of occurrences" 
  ) + #title of the plot and axis
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  coord_flip() + # flip axes
  theme_pander(base_size = 18, pc = "white") #custom colors and themes

# visualizing wordcloud package
Tidy_Bronte %>%
  inner_join(get_sentiments("bing")) %>% #import bing sentiments
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(50) %>% # top 50 words
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%  
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% # cloud.comparison requires matrix representation
  comparison.cloud(colors = c("#ab3400", "#469280")) # cloud comparison of word occurance

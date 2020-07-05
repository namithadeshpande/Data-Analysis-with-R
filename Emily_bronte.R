library(gutenbergr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)

Emily_Bronte <- gutenberg_download(768) 

Tidy_Bronte <- Emily_Bronte %>%
  unnest_tokens(word,text) %>%
  mutate(line_number = row_number()) %>%
  anti_join(stop_words)

Tidy_Bronte

install.packages("viridis")
library(viridis)

install.packages("ggthemes")
library(ggthemes)

Tidy_Bronte %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n),fill()) +
  geom_col(aes(fill=word), color = "black", size = .7, 
           show.legend = FALSE) +
  xlab(NULL) +
  ylab("Frequency")+
  coord_flip()+
  labs(title = "Word frequency in \"Wuthering Heights\"")+
  scale_fill_viridis(option = "magma", discrete = TRUE) + 
  theme_pander(base_size = 18, pc = "white") 

nrc_emotions <- get_sentiments("nrc") %>%
  filter(sentiment == "joy" |
           sentiment == "anger" |
           sentiment == "fear" |
           sentiment == "sadness")

TB_emotions <- Tidy_Bronte %>%
  inner_join(nrc_emotions) %>% 
  count(word, sentiment) %>% 
  arrange(sentiment) 

TB_emotions %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(color = "black", size = .7, show.legend = FALSE)+
  labs(
    title = "Top 10 words clustered by sentiment", 
    x = NULL, 
    y = "number of occurrences" 
  ) +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  coord_flip() + # flip axes
  theme_pander(base_size = 18, pc = "white") 

install.packages("reshape2")
library(reshape2)

install.packages("wordcloud")
library(wordcloud)

Tidy_Bronte %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(50) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%  
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#ab3400", "#469280")) 

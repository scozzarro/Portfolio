#Whatsapp project - Sentiment analysis

library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(rvest)
library(textdata)

#1.0 Lexicon analysis ----

#1.1 Lexicon diversity ----
mychat %>% unnest_tokens(input = text, output = word) %>%
           filter(!word %in% useless_words)  %>%
           group_by(author) %>%
           summarise(lex_div = n_distinct(word)) %>%
           arrange(desc(lex_div)) %>%
           ggplot(aes(reorder(author, lex_div), lex_div, fill = author)) +
           geom_col(show.legend = FALSE) +
           geom_text(aes(label = scales::comma(lex_div)), hjust = -0.1) +
           ylab("Lexicon diversity") +
           xlab("User") +
           ggtitle("Lexicon diversity in chat by author") +
           coord_flip()

#1.2 Unic word per author ----
unic_words_G<- mychat %>% unnest_tokens(input = text, output = word) %>%
                          filter(author != "G") %>%
                          count(word, sort = TRUE)

mychat %>%  unnest_tokens(input = text, output = word) %>%
            filter(author == "G") %>%
            count(word, sort = TRUE) %>%
            filter(!word %in% unic_words_G$word) %>%
            top_n(15, n) %>%
            ggplot(aes(reorder(word, n), n)) +
            geom_col(show.legend = FALSE) +
            ylab("Number of times used") +
            coord_flip() +
            ggtitle("Top used word for G")

unic_words_A<- mychat %>% unnest_tokens(input = text, output = word) %>%
                          filter(author != "Andrea Marciano") %>%
                          count(word, sort = TRUE)

mychat %>% unnest_tokens(input = text, output = word) %>%
           filter(author == "Andrea Marciano") %>%
           count(word, sort = TRUE) %>%
           filter(!word %in% unic_words_A$word) %>%
           top_n(15, n) %>%
           ggplot(aes(reorder(word, n), n)) +
           geom_col(show.legend = FALSE) +
           ylab("Number of times used") +
           coord_flip() +
           ggtitle("Top used word for Andrea Marciano")

#2.0 Sentiment analysis with AFINN ----

# GET POSITIVE / NEGATIVE FROM LEXICON PACKAGE
lexicon_negpos<- get_sentiments("afinn")

#PREVIEW OF THE LEXICON FORMAT
lexicon_negpos %>% head(10) %>%kable() %>% kable_styling(full_width = FALSE, font_size = 11)

# PREVIEW WHAT ARE THE POSSIBLE VALUES
table(lexicon_negpos$value) %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 11)

emoji_sent_score<- mychat %>% select(emoji, emoji_name) %>% 
                              unnest(emoji, emoji_name) %>%
                              mutate(emoji = str_sub(emoji, end = 1)) %>%
                              mutate(emoji_name = str_remove(emoji_name, ":.*")) %>%
                              distinct() %>%
                              unnest_tokens(input = emoji_name, output = emoji_words) %>%
                              inner_join(lexicon_negpos, by = c("emoji_words" = "word"))

bind_cols(slice(emoji_sent_score, 01:05),
          slice(emoji_sent_score, 06:10),
          slice(emoji_sent_score, 11:15),
          slice(emoji_sent_score, 16:20),
          slice(emoji_sent_score, 21:25)) %>%
          kable() %>%
          kable_styling(full_width = FALSE, font_size = 11)

emoji_chat<- mychat %>% unnest(emoji, emoji_name) %>% mutate( emoji = str_sub(emoji, end = 1)) %>%
                        mutate( emoji_name = str_remove(emoji_name, ":.*"))

emoji_chat<- emoji_chat %>% select(author, emoji_name) %>% unnest_tokens(input = emoji_name, output = emoji_words)

user_summary<- emoji_chat %>% inner_join(lexicon_negpos, by = c("emoji_words"="word")) %>%
                              count(author, value) %>%
                              group_by(author) %>%
                              mutate(mean = n/sum(n)) %>%
                              ungroup()

reordenar_niveles <- c(-3,-2,-1,3,2,1)
colores <- c("#d7191c","#fdae61","#ffffbf","#1a9641","#a6d96a","#ffffbf")
mis_colores <- brewer.pal(5,"RdYlGn")[c(1,2,3,5,4,3)]

user_summary %>% mutate(mean = ifelse(value<0, -mean, mean)) %>%
                 group_by(author) %>%
                 mutate(balance = sum(mean)) %>%
                 ungroup() %>%
                 mutate(value = factor(value, levels = reordenar_niveles, ordered = TRUE)) %>%
                 ggplot(aes(reorder(author, balance), mean, fill = value)) +
                 geom_bar(stat = "identity", position = "stack", show.legend = FALSE, width = .5) +
                 scale_fill_manual(values = mis_colores) +
                 xlab("User") +
                 ylab("Scale of positive and negative") +
                 coord_flip() +
                 ggtitle("Emoji sentiment analysis with AFINN") +
                 theme_minimal()

#2.1 Most frequent emotion ----       
lexicon_sentiment<- get_sentiments("nrc")

emoji_emo<- mychat %>% select(emoji, emoji_name) %>%
                       unnest(emoji, emoji_name) %>%
                       mutate(emoji = str_sub(emoji, end = 1)) %>%
                       mutate(amoji_name = str_remove(emoji_name, ":.*")) %>%
                       unnest_tokens(input = emoji_name, output = emoji_words) %>%
                       inner_join(lexicon_sentiment, by = c("emoji_words" = "word")) %>%
                       filter(!sentiment %in% c("negative", "positive")) %>%
                       count(emoji, emoji_words, sentiment) %>%
                       group_by(sentiment) %>%
                       top_n(4, n) %>%
                       slice(1:4) %>%
                       ungroup() %>%
                       select(-n)

chat_sentiment<- emoji_chat %>% inner_join(lexicon_sentiment, by = c("emoji_words" = "word")) %>%
                                filter(!sentiment %in% c("negative", "positive"))

chat_sentiment %>% count(sentiment) %>%
                   ggplot(aes(reorder(sentiment, n), n)) +
                   geom_col(aes(fill = n), show.legend = FALSE, width = .1) +
                   geom_point(aes(color = n), show.legend = FALSE, size = 3) +
                   coord_flip() +
                   ylab("Number of times") +
                   xlab("Emotion") +
                   scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                   scale_color_gradient(low="#2b83ba",high="#d7191c") +
                   ggtitle("Most frequent emotions expressed in chat through") +
                   theme_minimal()

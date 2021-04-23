#New Whatsapp project for text analysis

library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)

#1. Import chat ----
mychat<- rwa_read('chat_A_G.txt')

#2. Clean Data
summary(mychat)
str(mychat)

mychat<- mychat[-1,] #delete first raw with whatsapp privacy encoding disclaimer

mychat$author<- as.character(mychat$author)
mychat$author[mychat$author != "Andrea Marciano"] <- "G"
mychat$author<- as.factor(mychat$author)                     


mychat<- mychat %>% 
         mutate(day = date(time))%>%
         mutate(season = case_when(
                                   day >= dmy(05012017) & day <= dmy(31032017) ~ 'Winter 2017',
                                   day >= dmy(01042017) & day <= dmy(21062017) ~ 'Spring 2017',
                                   day >= dmy(22062017) & day <= dmy(23092017) ~ 'Summer 2017',
                                   day >= dmy(24092017) & day <= dmy(20122017) ~ 'Autumn 2017',
                                   day >= dmy(21122017) & day <= dmy(31032018) ~ 'Winter 2018',
                                   day >= dmy(01042018) & day <= dmy(21062018) ~ 'Spring 2018',
                                   day >= dmy(22062018) & day <= dmy(23092018) ~ 'Summer 2018',
                                   day >= dmy(24092018) & day <= dmy(20122018) ~ 'Autumn 2018',
                                   day >= dmy(21122018) & day <= dmy(31032019) ~ 'Winter 2019',
                                   day >= dmy(01042019) & day <= dmy(21062019) ~ 'Spring 2019',
                                   day >= dmy(22062019) & day <= dmy(23092019) ~ 'Summer 2019',
                                   day >= dmy(24092019) & day <= dmy(20122019) ~ 'Autumn 2019',
                                   day >= dmy(21122019) & day <= dmy(31032020) ~ 'Winter 2020',
                                   day >= dmy(01042020) & day <= dmy(21062020) ~ 'Spring 2020',
                                   day >= dmy(22062020) & day <= dmy(23092020) ~ 'Summer 2020',
                                   day >= dmy(24092020) & day <= dmy(15122020) ~ 'Autumn 2020'
                                   ))

mychat$season<- factor(mychat$season)

#3. EDA ----

#3.1 Messages per seasons ----
mychat %>% group_by(season) %>%
           count(day) %>%
           ggplot(aes(day, n, fill = season)) +
           geom_bar(stat = 'identity') +
           ylab('Numbers of messages') +
           xlab('season') +
           ggtitle('Messages per Seasons') +
           theme_minimal() +
           theme(legend.position = 'bottom')

#3.2 Messages per day of week
mychat %>% mutate(wday_num = wday(day), wday_name = weekdays(day)) %>%
           group_by(season, wday_num, wday_name) %>%
           count() %>%
           ggplot(aes(reorder(wday_name, -wday_num), n, fill = season)) +
           geom_bar(stat = 'identity') +
           xlab('') +
           coord_flip() +
           ggtitle('Messages per day of week', 'Frequency per seasons') +
           theme_minimal() +
           theme(legend.title = element_blank(), legend.position = 'bottom')

#3.3 Message frequency by the time of day ----
wdays<- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
names(wdays)<- 1:7 #Messages per day hours
mychat %>% mutate(hours = hour(time), wday_num = wday(day), wday_name = weekdays(day)) %>%
           count(season, wday_num, wday_name, hours) %>%
           ggplot(aes(hours, n, fill = season)) +
           geom_bar(stat = 'identity') +
           ylab('Number of messages') +
           xlab('Hours') +
           ggtitle('Number of messages per day hours', 'Frequency per seasons') +
           facet_wrap(~wday_num, ncol = 7, labeller = labeller(wday_num = wdays)) +
           theme_minimal() +
           theme(legend.title = element_blank(), legend.position = 'bottom', 
                 panel.spacing.x = unit(0.0, 'lines'))

#3.4 Who has sent the most messages? ----
mychat %>% mutate(day = date(time)) %>%
           group_by(season) %>%
           count(author) %>%
           ggplot(aes(reorder(author,n), n, fill = season)) +
           geom_bar(stat = 'identity') +
           ylab('Total number of messages') +
           xlab('User') +
           coord_flip() +
           ggtitle('Total number of messages per user', 'Who has sent the most messages?, Freq per season') +
           theme_minimal() +
           theme(legend.title = element_blank(), legend.position = 'bottom')

#3.4 Lenght of messages ----
mychat %>% mutate(text_len = nchar(text)) %>%
           group_by(author) %>%
           summarise(avg_txt_len = mean(text_len)) %>%
           ggplot(aes(author, avg_txt_len, fill = author)) +
           geom_bar(stat = 'identity') +
           xlab('Author') +
           ylab('Average messages lenght') +
           coord_flip() +
           ggtitle('Average messages lenght by author') +
           theme_minimal() +
           theme(legend.title = element_blank(), legend.position = 'bottom')

#3.5 Emojis ----

#What are the most used emojis in chat?
# LIBRARY FOR EMOJI PNG IMAGE FETCH FROM https://abs.twimg.com
library(ggimage) # EMOJI RANKING

emojiplot<- mychat %>% 
            unnest(c(emoji, emoji_name)) %>%
            mutate(emoji = str_sub(emoji, end = 1)) %>%
            mutate(emoji_name = str_remove(emoji_name, ':.*')) %>%
            count(emoji, emoji_name) %>%
            top_n(30, n) %>%
            arrange(desc(n)) %>%
            mutate(emoji_url = map_chr(emoji, ~paste0('https://abs.twimg.com/emoji/v2/72x72/',
                                                      as.hexmode(utf8ToInt(.x)),'.png')))
emojiplot %>% ggplot(aes(reorder(emoji_name, n), n)) +
              geom_col(aes(fill = n), show.legend = FALSE, width = .2) +
              geom_point(aes(color = n), show.legend = FALSE, size = 3) +
              geom_image(aes(image = emoji_url), size = .045) +
              ylab('Number of times emoji was used') +
              xlab('Emoji meaning') +
              ggtitle('Most used emoji') + 
              coord_flip() +
              theme_minimal() +
              theme()

#What are the most used emojis in chat per user?
emojiplot2<- mychat %>% 
             unnest(c(emoji, emoji_name)) %>%
             mutate(emoji = str_sub(emoji, end = 1))%>%
             count(author, emoji, emoji_name, sort = TRUE) %>%
             group_by(author) %>%
             top_n(8, n) %>%
             slice(1:8) %>%
             mutate(emoji_url = map_chr(emoji, ~paste0('https://abs.twimg.com/emoji/v2/72x72/', 
                                                        as.hexmode(utf8ToInt(.x)),'.png')))
emojiplot2 %>% ggplot(aes(reorder(emoji, -n), n)) +
               geom_col(aes(fill = author, group = author), show.legend = FALSE, width = .20) +
               geom_image(aes(image = emoji_url), size = .08) +
               xlab('Emiji') +
               ylab('Number of time emoji was used') +
               facet_wrap(~author, ncol = 5, scales = 'free') +
               ggtitle('Most used emoji by user') +
               theme_minimal() +
               theme(axis.text.x = element_blank())

#3.6 Most used words ----
useless_words<-c('il','lo','la','un','uno','una','quello','quella','quelli','nostro','vostro','di','quanto','che','se','sono',
                 'loro','alla','alle','niente','meno','piu','qui','qua','con','voi','chi','mio','tuo','va','ma','è','stata',
                 'per', 'nn','a','le','te','in','e','sto','da','sei','me','ho','ha','mi','we','per','non','sta','o','fra',
                 'su','so','hai','ci','mo','sn','eh','ti','c3','i','fa','al','ne','del')

mychat %>% unnest_tokens(input = text, output = word) %>%
           filter(!word %in% useless_words) %>%
           count(word) %>%
           top_n(30, n) %>%
           arrange(desc(n)) %>%
           ggplot(aes(reorder(word, n), n, fill = n, color = n)) +
           geom_col(show.legend = FALSE, width = .1) +
           geom_point(show.legend = FALSE, size = 3) +
           ggtitle('Most used words in chat') +
           xlab('Words') +
           ylab('Number of time it was used') +
           coord_flip() +
           theme_minimal()

#Most used words in chat, by user
mychat %>% unnest_tokens(input = text, output = word) %>%
           filter(!word %in% useless_words) %>%
           count(author, word, sort = TRUE) %>%
           group_by(author) %>%
           top_n(20, n) %>%
           slice(1:20) %>%
           ungroup() %>%
           arrange(author, desc(n)) %>%
           mutate(order = row_number()) %>%
           ggplot(aes(reorder(word, n), n, fill = author, color = author)) +
           geom_col(show.legend = FALSE, width = .1) +
           geom_point(show.legend = FALSE, size = 3) +
           xlab('Words') +
           ylab('Number of time it was used') +
           coord_flip() +
           facet_wrap(~author, ncol = 3, scales = 'free') +
           ggtitle('Most used words by user') +
           theme_minimal()
           
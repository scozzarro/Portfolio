#Data exploration----

library(tidyverse)
library(visdat)
library(correlationfunnel)
library(skimr)
library(cowplot)


#1.0 Load Data----
path_main<- "In_app_purchase/Data/AppleStore.csv"
path_desc<- "In_app_purchase/Data/appleStore_description.csv"


main_raw_tbl<- read.csv(path_main)
desc_raw_tbl<- read.csv(path_desc)

unified_raw_tbl<- merge(main_raw_tbl, desc_raw_tbl[,-c(2,3)], by = "id", all.x = T)


#2.0 EDA----

#Sanity check
skim_without_charts(unified_raw_tbl)
glimpse(unified_raw_tbl)
unified_raw_tbl<- unified_raw_tbl[,-2] #drop meaningless "X" column 

vis_miss(unified_raw_tbl)

#Refactoring
unique(unified_raw_tbl$cont_rating)
unified_raw_tbl$cont_rating<- as_factor(unified_raw_tbl$cont_rating)

unique(unified_raw_tbl$prime_genre)
unified_raw_tbl$prime_genre<- as_factor(unified_raw_tbl$prime_genre)


vis_dat(unified_raw_tbl)


#Understanding
length(unique(unified_raw_tbl$prime_genre)) #number of category

##Category analysis ----

#Category presence in the store
unified_raw_tbl %>% group_by(prime_genre) %>%
                    summarise(n = n()) %>%
                    ggplot(aes(n, reorder(prime_genre, -n), fill = prime_genre)) +
                    geom_bar(stat = "identity") +
                    scale_y_discrete(limits = rev)+
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    theme(legend.position = "NONE") +
                    labs(title = "App category presence in the store", 
                         subtitle = "Games and Entertaiment dominate the App Store",
                         x = "Number of App",
                         y = "Category")

#Category presence by PAID or FREE
unified_raw_tbl %>% mutate(is_free = case_when(price == 0 ~ "FREE", TRUE ~ "PAID")) %>%
                    ggplot(aes(x = reorder(prime_genre, prime_genre, function(x)length(x)), fill = prime_genre)) +
                    geom_bar(stat = "count") +
                    facet_wrap(~is_free) +
                    coord_flip() +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    theme(legend.position = "NONE") +
                    labs(title = "App category presence in the store grouped by Free or Paid", 
                         subtitle = "Generally free app are more present in the app store except Photo e Video and Utilities",
                         x = "Number of App",
                         y = "Category")

#Average price by category
unified_raw_tbl %>% filter(price > 0) %>%
                    group_by(prime_genre) %>%
                    summarise(avg_price = mean(price)) %>%
                    ggplot(aes(avg_price, reorder(prime_genre, avg_price), fill = prime_genre)) +
                    geom_bar(stat = "identity") +
                    scale_x_continuous(labels = scales::dollar_format(), breaks = seq(0, 21, 3)) +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    theme(legend.position = "NONE") +
                    labs(title = "Average price for paid app by catagory", 
                         subtitle = "Apps related to health-care professions are the most expensive",
                         x = "Average Price in USD",
                         y = "Category")

                   
#Price and estimate bottom rev analysis
# unified_raw_tbl %>% mutate(is_free = case_when(price == 0 ~ "FREE", TRUE ~ "PAID")) %>%
#                     ggplot(aes(is_free, fill = is_free)) +
#                     geom_bar(stat = "count") +
#                     tidyquant::scale_fill_tq() +
#                     theme_minimal() +
#                     theme(legend.position = "NONE") +
#                     labs(title = "FREE Vs PAID App presence in the store",
#                          subtitle = "Noticeably more free app are present in the store",
#                          y = "Number of App",
#                          x = "")


##Price Analysis ----
unified_raw_tbl %>% mutate(is_free = case_when(price == 0 ~ "FREE", TRUE ~ "PAID")) %>%
                    group_by(is_free) %>%
                    summarise(n = n()) %>%
                    mutate(pct = n / sum(n)) %>%
                    ggplot(aes(is_free, pct, fill = is_free)) +
                    geom_bar(stat = "identity") +
                    scale_y_continuous(labels = scales::percent_format()) +
                    tidyquant::scale_fill_tq() +
                    theme_minimal() +
                    theme(legend.position = "NONE") +
                    labs(title = "FREE Vs PAID App presence in the store",
                        subtitle = "Noticeably more free app are present in the store",
                        y = "App presence in %",
                        x = "")

a <- unified_raw_tbl %>% filter(price > 0) %>%
                         ggplot(aes(price)) +
                         geom_density(adjust = 40, fill = tidyquant::palette_light()[[1]]) + 
                         theme_minimal() +
                         scale_x_continuous(labels = scales::dollar_format()) +
                         xlab("Price in USD") +
                         ylab("Number of App")
                         


b<- unified_raw_tbl %>% filter(price > 0) %>%
                        ggplot(aes(price)) +
                        geom_boxplot(outlier.colour = tidyquant::palette_light()[[2]]) +
                        theme_minimal() +
                        scale_x_log10(labels = scales::dollar_format()) +
                        coord_flip() +
                        xlab("Price in USD")

plot_raw<- plot_grid(a, b, nrow = 1)

title <- ggdraw() + 
         draw_label("Price distribution inside App Store",
         x = 0,
         hjust = 0) + 
         theme_minimal() +
         theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title, plot_raw, ncol = 1, rel_heights = c(0.1, 1))

'%!in%' <- function(x,y)!('%in%'(x,y))
not_free<- which(unified_raw_tbl$price > 0)
price_outliers<- boxplot(unified_raw_tbl$price[not_free], plot = FALSE)$out

unified_raw_tbl %>% filter(price >0 & price %!in% price_outliers) %>%
                    ggplot(aes(price)) +
                    geom_density(fill = tidyquant::palette_light()[[1]]) +
                    theme_minimal() +
                    scale_x_continuous(labels = scales::dollar_format()) +
                    labs(title = "Price distribution without outliers", 
                         x = "Price in USD")

unified_raw_tbl %>% filter(price > 0 & price %!in% price_outliers) %>%
                    ggplot(aes(price, fill = prime_genre)) +
                    geom_density(alpha = .4) +
                    scale_x_continuous(labels = scales::dollar_format()) +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    theme(legend.position = "NONE") +
                    facet_wrap(~prime_genre, nrow = 6) +
                    labs(title = "Price distribution by category", 
                         subtitle = "Outliers excluded",
                         x = "Price in USD")
      

unified_raw_tbl %>% filter(price > 0) %>%
                    group_by(sup_devices.num) %>%
                    summarise(avg_price = mean(price)) %>%
                    ggplot(aes(sup_devices.num, avg_price)) +
                    geom_point(color = tidyquant::palette_light()[[2]], size = 3) +
                    geom_line(size = 1, color = tidyquant::palette_light()[[1]]) +
                    geom_smooth(method = "loess") +
                    scale_y_continuous(labels = scales::dollar_format()) +
                    theme_minimal() +
                    labs(title = "Supported device vs Price",
                         x = "Number of supported devices",
                         y = "Average price in USD")

unified_raw_tbl %>% filter(price > 0) %>%
                    group_by(lang.num) %>%
                    summarise(avg_price = mean(price)) %>%
                    ggplot(aes(lang.num, avg_price)) +
                    geom_point(color = tidyquant::palette_light()[[2]], size = 3) +
                    geom_line(size = 1, color = tidyquant::palette_light()[[1]]) +
                    geom_smooth(method = "loess") +
                    scale_y_continuous(labels = scales::dollar_format()) +
                    theme_minimal() +
                    labs(title = "Language vs Price",
                         subtitle = "No clear influence of supported languages on price",
                         x = "Number of supported languages",
                         y = "Average Price")

unified_raw_tbl %>% filter(price >0) %>%
                    mutate(size_MB = size_bytes / 1000000) %>%
                    ggplot(aes(size_MB, price)) +
                    geom_line(size = 1, color = tidyquant::palette_light()[[1]]) +
                    scale_y_continuous(labels = scales::dollar_format()) +
                    theme_minimal() +
                    labs(title = "Price Vs App size in Mb", 
                         subtitle = "No clear influence of App size on price",
                         x = "App size in Mb",
                         y = "Price in USD")



# unified_raw_tbl %>% mutate(version = str_extract(ver, pattern = "\\d\\.\\d")) %>% 
#                     mutate(version = as.numeric(version)) %>%
#                     filter(price > 0 & price %!in% price_outliers) %>%
#                     select(version, price) %>%
#                     ggplot(aes(version, price)) +
#                     geom_point(color = tidyquant::palette_light()[[2]], size = 3) +
#                     geom_line(size = 1, color = tidyquant::palette_light()[[1]]) +
#                     scale_y_continuous(labels = scales::dollar_format()) +
#                     theme_minimal() +
#                     labs(title = "Version vs Price",
#                          x = "Number of developed versions",
#                          y = "Average Price")


##Market Analysis---- 
unified_raw_tbl %>% filter(price > 0) %>%
                    mutate(bottom_rev = price * rating_count_tot) %>%
                    group_by(prime_genre) %>%
                    summarise(avg_bottom_rev = mean(bottom_rev)) %>%
                    ggplot(aes(avg_bottom_rev, reorder(prime_genre, avg_bottom_rev), fill = prime_genre)) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    scale_x_continuous(labels = scales::dollar_format()) +
                    theme(legend.position = "NONE") +
                    labs(title = "Estimated average bottom revenue by category",
                         subtitle = "Excluding free app",
                         x = "Average bottom revenue in USD",
                         y = "Category")


unified_raw_tbl %>% mutate(bottom_rev = price * rating_count_tot) %>%
                    slice_max(order_by = bottom_rev, n = 10) %>% 
                    select(track_name, prime_genre, bottom_rev) %>%
                    mutate(track_name = strtrim(track_name, width = 23)) %>%
                    ggplot(aes(bottom_rev, reorder(track_name, bottom_rev), fill = prime_genre)) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    scale_x_continuous(labels = scales::dollar_format(scale = 1/1000000, prefix = "$", suffix = "M")) +
                    labs(title = "Top 10 app for estimated bottom revenue",
                         subtitle = "Games are the top performers for revenue",
                         x = "Estimated bottom revenue",
                         y = "App name")



#Market value by category pie chart (too many category)
# unified_raw_tbl %>% group_by(prime_genre) %>%
#                     mutate(bottom_rev = price * rating_count_tot) %>%
#                     summarise(mrkt_val = sum(bottom_rev)) %>%
#                     mutate(pct = mrkt_val/sum(mrkt_val), 
#                            ymax = cumsum(pct),
#                            ymin = c(0, head(ymax, n = -1)),
#                            labelposition = (ymax + ymin) / 2) %>%
#                     ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = prime_genre)) +
#                     geom_rect() +
#                     geom_label(x = 3.5,aes(y = labelposition, label = round(pct*100,1), color = "white")) +
#                     coord_polar(theta = "y") +
#                     xlim(c(2,4)) +
#                     theme_void() +
#                     tidyquant::scale_fill_tq() +
#                     theme(legend.position = "bottom")

unified_raw_tbl %>% group_by(prime_genre) %>%
                    mutate(bottom_rev = price * rating_count_tot) %>%
                    summarise(mrkt_val = sum(bottom_rev)) %>%
                    mutate(pct = mrkt_val/sum(mrkt_val)) %>%
                    ggplot(aes(mrkt_val, 
                               reorder(prime_genre, mrkt_val), 
                               fill = prime_genre,
                               label = scales::percent(round(pct, 2)))) +
                    geom_bar(stat = "identity") +
                    geom_text(stat = "identity", hjust = -0.5, size = 3) +
                    scale_x_continuous(labels = scales::dollar_format(scale = 1/1000000, prefix = "$", suffix = "M")) +
                    expand_limits(x = c(0, 30000000)) +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    theme(legend.position = "NONE") +
                    labs(title = "Estimated market bottom value by App category",
                         subtitle = "Gaming App represents the biggest market share",
                         x = "Estimated market value in USD",
                         y = "Category")
                    

unified_raw_tbl %>% filter(price > 0) %>%
                    mutate(user_rating = round(user_rating)) %>%
                    group_by(user_rating) %>%
                    summarise(avg_price = mean(price)) %>%
                    ggplot(aes(as.factor(user_rating), avg_price, fill = as.factor(user_rating))) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    scale_y_continuous(labels = scales::dollar_format()) +
                    theme(legend.position = "NONE") +
                    labs(title = "Average Price vs User Rating",
                         subtitle = "Excluding free app",
                         x = "User Rating",
                         y = "Average Price in USD")

cutpoints<- quantile(unified_raw_tbl$price[which(unified_raw_tbl$price >0)])

unified_raw_tbl %>% filter(price >0) %>%
                    mutate(price_range = case_when(price <= cutpoints[1] ~ "0-1$",
                                                   price > cutpoints[1] & price <= cutpoints[2] ~ "1-2$",
                                                   price > cutpoints[2] & price <= cutpoints[3] ~ "2-3$",
                                                   price > cutpoints[3] & price <= cutpoints[4] ~ "3-5$",
                                                   price > cutpoints[4] ~ "Over 5$")) %>%
                   mutate(price_range = as.factor(price_range)) %>%
                   ggplot(aes(as.factor(round(user_rating)), fill = as.factor(round(user_rating)))) +
                   geom_bar(stat = "count") +
                   facet_wrap(~ price_range, scales = "free") +
                   theme_minimal() +
                   tidyquant::scale_fill_tq() +
                   theme(legend.position = "NONE") +
                   labs(title = "User rating frequency by price range",
                        x = "User Rating",
                        y = "Number of rates")

unified_raw_tbl %>% select(user_rating, user_rating_ver, prime_genre) %>%
                    mutate(ev_user_rating_prev_ver = (user_rating * 2) - user_rating_ver) %>%
                    mutate(ev_user_rating_prev_ver = case_when(ev_user_rating_prev_ver < 0 ~ 0,
                                                               TRUE ~ ev_user_rating_prev_ver)) %>%
                    mutate(rating_delta= case_when((user_rating_ver - ev_user_rating_prev_ver) < -5 | (user_rating_ver - ev_user_rating_prev_ver) > 5 ~ (user_rating_ver - user_rating),
                                                    TRUE ~ user_rating_ver - ev_user_rating_prev_ver)) %>%
                    filter(rating_delta > ev_user_rating_prev_ver) %>%
                    mutate(ev_conversion_increase = case_when(rating_delta >= 4 ~ "770%",
                                                              ev_user_rating_prev_ver  <=1.5 & round(rating_delta) == 2 ~ "30%",
                                                              ev_user_rating_prev_ver  <=1.5 & round(rating_delta) == 3  ~ "340%",
                                                              ev_user_rating_prev_ver  <=1.5 & round(rating_delta) == 4  ~ "730%",
                                                              round(ev_user_rating_prev_ver)  == 2 & round(rating_delta) == 3  ~ "280%",
                                                              round(ev_user_rating_prev_ver)  == 2 & round(rating_delta) == 4  ~ "540%",
                                                              round(ev_user_rating_prev_ver)  == 2 & round(rating_delta) == 5  ~ "570%",
                                                              round(ev_user_rating_prev_ver)  == 3 & round(rating_delta) == 4  ~ "89%",
                                                              round(ev_user_rating_prev_ver)  == 3 & round(rating_delta) == 5  ~ "97%",
                                                              round(ev_user_rating_prev_ver)  == 4 & round(rating_delta) == 5  ~ "4%")) %>%
                    #slice_max(order_by = rating_delta, n = 60)
                    filter(ev_conversion_increase == "770%") %>%
                    group_by(prime_genre) %>%
                    summarise(n = n()) %>%
                    ggplot(aes(n, reorder(prime_genre, n), fill = prime_genre)) +
                    geom_bar(stat = "identity") +
                    scale_x_continuous(breaks = c(2,5,8,10)) +
                    theme_minimal() +
                    tidyquant::scale_fill_tq() +
                    theme(legend.position = "NONE") +
                    labs(title = "41 apps by category that had an estimated exponential conversion growth compared to prev version",
                         subtitle = "Jumping from a rate of 1 to a rate of 5 between version create an expected conversion increase of 770%",
                         caption = "According to Apptetive 2015 survey, https://thetool.io/2017/improve-user-ratings ",
                         x = "Number of Apps",
                         y = "Category")


unified_raw_tbl %>% mutate(user_rating = round(user_rating_ver)) %>%
                    group_by(lang.num) %>%
                    summarise(avg_user_rating_ver = mean(user_rating_ver)) %>%
                    ggplot(aes(lang.num, avg_user_rating_ver)) +
                    geom_point(size = 2, color = tidyquant::palette_light()[[2]]) +
                    geom_line(size = 0.6, color = tidyquant::palette_light()[[1]]) +
                    geom_smooth(method = "loess") +
                    theme_minimal() +
                    labs(title = "Supported languages Vs User Rating",
                         x = "Supported languages",
                         y = "User Rating current version")
                    
unified_raw_tbl %>% mutate(user_rating_ver = round(user_rating_ver)) %>%
                    group_by(sup_devices.num) %>%
                    summarise(avg_user_rating_ver = mean(user_rating_ver)) %>%
                    ggplot(aes(sup_devices.num, avg_user_rating_ver)) +
                    geom_point(size = 2, color = tidyquant::palette_light()[[2]]) +
                    geom_line(size = 0.6, color = tidyquant::palette_light()[[1]]) +
                    geom_smooth(method = "loess") +
                    theme_minimal() +
                    labs(title = "Supported devices Vs User Rating",
                         x = "Supported devices",
                         y = "User Rating current version")



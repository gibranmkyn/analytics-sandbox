
  
rm(list = ls())

t1 <- Sys.time()

setwd('/Volumes/Buku Gibran/edgar')

library(tidyverse)
library(edgar)
library(XML)
library(lubridate)
library(tm)
library(RSQLite)
library(tidytext)
library(udpipe)
library(rvest)


conn <- dbConnect(RSQLite::SQLite(), "edgar.db")




t1 <- Sys.time()

# ----- TF-IDF industry level


financial_era <- c('Post Financial Crisis (2009 - 2010)', 'Brexit (2016 - 2019)') # excluded eras = 'CITIS (2011 - 2012)', 'TCJA (2018 - 2019)'
years <- c('2009,2010', '2016,2017,2018,2019')

for (era in 1:length(financial_era)) {

  market_level <- dbGetQuery(conn, paste0('SELECT gics_sector, cleaned_noun FROM sp500 LEFT JOIN master_index ON master_index.cik =  sp500.cik 
                             WHERE year_filed IN (', years[1],')'))
  
  gics_tokens <- market_level %>% 
    unnest_tokens(word, cleaned_noun) %>% 
    count(gics_sector, word, sort = TRUE) %>% 
    ungroup() %>%
    bind_tf_idf(word, gics_sector, n)
  rm(market_level)
  
  gics_tokens %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(gics_sector) %>%
    top_n(20) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = gics_sector)) + geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf", title = paste("Important Terms", financial_era[1]), subtitle = "grouped by GICS sector") +
    
    facet_wrap(~gics_sector, ncol = 4, scales = "free") + coord_flip()

}


t1 <- Sys.time()

market_level <- dbGetQuery(conn, 'SELECT year_filed, cleaned_noun FROM master_index')

gics_tokens <- market_level %>% 
  unnest_tokens(word, cleaned_noun) %>% 
  count(year_filed, word, sort = TRUE) %>% 
  ungroup() %>%
  bind_tf_idf(word, year_filed, n)
rm(market_level)

gics_tokens %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(year_filed) %>%
  top_n(20) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = year_filed)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", title = "Important Terms for S&P 500") +
  facet_wrap(~year_filed, ncol = 4, scales = "free") + coord_flip()


t2 <- Sys.time()
t2-t1









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
library(readxl)
library(qdap)
library(sentimentr)
library(textfeatures)
library(tidyr)

conn <- dbConnect(RSQLite::SQLite(), "edgar.db")

# -- Import LM Dictionary
LM_dictionary_file <- "LoughranMcDonald_SentimentWordLists_2018.xlsx"
sentiment <- c("negative", "positive", "uncertainty", "litigious", "strong_modal","weak_modal", "constraining")

LM_dictionary <- data.frame()
for(s in 1:7) {
  local_df <- tibble(word = tolower(read_excel(LM_dictionary_file, sheet = s+1)$word), sentiment = sentiment[s] )
  LM_dictionary <- bind_rows(LM_dictionary, local_df)
}

rm(local_df)

dummy_LM <- tibble(accession_number = 'dummy', positive = 0, negative = 0, uncertainty = 0, litigious = 0, constraining = 0, strong_modal = 0, weak_modal = 0)
dummy_nrc <- tibble(accession_number = 'dummy', positive = 0, negative = 0, anger = 0, fear = 0, trust = 0, sadness = 0, surprise = 0, disgust = 0, joy = 0, anticipation = 0)
dummy_bing <- tibble(accession_number = 'dummy', positive = 0, negative = 0)

cik <- dbGetQuery(conn, 'SELECT distinct(cik) as cik FROM master_index')$cik

# accession_number_belum <- dbGetQuery(conn, 'SELECT master_index.accession_number FROM master_index WHERE master_index.accession_number NOT IN (SELECT accession_number FROM sentiment)')$accession_number
t1 <- Sys.time()
#for (c in 1:length(cik)) {
#  df_cik <- dbGetQuery(conn, paste0('SELECT accession_number, cleaned_text FROM master_index WHERE cik = "',cik[c], '"'))
  


 # for (r in 1:nrow(df_cik)) {
  for (r in 1:length(accession_number_belum)) {
    # df_report <- df_cik[r,]
    df_report <- dbGetQuery(conn, paste0('SELECT accession_number, cleaned_text FROM master_index WHERE accession_number = "',accession_number_belum[r], '"'))
      
    # ----- Sentiment Syuzhet, Vader, and n_words
    sentiment_syuzhet_vader <- textfeatures(df_report$cleaned_text, normalize = FALSE, word_dims = FALSE, sentiment = TRUE) %>%
      select(sent_syuzhet,sent_vader) %>%
      mutate(accession_number = df_report$accession_number)
    
    # ----- Text Complexity
    tokenised <- df_report %>% 
      unnest_tokens(word, cleaned_text)
    
    n_words <- tokenised %>%
      group_by(accession_number) %>%
      count(accession_number)
    
    n_complex <- tokenised %>%
      group_by(word, accession_number) %>%
      mutate(complexity = nchar( gsub( "[^X]", "", gsub( "[aeiouy]+", "X", tolower( word ))))) %>%
      filter(complexity >=3) %>%
      group_by(accession_number) %>%
      count(accession_number)
    
    complexity <- tibble(accession_number = n_words$accession_number,
                         complexity = n_complex$n / n_words$n)
    
    rm(n_complex)
    
    # ----- Sentiment LoughranMcDonald
    tokens_LM <- tokenised %>%
      inner_join(LM_dictionary) 
    
    word_count_LM <- tokens_LM %>% group_by(accession_number) %>% summarise(LM_total_words =n())
    
    sentiment_LM <- tokens_LM %>% 
      group_by(accession_number,sentiment) %>% 
      summarise(total_sentiment = n()) %>% 
      spread(sentiment, total_sentiment, fill = 0) %>%
      bind_rows(dummy_LM) %>%
      left_join(word_count_LM) %>%
      mutate(LM_sent = positive - negative,
             LM_positive = positive / LM_total_words,
             LM_negative = negative / LM_total_words,
             LM_uncertainty = uncertainty / LM_total_words,
             LM_litigious = litigious / LM_total_words,
             LM_constraining = constraining / LM_total_words,
             LM_strong_modal = strong_modal / LM_total_words,
             LM_weak_modal = weak_modal / LM_total_words) %>%
      select(-c(positive, negative, uncertainty, litigious, constraining, strong_modal, weak_modal)) %>%
      filter(accession_number != 'dummy')
    
    rm(tokens_LM, word_count_LM)
    
    # ----- Sentimentr
    text <- get_sentences(df_report$cleaned_text)
    sentimentr <- tibble(accession_number = df_report$accession_number, 
                         sentimentr = as.data.frame(sentiment_by(text))$ave_sentiment)
    
    rm(text)
    
    # ----- Sentiment Afinn
    sentiment_afinn <- tokenised %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(accession_number) %>% 
      summarise(afinn_sent = sum(value))
    
    # ----- Sentiment bing
    tokens_bing <- tokenised %>% 
      inner_join(get_sentiments("bing"))
    
    word_count_bing <- tokens_bing %>% group_by(accession_number) %>% summarise(bing_total_words =n())
    
    sentiment_bing <- tokens_bing %>% 
      group_by(accession_number,sentiment) %>% 
      summarise(total_sentiment = n()) %>% 
      spread(sentiment, total_sentiment, fill = 0) %>%
      bind_rows(dummy_bing) %>%
      left_join(word_count_bing) %>%
      mutate(bing_sent = positive - negative,
             bing_positive = positive/bing_total_words,
             bing_negative = negative/bing_total_words) %>%
      select(-c(positive, negative)) %>%
      filter(accession_number != 'dummy')
    
    rm(tokens_bing, word_count_bing)
    
    # ----- Sentiment NRC
    tokens_nrc <- tokenised %>% 
      inner_join(get_sentiments("nrc")) 
    
    word_count_nrc <- tokens_nrc %>% group_by(accession_number) %>% summarise(nrc_total_words =n())
    
    sentiment_nrc <- tokens_nrc %>% 
      group_by(accession_number,sentiment) %>% 
      summarise(total_sentiment = n()) %>% 
      spread(sentiment, total_sentiment, fill = 0) %>%
      bind_rows(dummy_nrc) %>%
      left_join(word_count_nrc) %>%
      mutate(nrc_sent = positive - negative,
             nrc_positive = positive / nrc_total_words,
             nrc_negative = negative / nrc_total_words,
             nrc_anger = anger/nrc_total_words,
             nrc_fear = fear/nrc_total_words,
             nrc_trust = trust/nrc_total_words,
             nrc_sadness = sadness/nrc_total_words,
             nrc_surprise = surprise/nrc_total_words,
             nrc_disgust = disgust/nrc_total_words,
             nrc_joy = joy/nrc_total_words,
             nrc_anticipation = anticipation/nrc_total_words) %>%
      select(-c(positive, negative, anger, trust, sadness, surprise, disgust,joy, anticipation, fear )) %>%
      filter(accession_number != 'dummy')
    
    rm(tokens_nrc, word_count_nrc)
    
    # ----- Merging Sentiment Features
    sentiment_df <- sentiment_LM %>%
      left_join(complexity) %>%
      left_join(sentimentr) %>%
      left_join(sentiment_afinn) %>%
      left_join(sentiment_bing) %>%
      left_join(sentiment_nrc) %>%
      left_join(sentiment_syuzhet_vader)
    
    rm(sentiment_LM, complexity, sentimentr, sentiment_afinn, sentiment_bing, sentiment_nrc, sentiment_syuzhet_vader, n_words, tokenised)
    
    # ----- Insertion to SQL table
    dbWriteTable(conn,"sentiment", sentiment_df, append = TRUE) # insert to sentiment_df Table
    
    rm(sentiment_df)
  
  #print(paste(df_report$accession_number[1], "has been processed..."))
  print(paste(r, "of", length(accession_number_belum))  )
    
  }
 # print(paste(c, "of", length(cik)))
#}


t2 <- Sys.time()

t2-t1
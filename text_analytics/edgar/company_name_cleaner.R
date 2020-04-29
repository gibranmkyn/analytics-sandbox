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


# ----- STOPWORDS
stopw_loughran_mcdonald <- c()
stopw_dictionaries <-list.files('stopw_loughran_mcdonald')

for (i in 1:length(stopw_dictionaries)) {
  file_path <-paste('stopw_loughran_mcdonald', stopw_dictionaries[i],sep="/")
  local_list <- read_lines(file_path)
  local_list <- iconv(local_list, "ASCII", "UTF-8", sub="") %>% tolower()
  stopw_loughran_mcdonald <- c(stopw_loughran_mcdonald, local_list)
}

stopw_custom <- c('vs', 'financial', 'statement', 'exhibit','report','figure','fig','tab','table', 'mda', 'company', 'page')

stopw_final <- c(stopw_loughran_mcdonald, stopw_custom)
rm(stopw_loughran_mcdonald, stopw_custom, stopw_dictionaries, file_path, local_list)

# ----- REMOVE STOPWORDS TFIDF
conn <- dbConnect(RSQLite::SQLite(), "edgar.db")
sample_reports <- dbGetQuery(conn, 'SELECT cik, cleaned_noun FROM master_index WHERE year_filed IN (2011, 2017)')

tf_idf_samples <- sample_reports %>%
  unnest_tokens(word, cleaned_noun) %>% 
  count(cik, word, sort = TRUE) %>% 
  ungroup() %>%
  bind_tf_idf(word, cik, n)

rm(sample_reports)

summarised_tf_idf <- tf_idf_samples %>%
  group_by(word) %>%
  summarise(avg_tf_idf = mean(tf_idf)) %>%
  arrange(desc(avg_tf_idf))

rm(tf_idf_samples)

ggplot(summarised_tf_idf, aes(x=avg_tf_idf)) + 
  geom_histogram(color="black", fill="black", bins = 200) + 
  scale_y_log10() +
  labs(title = "TF-IDF Distribution on log-scale")


top_90_percent <- summarised_tf_idf %>%
  top_frac(0.90) %>%
  arrange(desc(avg_tf_idf))

buttom_10_percent <- summarised_tf_idf %>%
  anti_join(top_90_percent) %>%
  arrange(desc(avg_tf_idf))

stopw_tfidf <- buttom_10_percent$word
stopw_final <- c(stopw_final, stopw_tfidf)

rm(stopw_tfidf, summarised_tf_idf, top_90_percent, buttom_10_percent)


# ------ SET UP DATABASE

#dbExecute(conn, 'ALTER TABLE master_index ADD COLUMN cleaned_text TEXT;') # add cleaned_text to master index
#dbExecute(conn, 'ALTER TABLE master_index ADD COLUMN cleaned_noun TEXT;') # add clean noun column

# ------ NAME CLEANER 10Q

conn <- dbConnect(RSQLite::SQLite(), "edgar.db")

market_level <- dbGetQuery(conn, 'SELECT company_name, master_index.accession_number, cleaned_10q.cleaned_text, cleaned_10q.cleaned_noun
           FROM master_index INNER JOIN cleaned_10q ON master_index.accession_number = cleaned_10q.accession_number ')


for(i in 1:nrow(market_level)){
  tryCatch({
  x <- market_level[i,]
  name <- strsplit(tolower(x[1]), " ")[[1]]
  
  cleaned_noun <- x %>%
    unnest_tokens(word, cleaned_noun) %>%
    filter(!word %in% name) %>%
    filter(!word %in% stopw_final) %>%
    summarise(cleaned_noun = paste(word, collapse = " "))
  
  cleaned_text <- x %>%
    unnest_tokens(word, cleaned_text) %>%
    filter(!word %in% name) %>%
    filter(!word %in% stopw_final) %>%
    summarise(cleaned_text = paste(word, collapse = " "))
  
  accession_number <- x$accession_number[1]
  
  
  dbExecute(conn, paste0("UPDATE master_index SET cleaned_text = '",cleaned_text ,"', cleaned_noun = '",cleaned_noun ,"' WHERE accession_number = '",accession_number ,"'"))
  rm(x, name, cleaned_noun, cleaned_text, accession_number)
  
  print(paste(i, "of", nrow(market_level), "10Q reports"))
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dbDisconnect(conn)



# ------ NAME CLEANER 10K MDA
conn <- dbConnect(RSQLite::SQLite(), "edgar.db")


market_level <- dbGetQuery(conn, 'SELECT company_name, master_index.accession_number, cleaned_10k_mda.cleaned_text, cleaned_10k_mda.cleaned_noun
           FROM master_index INNER JOIN cleaned_10k_mda ON master_index.accession_number = cleaned_10k_mda.accession_number ')

for(i in 1:nrow(market_level)){
  tryCatch({
    x <- market_level[i,]
    name <- strsplit(tolower(x[1]), " ")[[1]]
    
    cleaned_noun <- x %>%
      unnest_tokens(word, cleaned_noun) %>%
      filter(!word %in% name) %>%
      filter(!word %in% stopw_final) %>%
      summarise(cleaned_noun = paste(word, collapse = " "))
    
    cleaned_text <- x %>%
      unnest_tokens(word, cleaned_text) %>%
      filter(!word %in% name) %>%
      filter(!word %in% stopw_final) %>%
      summarise(cleaned_text = paste(word, collapse = " "))
    
    accession_number <- x$accession_number[1]
    
    
    dbExecute(conn, paste0("UPDATE master_index SET cleaned_text = '",cleaned_text ,"', cleaned_noun = '",cleaned_noun ,"' WHERE accession_number = '",accession_number ,"'"))
    rm(x, name, cleaned_noun, cleaned_text, accession_number)
    
    print(paste(i, "of", nrow(market_level), "10K MDA"))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


t2 <- Sys.time()

t2-t1

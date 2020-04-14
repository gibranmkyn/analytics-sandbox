rm(list = ls())
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
library(xml2)
t1<- Sys.time()

# ----- get stopwords
stopw_loughran_mcdonald <- c()
stopw_dictionaries <-list.files('stopw_loughran_mcdonald')

for (i in 1:length(stopw_dictionaries)) {
  file_path <-paste('stopw_loughran_mcdonald', stopw_dictionaries[i],sep="/")
  local_list <- read_lines(file_path)
  local_list <- iconv(local_list, "ASCII", "UTF-8", sub="") %>% tolower()
  stopw_loughran_mcdonald <- c(stopw_loughran_mcdonald, local_list)
}

stopw_custom <- c('vs', 'financial', 'statement', 'exhibit','report','figure','fig','tab','table', 'mda', 'company')

stopw_final <- c(stopw_loughran_mcdonald, stopw_custom)
rm(stopw_loughran_mcdonald, stopw_custom, stopw_dictionaries, file_path, local_list)

# ----- Get the udpipe model
ud_model <- udpipe_download_model(language = "english", overwrite = F)
ud_model <- udpipe_load_model(ud_model$file_model)

conn <- dbConnect(RSQLite::SQLite(), "edgar.db")

z = 0
# ----------- DARURAT
x <- dbGetQuery(conn, 'SELECT cik, count(*) FROM cleaned_10q LEFT JOIN master_index ON cleaned_10q.accession_number = master_index.accession_number GROUP BY cik')
x$cik
the_index <- dbGetQuery(conn, paste('SELECT cik, accession_number FROM master_index WHERE form_type = "10-Q" AND cik NOT IN(', paste(x$cik, collapse = ","), ')'))

# the_index <- dbGetQuery(conn, 'SELECT cik, accession_number FROM master_index WHERE form_type = "10-Q"')

split_the_index <- split(the_index, with(the_index, interaction(cik)), drop = TRUE)
rm(the_index)

for (s in 1:length(split_the_index)) {
  file_pattern <- paste0(split_the_index[[s]]$accession_number, '.html')
  listed_files <- list.files(paste0('Edgar filings_HTML view/Form 10-Q/',split_the_index[[s]]$cik[1]), pattern = paste0(file_pattern, collapse = "|"))
  accession_number <- split_the_index[[s]]$accession_number
  file_path <- paste0('Edgar filings_HTML view/Form 10-Q/',split_the_index[[s]]$cik[1],'/', listed_files)
  
  rm(file_pattern, listed_files)
  
  for(i in 1:length(file_path)) {
    doc <- xml2::read_html(file_path[i], options = "HUGE") %>%
      html_text() %>%
      tolower() %>%
      removePunctuation() %>%
      removeNumbers %>%
      stripWhitespace()
    
    doc_begin <- regmatches(doc,gregexpr("(?<=notes to).*",doc,perl=TRUE))[[1]] # regex to slice document from chapter notes on financial statements
    if(length(doc_begin) > 0) {doc_begin <- doc_begin} else {doc_begin <- regmatches(doc,gregexpr("(?<=notes).*",doc,perl=TRUE))[[1]]} 
    
    doc <- regmatches(doc_begin,gregexpr(".*(?<=item exhibits)",doc,perl=TRUE))[[1]][1] # regex to slice document until chapter "exhibits"  
    rm(doc_begin)
    
    cleanedDoc <- gsub("<.*?>", "", doc)
    cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
    cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
    cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
    cleanedDoc <- gsub("\\d+", "", cleanedDoc)
    cleanedDoc <- gsub("&#;", "", cleanedDoc)
    cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
    cleanedDoc <- cleanedDoc[cleanedDoc != ""]
    cleanedDoc <- cleanedDoc[cleanedDoc != " "]
    cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
    cleanedDoc <- cleanedDoc[cleanedDoc != ","]
    
    sub_this <- c("table of contents",
                  "page",
                  "financial information",
                  "financial statements",
                  "summary of significant accounting policies",
                  "(continued)",
                  "financial intstruments",
                  "derivatives and hedging activities",
                  "fair value hedges",
                  "fair value measurements",
                  "results of operations",
                  "financial statements",
                  "consolidated statements",
                  "consolidated financial",
                  "notes to consolidated financial statements",
                  "quantitative and qualitative disclosure about market risk",
                  "controls and procedures",
                  "other information",
                  "legal proceedings",
                  "risk factors",
                  "unregistered sales of equity securities and use of proceeds",
                  "management discussion and analysis of financial condition and results of operations",
                  "managements discussion and analysis of financial condition and results of operations",
                  "exhibits") #company_name
    cleanedDoc <- gsub(paste0(sub_this, collapse = '|'),"", cleanedDoc)
    
    text_transformed <- tibble(accession_number = accession_number[i], cleaned = cleanedDoc)
    
    
    # ----- Tokenisation and Part-of-speech Tagging
    tokenised <- text_transformed %>%
      select(accession_number, cleaned) %>%
      unnest_tokens(word, cleaned) %>%
      group_by(accession_number, word) %>%
      filter(!word %in% stopw_final)
    
    rm(text_transformed)
    
    # Udpipe Annotating
    local_df <- udpipe_annotate(tokenised$word,
                                doc_id = tokenised$accession_number,
                                object = ud_model) %>% as.data.frame()
    rm(tokenised)
    
    # Get nouns only
    annotated_nouns <- local_df %>% 
      filter(upos == "NOUN") %>%
      select(doc_id,lemma) %>% 
      group_by(doc_id) %>% 
      summarise(cleaned_noun = paste(lemma, collapse = " ")) %>% 
      rename(accession_number = doc_id)
    
    # Get the most important POS
    annotated_full <- local_df %>% 
      filter(upos %in% c("ADV","ADJ","NOUN", "AUX", "PART")) %>%
      select(doc_id,lemma) %>% 
      group_by(doc_id) %>% 
      summarise(cleaned_text = paste(lemma, collapse = " ")) %>% 
      rename(accession_number = doc_id)
    
    # Store the data into lists we created before for loop
    local_df <- annotated_nouns %>%
      left_join(annotated_full, by= 'accession_number')
    
    # ----- Insertion to SQL table
    dbWriteTable(conn,"cleaned_10q", local_df, append = TRUE) # create master_index Table
    
    temp_report <- dbGetQuery(conn, paste0('SELECT cik, quarter ,company_name, year_filed, form_type, cleaned_10q.accession_number 
                         FROM master_index LEFT JOIN cleaned_10q ON cleaned_10q.accession_number = master_index.accession_number 
                         WHERE cleaned_10q.accession_number = "',local_df$accession_number[1],'"'))
    
    print(paste(temp_report$form_type, temp_report$quarter, temp_report$year_filed, 'report for CIK:',temp_report$cik, temp_report$company_name, 'has been processed.' ))
    
    rm(annotated_nouns, annotated_full, local_df, temp_report)
  }
  
  z <- z + 1
  print(paste(z, 'out of 19'))
}
t2 <- Sys.time()
t2-t1
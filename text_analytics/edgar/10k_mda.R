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
t1 <- Sys.time()

conn <- dbConnect(RSQLite::SQLite(), "edgar.db")

maseter_index_list <-list.files('Master Indexes')
sp500 <- read_csv('sp500.csv')
for(i in 1:length(maseter_index_list)) {
  
  load(paste0('Master Indexes/',maseter_index_list[i]))
  
  local_df <- year.master %>%
    filter(cik %in% sp500$CIK, form.type %in% c('10-Q', '10-K')) %>%
    mutate(date.filed = as.Date(date.filed)) %>%
    mutate(year_filed = year(date.filed)) %>%
    mutate(accession.number = gsub(".*/", "", edgar.link)) %>%
    mutate(accession.number = gsub('.txt','',accession.number)) %>%
    select(-edgar.link)
  
  colnames(local_df) <- gsub("\\.", "_", colnames(local_df)) # column names with dots (.) will confuse SQL
  
  dbWriteTable(conn,"master_index", local_df, append = TRUE) # create master_index Table
}

dbWriteTable(conn,"sp500", sp500) # create sp500 table for information about S&P500
rm(local_df, year.master, sp500)


dbGetQuery(conn, 'SELECT count(cik) from master_index') # 20678
dbGetQuery(conn, 'SELECT count(cik) from sp500') # 505

dbDisconnect(conn)



## MD&A chapter 10k reports
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

the_index <- dbGetQuery(conn, 'SELECT accession_number FROM master_index WHERE form_type = "10-K" ORDER BY date_filed')

split_size <- 50
split_the_index <- split(the_index$accession_number, ceiling(seq_along(the_index$accession_number)/split_size))

rm(the_index)

for (s in 1:length(split_the_index)) {
  file_pattern <- paste0(split_the_index[[s]], '.txt')
  listed_files <- list.files('MD&A section text', pattern = paste0(file_pattern, collapse = "|"))
  file_path <- paste0('MD&A section text/', listed_files)
  
  rm(file_pattern, listed_files)
  
  for(i in 1:length(file_path)) {
    
    # ----- Clean Text
    text_file <- read_lines(file_path[i])
    text_transformed <- tibble(
      company_name = tolower(gsub('Company Name: ','',text_file[2])), 
      accession_number = gsub('Accession Number: ','',text_file[5]), 
      mgmtdisc = gsub(" s "," ",tolower(text_file[8]) %>% 
                        removePunctuation()) %>%
        removeNumbers() %>%
        stripWhitespace())
    
    rm(text_file)
    
    company_name <- unlist(str_split(text_transformed$company_name, " ", n= nchar(text_transformed$company_name)))[1]
    
    sub_this <- c("item","management", "managements", "discussion and analysis", "financial condition", "results of operations",
                  company_name)
    text_transformed$cleaned <- gsub(paste0(sub_this, collapse = '|'),"", text_transformed$mgmtdisc)
    
    rm(company_name, sub_this)
    
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
    dbWriteTable(conn,"cleaned_10k_mda", local_df, append = TRUE) # create master_index Table
    
    temp_report <- dbGetQuery(conn, paste0('SELECT cik, company_name, year_filed, form_type, cleaned_10k_mda.accession_number 
                         FROM master_index LEFT JOIN cleaned_10k_mda ON cleaned_10k_mda.accession_number = master_index.accession_number 
                         WHERE cleaned_10k_mda.accession_number = "',local_df$accession_number[1],'"'))
    
    print(paste(temp_report$form_type, temp_report$year_filed, 'report for CIK:',temp_report$cik, temp_report$company_name, 'has been processed.' ))
    
    rm(annotated_nouns, annotated_full, local_df, temp_report)
  }
}
rm(file_path)


t2 <- Sys.time()
t2-t1
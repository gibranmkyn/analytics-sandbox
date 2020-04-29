

rm(list = ls())
library(tidyverse)
library(edgar)
library(XML)
library(lubridate)
library(tm)
library(RSQLite)
library(tidytext)
library(udpipe)
library(rvest)

setwd('/Volumes/Buku Gibran/edgar')

t1 <- Sys.time()
conn <- dbConnect(RSQLite::SQLite(), "edgar.db")

cik <- dbGetQuery(conn, 'SELECT cik from sp500')$cik


for (c in 1:length(cik)) {
sentiment_df <- getSentiment(cik.no = cik[c], form.type = c('10-K','10-Q'), filing.year = 2009:2019)

dbWriteTable(conn,"sentiment", sentiment_df, append = TRUE) # insert to sentiment table

print(paste(cik[c],"of", length(cik)))
}

t2 <- Sys.time()
t2-t1
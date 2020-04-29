

rm(list = ls())
library(tidyverse)
library(edgar)

setwd('/Volumes/Buku Gibran/edgar')

sp500 <- read_csv('sp500.csv')

# df <- edgar::getMgmtDisc(cik.no = sp500$CIK, filing.year = c(2009:2019))
# save(df, file = 'df.rda')

df_10q <- getFilingsHTML(cik.no = sp500$CIK, form.type = '10-Q', filing.year = c(2009:2019))
save(df_10q, file = 'df_10q.rda')


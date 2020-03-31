remove_columns <- c('listing_id', 'last_scraped', 'name', 'new_description', 'transit', 'host_id.x','city', 'neighbourhood_group_cleansed','state','zipcode','country_code', 'country','amenities','calendar_last_scraped','first_review', 'file_name.y')


load('temp/tf_idf_dataframe.rda')
# ----- Data Prep for the plot
data_without_tfidfmatrix <- final_df_monthly %>%
  ungroup() %>%
  select_if(!names(.) %in% remove_columns) %>% 
  select_if(!names(.) %in% names(tf_idf_dataframe)) %>% 
  select_if(is.numeric) %>%
  filter(is.infinite(log(monthly_average_price)) == FALSE)


rm(tf_idf_dataframe.rda)
data_without_tfidfmatrix <- data_without_tfidfmatrix[,colSums(is.na(data_without_tfidfmatrix))<nrow(data_without_tfidfmatrix)] # remove NA columns

data_with_tfidfmatrix <- final_df_monthly %>%
  ungroup() %>%
  select_if(!names(.) %in% remove_columns) %>% 
  select_if(is.numeric) %>%
  filter(is.infinite(log(monthly_average_price)) == FALSE)

data_with_tfidfmatrix <- data_with_tfidfmatrix[,colSums(is.na(data_with_tfidfmatrix))<nrow(data_with_tfidfmatrix)] # remove NA columns

# ----- Building the models
price_modelx <- lm(log(monthly_average_price)~., data = data_without_tfidfmatrix)

price_modely <- lm(log(monthly_average_price)~., data = data_with_tfidfmatrix)

summary(price_modelx)
summary(price_modely)


# ----- Plot the most significant variables
head(as.data.frame(summary(modelx)$coefficients) %>%
       tibble::rownames_to_column() %>%
       mutate(`absolute t value` = abs(`t value`)) %>%
       arrange(`Pr(>|t|)`) , 30) %>%
  mutate(rowname=factor(rowname, levels=rowname)) %>%
  ggplot(aes(x = rowname, y = `absolute t value`)) + geom_col() + coord_flip()


head(as.data.frame(summary(modely)$coefficients) %>%
       tibble::rownames_to_column() %>%
       mutate(`absolute t value` = abs(`t value`)) %>%
       arrange(`Pr(>|t|)`) , 30) %>%
  mutate(rowname=factor(rowname, levels=rowname)) %>%
  ggplot(aes(x = rowname, y = `absolute t value`)) + geom_col() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(paste(deparse(substitute(price_modelx)), ': without weighted TF-IDF matrix as features has Adjusted R-squared of', round(summary(price_modelx)$adj.r.squared,3)))
print(paste(deparse(substitute(price_modely)), ': with weighted TF-IDF matrix as features has Adjusted R-squared of', round(summary(price_modely)$adj.r.squared,3)))

rm(price_modelx)
rm(price_modely)
rm(tf_idf_dataframe)
rm(data_without_tfidfmatrix)
rm(data_with_tfidfmatrix)

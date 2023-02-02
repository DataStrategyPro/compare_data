library(tidyverse)
library(dbplyr)

#check_db_df_tables

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mpg)

## map_table_columns----

## check_unique(df,select)-------
# Are the fields used for joining unique? 
  

df_null <- tibble(id=1:5,category=c(rep('A',3),rep('B',2)),value=c(2,2,NA,NA,3)) 
df_null
copy_to(con,df_null,overwrite = TRUE)
df <- tbl(con,'df_null')

check_null_count <- function(df,table_name=deparse(substitute(df)),test_name=NULL){
  
  if(is.null(test_name)){
    test_name = paste0(table_name,group_vars(df),'_null_count')
  }
  df %>% 
    mutate_all(is.na) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    mutate(test_name=test_name) %>% 
    select(test_name,everything())
}
df %>% 
  group_by(category) %>% 
  check_null_count()  

deparse(substitute(df))

df %>% group_vars()

## check_null_pct(df)
check_null_pct <- function(df,gb=NULL){
  row_count <- db_null %>% count() %>% pull(n)
  
  df %>% 
    mutate_all(~as.double(is.na(.))/row_count) %>% 
    summarise_all(sum,na.rm=TRUE)
}

df %>% group_by(NULL)

df %>% 
  check_null_pct() 

check_contains_nulls <- function(df,gb){
  df <- check_null_pct(df)  
  df %>% 
    mutate_all(~ifelse(.>0,'Nulls','No Nulls'))
}

db_null %>% 
  check_contains_nulls()
  
  
check_distinct_count <- function(df){
  df %>% mutate_all(~count(distinct(.)))
}



  
# Reviews the sum, min, max, mean, median, count for fields
check_stats <- function(df){
  df %>% summarise_if(is.numeric,list(min=min,max=max,avg=mean,med=median,sum=sum))
}

df %>% group_by(category) %>% 
  check_stats()

# In double entry book keeping accounts should balance to zero when grouped by 
# Chart of Account Code and Date
check_zero_balance <- function(df){
  
}


check_complete <- function(df,ref){
  
}
# df is the table you want to check against the ref table to check for completeness
# the ref table has a list of all the combinations that must be present in the df table to pass


check_diff_on_fields <- function(df,ref,common_fields){
  
}
# df is the table you want to check against the ref table to check for variance on a specified numeric field
# numeric field from table df and ref should be equal when compared on a common aggregate

check_diff <- function(df,ref){
  
}


check_acceptable <- function(df){
  
}
# table x has a list of all of the acceptable values
# table y is compared to table x and fails if a combination is not within the acceptable values


get_group_samples <- function(df,lkp,n){
  
}

# Uses machine learning to help investigate the causes of failed records. 
# Identifies the relationship
investigate_results <- function(df){
  
}

summarise_results <- function(folder){
  
}

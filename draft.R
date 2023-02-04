library(tidyverse)
library(dbplyr)

#check_db_df_tables

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mpg)

## map_table_columns----

## check_unique(df,select)-------
# Are the fields used for joining unique? 
  

# Setup test data ---------------------------------------------------------

df_null <- tibble(id=1:5,category=c(rep('A',3),rep('B',2)),value=c(2,2,NA,NA,3)) 
df_null
copy_to(con,df_null,overwrite = TRUE)
df <- tbl(con,'df_null')
df

copy_to(con,mpg,overwrite = TRUE)
db_mpg <- tbl(con,'mpg')

ref <- tibble(id=2:6,category=c(rep('A',3),rep('B',2)),value=c(2,2,NA,NA,3))
copy_to(con,ref,overwrite = TRUE)
db_ref <- tbl(con,'ref')

# Helper functions --------------------------------------------------------



mk_test_name <- function(df,test_name='test',table_name=NULL){
  # Example of how to get the name of an object back as a character vector
  # This only works before grouping is applied to the object
  # This means to use this we need to figure out how to apply a dynamic grouping (that handles no group) 
  # applied to the data after extracting the name 
  if(is.null(table_name)){
    table_name <- deparse(substitute(df))
  }
  test_name = str_flatten(c(table_name,test_name,group_vars(df)),collapse = '_',na.rm = TRUE)
  return(test_name)
}

add_test_name <- function(df,test_name=NULL){
  df %>%     
    mutate(test_name=test_name) %>% 
    select(test_name,everything())
}

gb <- function(df,gb=NULL){
  if(!is.null(gb)){
    df <- df %>% group_by(!!!syms(gb))
  }
  return(df)
}

class(df)

df %>% head(1000)


write_result_csv <- function(df,test_name,limit=1000,write=FALSE){
  if(write){
    df_rows <- df %>% count() %>% pull(n)
    if(df_rows > limit){
      print(paste('Writting',limit,'rows out of',df_rows,'. To return more rows adjust the limit'))
    }
    if(is(df,'tbl_sql')){
      df <- df %>% head(limit) %>%  collect()
    }
    folder = paste0('output/',lubridate::today(),'/')
    fs::dir_create(folder)
    df %>% write_csv(paste0(folder,test_name,'.csv'))
  }
}

db_write

df %>% write_result_csv('test')

df %>% group_by(category) %>% 
  mk_test_name()
mk_test_name(df)

df_ws <- mpg %>% head(10) %>% 
  mutate_if(is.character,~str_c(.,' ')) 

sql_query_insert(con,x_name = 'mpg',y = df_ws,conflict = 'ignore') 

copy_to(con,df = df_ws,name = 'mpg',append=TRUE) %>% show_query()

DBI::dbWriteTable(con,'mpg',df_ws,append=TRUE) %>% sql_render()
dbWriteTable(con, "test_table", mtcars[2:5, ], append = TRUE)


db_mpg %>% count()

db_mpg %>% 
  rows_append(df_ws,in_place = TRUE,copy = TRUE) %>% 
  show_query()

dbplyr::db

db_mpg %>% count()


rows_insert()

mpg %>% count()
check_white_space(df){
  
}

df %>% map(unique)


get_distinct_col_values <- function(df,col){
  df %>% 
    count(!!sym(col)) %>% 
    pivot_longer(cols = !!sym(col), names_to = 'column_name',values_to = 'distinct_values') %>% 
    mutate(
      white_space = str_length(distinct_values) > str_length(trimws(distinct_values)),
      result = case_when(white_space==1 ~ 'Fail', .default = 'Pass'),
      result_detail = case_when(white_space==1 ~ paste0("Column [",column_name,"] '",distinct_values,"' contains white space"), .default = ''),
      ) %>% 
    collect()
}



# Check Functions ----------------------------------------------------------
check_white_space <- function(df,table_name=deparse(substitute(df)),test_name='white_space'){
  test_name <- paste(table_name,test_name,sep = '_')
  
  df %>% 
    select_if(is.character) %>% 
    names() %>% 
    map_df(~get_distinct_col_values(df,.)) %>% 
    group_by(result,result_detail,column_name) %>% 
    summarise(n=sum(n)) %>% 
    mutate(pct = n / sum(n)) %>% 
    add_test_name(test_name)
}

  
db_mpg %>% check_white_space()


check_null_count <- function(df,gb=NULL,test_name=NULL){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'null_count')
  }
  df <- gb(df,gb)
  
  df %>% 
    mutate_all(is.na) %>% 
    summarise_all(sum,na.rm=TRUE) %>%
    pivot_longer()
    add_test_name(test_name)
}

df %>% summarise_all(sum) %>% 
  pivot_longer(cols = everything(),names_to = 'column_name',values_to = 'value')

df %>% check_null_count()  
df %>% check_null_count('category')  
df %>% check_null_count(gb=c('category','id'))  
df %>% check_null_count(gb=c('category','id'),test_name = 'asdf')  

# If nulls allowed warning, otherwise fail
check_null_pct <- function(df,gb=NULL,test_name=NULL){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'null_pct')
  }
  row_count <- df %>% count() %>% pull(n)
  df <- gb(df,gb)
  
  df %>% 
    mutate_all(~as.double(is.na(.))/row_count) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    add_test_name(test_name)
}

row_count <- df %>% count() %>% pull(n)

df
df %>% 
  group_by(id) %>% 
  mutate_all(~as.double(is.na(.))/row_count) %>% 
  summarise_all(sum,na.rm=TRUE) %>% 
  pivot_longer(-category,names_to = 'column') %>% 
  mutate(n=1)

df %>% check_null_pct()  
df %>% check_null_pct('category')  
df %>% check_null_pct(gb=c('category','id'))  
df %>% check_null_pct(gb=c('category','id'),test_name = 'asdf')  


check_contains_nulls <- function(df,gb=NULL,test_name=NULL){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'contains_nulls')
  }
  
  df <- check_null_pct(df,gb)  
  print(test_name)
  df %>% 
    mutate_all(~ifelse(.>0,'Complete','Has Nulls')) %>%
    add_test_name(test_name)
  # Should this return boolean or text? Text is easier for business users to interpret 
}

df %>% check_contains_nulls()  %>% sql_render()
df %>% check_contains_nulls('category')  
df %>% check_contains_nulls(gb=c('category','id'))  
df %>% check_contains_nulls(gb=c('category','id'),test_name = 'asdf')  

check_distinct_count <- function(df,gb=NULL,test_name=NULL){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'distinct_count')
  }
  df <- gb(df,gb)
  
  df %>% mutate_all(~count(distinct(.))) %>% 
    add_test_name(test_name)
}

df %>% check_distinct_count()  
df %>% check_distinct_count('category')  
df %>% check_distinct_count(gb=c('category','id'))  
df %>% check_distinct_count(gb=c('category','id'),test_name = 'asdf')  
  


# Reviews the sum, min, max, mean, median, count for fields
check_stats <- function(df,gb=NULL,test_name=NULL){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'stats')
  }
  df <- gb(df,gb)
  
  df %>% summarise_if(is.numeric,list(min=min,max=max,avg=mean,med=median,sum=sum)) %>% 
    add_test_name(test_name)
}

df %>% check_stats()  
df %>% check_stats('category')  
df %>% check_stats(gb=c('category','id'))  
df %>% check_stats(gb=c('category','id'),test_name = 'asdf')  


# In double entry book keeping accounts should balance to zero when grouped by 
# Chart of Account Code and Date
check_zero_balance <- function(df,value_col,gb=NULL,test_name=NULL){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'zero_balance')
  }
  df <- gb(df,gb)
  
  df %>% 
    summarise(balance = sum(.data[[value_col]],na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(
      result = case_when(balance == 0 ~ 'Pass', TRUE ~ 'Fail'),
      result_detail = case_when(balance == 0 ~ '', TRUE ~ 'Does not balance to zero')) %>% 
    add_test_name(test_name)
}

df %>% check_zero_balance('value')  
df %>% check_zero_balance('value','category')  
df %>% check_zero_balance('value',gb=c('category','id'))  
df %>% check_zero_balance('value',gb=c('category','id'),test_name = 'asdf')  


# df is the table you want to check against the ref table to check for completeness
# the ref table has a list of all the combinations that must be present in the df table to pass
check_complete <- function(df,ref){
  
}




# df is the table you want to check against the ref table to check for variance on a specified numeric field
# numeric field from table df and ref should be equal when compared on a common aggregate
check_diff_on_fields <- function(df,ref,df_value_col,ref_value_col=df_value_col,gb=NULL,test_name=NULL){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'diff_on_fields')
  }
  
  df <- df %>% 
    group_by(!!!syms(gb)) %>% 
    summarise(df_value = sum(!!sym(df_value_col),na.rm = TRUE),df_n = n())

  ref <- ref %>% 
    group_by(!!!syms(gb)) %>% 
    summarise(ref_value = sum(!!sym(df_value_col),na.rm = TRUE),ref_n = n())
  
  df %>% 
    full_join(ref,by = gb,copy = TRUE) %>% 
    mutate(
      diff = df_value - ref_value,
      n = (ifelse(is.na(df_n),0,df_n) + ifelse(is.na(ref_n),0,ref_n))/2,
      pct = n / sum(n),
      result = case_when(diff == 0 ~ 'Pass', TRUE ~ 'Fail'),
      result_detail = case_when(
        diff == 0 ~ '',
        is.na(df_value) ~ 'Not in data',
        is.na(ref_value) ~ 'Not in ref',
        diff > 0 ~ 'data is greater than source',
        diff < 0 ~ 'data is less than source',
        TRUE ~ 'Unexpected error'
        )
      ) %>%
    select(-df_n,-ref_n) %>% 
    add_test_name(test_name)  
}

df %>% check_diff_on_fields(ref,'value',gb='id')  
df %>% check_diff_on_fields(ref,'value',gb=c('id','category'))  
df %>% check_diff_on_fields(ref,'value',gb=c('category'))  
df %>% check_diff_on_fields(ref,'value',gb=c('category','id'),test_name = 'asdf')  



check_diff <- function(df,ref){
  
}


# table x has a list of all of the acceptable values
# table y is compared to table x and fails if a combination is not within the acceptable values
check_acceptable <- function(df){
  
}


get_group_samples <- function(df,lkp,n){
  
}



# Uses machine learning to help investigate the causes of failed records. 
# Identifies the relationship
investigate_results <- function(df){
  
}

summarise_results <- function(folder){
  
}

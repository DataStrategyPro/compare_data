library(tidyverse)
library(dbplyr)

# Helper functions --------------------------------------------------------

mk_test_name <- function(df,test_name='test',table_name=NULL){
  # Example of how to get the name of an object back as a character vector
  # This only works before grouping is applied to the object
  # This means to use this we need to figure out how to apply a dynamic grouping (that handles no group) 
  # applied to the data after extracting the name 
  if(is.null(table_name)){
    table_name <- deparse(substitute(df))
  }
  test_name = paste(c(table_name,test_name),collapse = '_')
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


write_result_csv <- function(df,test_name,folder_name=paste0('output/',lubridate::today(),'/'),limit=1000,write=FALSE){
  if(write){
    df <- df %>% ungroup()
    
    df_rows <- df %>% count() %>% pull(n)
    
    if(df_rows > limit){
      print(paste('Writting',limit,'rows out of',df_rows,'. To return more rows adjust the limit'))
    }
    df <- df %>% head(limit)
    if(is(df,'tbl_sql')){
      df <- df %>% collect()
    }
    folder = folder_name
    fs::dir_create(folder)
    df %>% write_csv(paste0(folder,test_name,'.csv'))
  }
}

get_distinct_col_values <- function(df,col){
  tryCatch({
    df <- df %>% 
      count(!!sym(col)) %>% 
      pivot_longer(cols = !!sym(col), names_to = 'column_name',values_to = 'distinct_values') %>% 
      mutate(
        white_space = str_length(distinct_values) > str_length(trimws(distinct_values)),
        result = case_when(white_space==1 ~ 'Fail', .default = 'Pass'),
        result_detail = case_when(white_space==1 ~ paste0("Column [",column_name,"] '",distinct_values,"' contains white space"), .default = ''),
      ) %>% 
      collect()
    return(df)
  },
  error = function(e){
    print(e)
  })
}

# Check Functions ----------------------------------------------------------
# check to see if keys / combined keys are unique
check_unique_keys <- function(df,keys,test_name=NULL,write=FALSE,...){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'unique_keys')
  }

  df <- df %>% 
    count(!!!syms(keys))  %>% 
    mutate(
      result = case_when(n==1 ~ 'Pass',.default = 'Fail'),
      result_detail = case_when(n==1 ~ '',.default = paste(!!!syms(keys),'has',n,'duplicates'))
    ) %>% 
    count(result,result_detail) %>% 
    mutate(pct = n/sum(n)) %>% 
    add_test_name(test_name)
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}


check_white_space <- function(df,table_name=deparse(substitute(df)),test_name='white_space',write=FALSE){
  test_name <- paste(table_name,test_name,sep = '_')
  
  df <- df %>% 
    select_if(is.character) %>% 
    names() %>% 
    map_df(~get_distinct_col_values(df,.)) %>% 
    group_by(result,result_detail,column_name) %>% 
    summarise(n=sum(n)) %>% 
    ungroup() %>% 
    mutate(pct = n / sum(n)) %>% 
    add_test_name(test_name) 

  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}


check_null_columns <- function(df, test_name = NULL, print_sql = FALSE, write = FALSE) {
  if(is.null(test_name)) {
    test_name <- mk_test_name(df,'null_count')
  }

  row_count <- df %>% count() %>% pull(n)
  
  df <- df %>% 
    mutate_all(~is.na(.) %>% as.integer()) %>% 
    summarise_all(sum,na.rm=TRUE)

  if(is(df,'tbl_sql')){
    if(print_sql){
        print(sql_render(df))
    }
    
    df <- df %>% collect()
  }    
  
  df <- df %>% 
    pivot_longer(everything(),names_to = 'column_name',values_to = 'value') %>% 
    mutate(pct = as.double(value) / row_count,
           # this n shows the number of items in that column which meet the criteria. Consider dropping
           n = ifelse(value == 0,row_count,pct * row_count),
           result = ifelse(value == 0,'Pass','Warning'),
           result_detail = ifelse(value == 0,'No NULLs',paste0("Column [",column_name,"] contains ",round(pct * 100,0),"% NULLs")),
          # n has been adjusted to show the percentage of columns that have nulls or not. 
           n=1,
           pct = n / sum(n)
    ) %>% 
    add_test_name(test_name)
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
  
}

check_distinct_count <- function(df,gb=NULL,test_name=NULL,print_sql=FALSE,write=FALSE){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'distinct_count')
  }
  # df <- gb(df,gb)
  
  df <- df %>% 
    mutate_all(~count(distinct(.))) 
  # %>% ungroup()

  if(is(df,'tbl_sql')){
    if(print_sql){
        print(sql_render(df))
    }
    
    df <- df %>% collect()
  }    
    
  df <- df %>% 
    pivot_longer(everything(),names_to = 'column_name',values_to = 'n') %>%
    mutate(pct = as.double(n) / sum(n),
           result = 'Info',
           result_detail = column_name) %>% 
    add_test_name(test_name)

  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

# Reviews the sum, min, max, mean, median, count for fields
check_stats <- function(df,gb=NULL,test_name=NULL,write=FALSE){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'stats')
  }
  df <- gb(df,gb)
  
  df <- df %>% summarise_if(is.numeric,list(min=min,max=max,avg=mean,med=median,sum=sum)) %>% 
    add_test_name(test_name)
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

# In double entry book keeping accounts should balance to zero when grouped by 
# Chart of Account Code and Date
check_zero_balance <- function(df,value_col,gb=NULL,test_name=NULL,write=FALSE){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'zero_balance')
  }
  df <- gb(df,gb)
  
  df <- df %>% 
    summarise(balance = sum(.data[[value_col]],na.rm=TRUE),n=n()) %>% 
    ungroup() %>% 
    mutate(
      pct = as.double(n)/sum(n),
      result = case_when(balance == 0 ~ 'Pass', TRUE ~ 'Fail'),
      result_detail = case_when(balance == 0 ~ '', TRUE ~ 'Does not balance to zero')) %>% 
    add_test_name(test_name)
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

# df is the table you want to check against the ref table to check for completeness
# the ref table has a list of all the combinations that must be present in the df table to pass
check_complete <- function(df,ref){
  
}

# df is the table you want to check against the ref table to check for variance on a specified numeric field
# numeric field from table df and ref should be equal when compared on a common aggregate
check_diff <- function(df,ref,df_value_col,ref_value_col=df_value_col,gb=NULL,test_name=NULL,write=FALSE){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'diff')
  }
  
  df <- df %>% 
    group_by(!!!syms(gb)) %>% 
    summarise(df_value = sum(!!sym(df_value_col),na.rm = TRUE),df_n = n())
  
  ref <- ref %>% 
    group_by(!!!syms(gb)) %>% 
    summarise(ref_value = sum(!!sym(df_value_col),na.rm = TRUE),ref_n = n())
  
  df <- df %>% 
    full_join(ref,by = gb,copy = TRUE) %>% 
    ungroup() %>% 
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
    add_test_name(test_name)#  %>%
    # mutate(test_name = paste(test_name,!!!syms(gb)))
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

mk_acceptable_value_lkp <- function(df,gb,acceptable_value_col,write=FALSE){
  df <- df %>% 
    group_by(!!!syms(gb)) %>% 
    mutate("valid_{acceptable_value_col}s" := str_c(!!sym(acceptable_value_col),collapse = ',')) %>% 
    select(!!!syms(gb),matches(glue("valid_{acceptable_Value_col}s"))) %>% 
    distinct()

  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

check_db_to_ref <- function(dbf,ref,db_value_col=NA,ref_value_col=NA,acceptable_value_col=NA,print_sql=FALSE,subset_by_ref=FALSE,write=FALSE){
  set <- intersect(names(dbf),names(ref))
  print(glue("Tables joined on {str_c(set,collapse=',')}"))
  ref_cols <- names(ref)
  dropped_columns <- setdiff(set,ref_cols)
  summarise_cols <- na.omit(c(db_value_col,'n'))
  print(glue("Dropping from reference table {str_c(dropped_columns,collapse=',')}"))
  
  if(!is.na(acceptable_value_col)){
    ref_for_acceptable_values <- ref %>% 
      mutate_at(vars(!!syms(set)),keep_all=TRUE) %>% 
      mutate_if(is.character,replace_na,'')
  }
  ref <- ref %>% 
    distinct_at(.vars = vars(!!!syms(set)),.keep_all = TRUE) %>% 
    mutate_if(is.character,replace_na,'')
  
  df <- dbf %>% 
    mutate_if(is.character,~case_when(is.na(.) ~ '', TRUE ~ trimws(.) )) %>% 
    mutate(n=1) %>% 
    group_by(!!!syms(set)) %>% 
    summarise_at(summarise_cols,sum,na.rm=TRUE) %>% 
    mutate(.merge.x=1)
  
  if(subset_by_ref){
    df <- df %>% right_join(ref %>% mutate(.merge.y = 1),copy = TRUE)
  } else{
    df <- df %>% full_join(ref %>% mutate(.merge.y = 1),copy = TRUE)
  }
  
  df <- df %>% 
    mutate(result = ifelse(is.na(.merge.x),'Warning', ifelse(is.na(.merge.y),'Fail','Pass')),
           result_detail = ifelse(is.na(.merge.x),'Not in data', ifelse(is.na(.merge.y),'Not in reference file','Pass'))
    ) %>% 
    replace_na(list(n=1)) %>% 
    ungroup() %>% 
    mutate(pct = n / sum(n)) %>% 
    select(-.merge.x,-.merge.y) %>% 
    arrange(desc(n))
  
  if(is(dbf,'tbl_sql')){
    if(print_sql){
      sql <- df %>% sql_render()
      print(sql)
    }
    df <- df %>% collect()
  }
  
  if(is.na(db_value_col)){
    df <- df %>% select(!!!syms(ref_cols),result,result_detail,n,pct)
  }else{
    df <- df %>% select(!!!syms(ref_cols),db_value = !!sym(db_value_col),result,result_detail,n,pct)
    if(!is.na(ref_value_col)){
      df <- df %>% 
        rename(ref_value = !!sym(ref_value_col)) %>% 
        mutate(
          diff = db_value - ref_value,
          result = ifelse(replace_na(diff,0) != 0,'Fail',result),
          result_detail = ifelse(replace_na(diff,0) != 0,'Values dont match', result_detail)
        )
      relocate(diff,.after=db_value)
    }
  }
  if(!is.na(acceptable_value_col)){
    gb <- set[!set %in% acceptable_value_col]
    df_acceptable_value_lkp <- mk_acceptable_value_lkp(ref_for_acceptable_values,gb,acceptable_value_col)
    df <- df %>% left_join(df_acceptable_value_lkp,by=gb)
  }
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

# return a sample of n records for the underlying data for each group
# This function accepts dataframe and a lookup table typically a database table
# The dataframe typically a summary of the data and contains a 1 to many relationship with the lookup table.

# Often drill downs into summary data are required to diagnose issues
# For example an ID of a failed record can be used to lookup a system
# However ID's are not included in summaries because they would return too many rows
# Therefore a sample of IDs for a group of failed records can be useful


get_group_samples <- function(df,lkp,n=1,add_cols='',all_cols=FALSE,print_sql=FALSE,write=FALSE){
  df <- df %>% 
    group_by(result,result_detail) %>% 
    filter(row_number()<=n) %>% 
    ungroup()
  
  set <- intersect(names(df),names(lkp))
  df_names = names(df)
  
  db <- lkp %>% 
    inner_join(df,copy = TRUE) %>% 
    mutate(temp=1) %>% 
    group_by(!!!syms(set)) %>% 
    slice_sample(temp,n=n,with_ties=FALSE) %>% 
    select(-temp) %>% 
    select({{df_names}},any_of(add_cols),if(all_cols){everything()})
  
  if(print_sql){
    sql <- db %>% sql_render()
    print(sql)
  }
  
  df <- db %>% collect() 
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

summarise_result <- function(file){
  print(file)
  df <- read_csv(file)
  if(all(c('test_name','result','result_detail','n') %in% names(df))){
    df <- df %>% 
      janitor::clean_names() %>% 
      group_by(test_name,result,result_detail) %>% 
      summarise_at(vars(n,pct),sum,na.rm=TRUE) %>% 
      arrange(desc(n))
    
    return(df)
  }else{
    print("***** test_name, result, result_detail, n, pct are mandatory*****")
    return()
  }
}

# Loop through a folder of result output files
summarise_results <- function(folder){
  fs::dir_ls(folder) %>% 
    map_dfr(summarise_result)
}

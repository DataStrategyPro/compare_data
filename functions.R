library(tidyverse)
library(dbplyr)
library(fs)
library(reactable)

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

# check balance replaces check_zero_balance
# it adds a split for debit and credit values
check_balance <- function(df, debit_col, credit_col, gb){
  df %>% 
    mutate(n=1) %>% 
    group_by(!!!syms(gb)) %>% 
    summarise_at(vars(debit_col, credit_col, n)) %>% 
    mutate(
      balance = !!sym(debit_col) + !!sym(credit_col),
      result = ifelse(balance = 0, 'Pass', 'Fail'),
      result_detail = ifelse(balance = 0, 'Balance is zero', 'Balance not zero')
      ) %>% 
    ungroup()
}

# df is the table you want to check against the ref table to check for completeness
# the ref table has a list of all the combinations that must be present in the df table to pass
check_complete <- function(df,ref){
  
}

# df is the table you want to check against the ref table to check for variance on a specified numeric field
# numeric field from table df and ref should be equal when compared on a common aggregate
check_diff <- function(
    df,
    ref,
    df_value_col,
    ref_value_col=df_value_col,
    gb=NULL,
    test_name=NULL,
    write=FALSE, 
    precision = 2,
    df_name = 'df',
    ref_name = 'ref'
    ){
  if(is.null(test_name)){
    test_name <- mk_test_name(df,'diff')
  }
  
  df <- df %>% 
    group_by(!!!syms(gb)) %>% 
    summarise(df_value = sum(!!sym(df_value_col),na.rm = TRUE),df_n = n())
  
  ref <- ref %>% 
    group_by(!!!syms(gb)) %>% 
    summarise(ref_value = sum(!!sym(ref_value_col),na.rm = TRUE),ref_n = n())
  
  df <- df %>% 
    full_join(ref,by = gb,copy = TRUE) %>% 
    ungroup() %>% 
    mutate(
      diff = round(df_value - ref_value,precision),
      df_n = ifelse(is.na(df_n),0,df_n),
      ref_n = ifelse(is.na(ref_n),0,ref_n),
      n = max(df_n, ref_n),
      pct = as.double(n) / sum(n),
      result = case_when(diff == 0 ~ 'Pass', TRUE ~ 'Fail'),
      result_detail = case_when(
        diff == 0 ~ 'Diff = 0',
        df_n == 0 ~ paste0('Not in ', df_name),
        ref_n == 0 ~ paste0('Not in ', ref_name),
        is.na(df_value) ~ paste0('Null value in ', df_name),
        is.na(ref_value) ~ paste0('Null value in ', ref_name),
        diff > 0 ~ paste0(df_name, '_value is greater than ', ref_name, '_value'),
        diff < 0 ~ paste0(df_name, '_value is less than ', ref_name, '_value'),
        TRUE ~ 'Unexpected error'
      )
    ) %>%
    rename(
      '{df_name}_value' := df_value, 
      '{ref_name}_value' := ref_value, 
      '{df_name}_n' := df_n, 
      '{ref_name}_n' := ref_n
    ) %>% 
    # select(-df_n,-ref_n) %>% 
    add_test_name(test_name)#  %>%
    # mutate(test_name = paste(test_name,!!!syms(gb)))
  
  write_result_csv(df,test_name = test_name,write = write)
  
  return(df)  
}

mk_acceptable_value_lkp <- function(df,gb,acceptable_value_col,write=FALSE){
  df <- df %>% 
    group_by(!!!syms(gb)) %>% 
    mutate("valid_{acceptable_value_col}s" := str_c(!!sym(acceptable_value_col),collapse = ',')) %>% 
    select(!!!syms(gb),matches(glue("valid_{acceptable_value_col}s"))) %>% 
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


get_group_samples <- function(df, lkp, n_rows=1, add_cols='', all_cols=FALSE, print_sql=FALSE, write=FALSE){
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
    slice_sample(temp, n=n_rows, with_ties=FALSE) %>% 
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

pivot_results <- function(df){
  df %>% 
    transmute(
      result = factor(result, levels = c('Pass','Fail','Warning','Info')),
      pct = as.double(n) / sum(n)
    ) %>% 
    pivot_wider(
      # id_cols = test_name,
      names_from = result,
      values_from = pct,
      names_expand = TRUE,
      values_fill = 0
    )     
}

# the source should contain all of the transactions
# this add the results and result details to the transactions
# the results can then be used as a label for machine learning
# to understand the cause of failure

add_results_to_source <- function(results, source, match_on){
  source %>% 
    inner_join(
      df_results %>% 
        select(!!!syms(match_on), result, result_detail),
      by = match_on,
      copy = TRUE
    )
}

# get a sample of transactions for each result
# For reporting use replace = FALSE to pull each transaction only once
# For ML use replace = TRUE and increase n to > 1000 to perform over sampling
# Over sampling is a technique used to make an number of records per group which 
# important for machine learning to detect the results
get_transaction_sample <- function(df, n = 10, replace = FALSE){
  if(is(df,'tbl_sql')){
    df <- df %>% 
      slice_sample(by = c(result, result_detail), n = n, with_ties = FALSE) %>% 
      collect()
  }
  df %>% slice_sample(by = c(result, result_detail), n = n, replace = replace)
}


# This summary is used for the high level report view
summarise_result <- function(df){
  if(all(c('result','n') %in% names(df))){
    df <- df %>% 
      group_by(result) %>% 
      summarise_at(vars(n),sum,na.rm=TRUE) %>% 
      ungroup() %>% 
      pivot_results()
   
  }else{
    df <- df %>% 
      count(test_name) %>% 
      mutate(result = 'Info') %>% 
      pivot_results()
  }
  return(df)
}

# Summary for second layer of drill down
result_detail_summary <- function(df){
  df %>% 
    group_by(result, result_detail) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(pct = n / sum(n, na.rm = TRUE))
}

# Loop through a folder of result output files
summarise_results <- function(folder){
  fs::dir_ls(folder) %>% 
    map_dfr(summarise_result)
}

standardise_csv <- function(file,rename_list=NULL){
  test_name = path_ext_remove(path_file(file))
  df <- read_csv(file)
  df <- df %>% 
    janitor::clean_names() %>% 
    mutate(test_name = test_name)
  if(!is.null(rename_list)){
    df <- df %>% rename(any(rename_list))
  }
  return(df)
}

consolidate_results <- function(files,rename_list=NULL,test_descriptions_file=NULL){
  df <- tibble(file = files) %>% 
    mutate(
      test_name = fs::path_ext_remove(fs::path_file(file)),
      data = map(file,standardise_csv,rename_list),
      summary = map(data,summarise_result))
  
  if(!is.null(test_descriptions_file)){
    df_test_descriptions <- read_csv(test_descriptions_file)
    df <- df %>% left_join(df_test_descriptions,by = 'test_name')
  }

  return(df)
}

consolidate_results2 <- function(files,rename_list=NULL,test_detail_file=NULL){
  df <- tibble(file = files) %>% 
    mutate(
      file_name = fs::path_ext_remove(fs::path_file(file)),
      data = map(file,standardise_csv,rename_list),
      summary = map(data,summarise_result))
  
  if(!is.null(test_detail_file)){
    df_test_descriptions <- read_csv(test_detail_file)
    df <- df %>% 
      full_join(df_test_descriptions,by = 'file_name') %>% 
      filter(display == 'x') %>% 
      arrange(test_name)
  }

  return(df)
}

pct_fmt <- function(x){
  paste0(floor(x * 10000) / 100, "%")
}

display_results <- function(df_consolidated){
  df_summary <- df_consolidated %>% 
    select(test_name, summary, to_do) %>% 
    unnest(summary, keep_empty = TRUE) %>% 
    mutate_if(is.numeric,replace_na,0) 
  
  rt <- reactable(df_summary
                  , highlight = TRUE
                  # , selection='single'
                  # , onClick = 'select'
                  , filterable = FALSE
                  , defaultPageSize = 100
                  , compact = TRUE
                  #, width = 500
                  , columns = list(
                    test_name = colDef(minWidth = 170),
                    Pass = colDef(
                      cell = function(value){
                        pct_fmt(value)
                      },
                      style = function(value){
                        if (value > 0){
                          color <- "#33ca47"
                        } else{
                          color <- "#ffffff"
                        }
                        list(backgroundColor = color,color = "#ffffff")
                      }
                      ),
                    Fail = colDef(
                      cell = function(value){
                        pct_fmt(value)
                      },
                      style = function(value){
                        if (value > 0){
                          color <- "#f44336"
                        } else{
                          color <- "#ffffff"
                        }
                        list(backgroundColor = color,color = "#ffffff")
                      }),
                    Info = colDef(
                      cell = function(value){
                        pct_fmt(value)
                      },
                      style = function(value){
                        if (value > 0){
                          color <- "#2986cc"
                        } else{
                          color <- "#ffffff"
                        }
                        list(backgroundColor = color,color = "#ffffff")
                      }),
                    Warning = colDef(
                      cell = function(value){
                        pct_fmt(value)
                      },
                       style = function(value){
                         if (value > 0){
                           color <- "#e69138"
                         } else{
                           color <- "#ffffff"
                         }
                         list(backgroundColor = color,color = "#ffffff")
                       })
                  )
                  ,details = function(index){
                    df_detail <- df_consolidated[index,]$data[[1]]
                    test_description <- df_consolidated[index,] %>%
                      select(any_of('test_description')) %>%
                      pull()
                    htmltools::div(
                      htmltools::h5(test_description),
                      if (!is.null(df_detail)) {
                        reactable(df_detail,
                                  highlight = TRUE,
                                  # selection = 'single',
                                  # onClick = 'select',
                                  filterable = TRUE,
                                  defaultPageSize = 50,
                                  resizable = TRUE
                                  # Commenting out because column always needs to be there
                                  # columns = list(
                                  #   pct = colDef(format = colFormat(percent = TRUE,digits = 2))
                                  #
                                  # )
                        )
                        
                      }

                    )
                  }
  )
  return(rt)
}

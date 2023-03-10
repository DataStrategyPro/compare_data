source('functions.R')

#check_db_df_tables

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mpg)


# Setup test data ---------------------------------------------------------

df_null <- tibble(id=1:5,category=c(rep('A',3),rep('B',2)),date = rep(lubridate::ymd('2023-01-01'),5),value=c(2,2,NA,NA,3)) 

df_null
copy_to(con,df_null,overwrite = TRUE)
df <- tbl(con,'df_null')
df

copy_to(con,mpg,overwrite = TRUE)
db_mpg <- tbl(con,'mpg')

ref <- tibble(id=2:6,category=c(rep('A',3),rep('B',2)),value=c(2,2,NA,NA,3))
copy_to(con,ref,overwrite = TRUE)
db_ref <- tbl(con,'ref')


# Setup white space data --------------------------------------------------


df_ws <- mpg %>% head(10) %>% 
  mutate_if(is.character,~str_c(.,' ')) 

db_mpg %>% count()

db_mpg %>% 
  rows_append(df_ws,in_place = TRUE,copy = TRUE) 

db_mpg %>% count()

# Are the fields used for joining unique? 
tibble(ID=c(1:5,4,5), combined_key_part = 1:7) %>% 
  check_unique_keys('ID')

tibble(ID=c(1:5,4,5), combined_key_part = 1:7) %>% 
  check_unique_keys(c('ID','combined_key_part'))


db_mpg %>% check_white_space(write = FALSE) %>% 
  summarise_if(is.numeric,sum)
check_white_space(db_mpg,write = TRUE)


df %>% check_null_columns() 
df %>% check_null_columns(write = TRUE) 
df %>% check_null_columns(test_name = 'asdf')


df %>% check_distinct_count()  
df %>% check_distinct_count(write = TRUE)  
# df %>% check_distinct_count('category')  
# 
# df %>% check_distinct_count('category',write = TRUE)  
# df %>% check_distinct_count(gb=c('category','id'))  
# df %>% check_distinct_count(gb=c('category','id'),test_name = 'asdf')  

df %>% group_by(category) %>% 
  mutate_all(~count(distinct(.))) %>% 
  ungroup() %>% 
  pivot_longer(-category,names_to = 'column_name',values_to = 'n')


df %>% check_stats()  
df %>% check_stats('category')  
df %>% check_stats(gb=c('category','id'))  
df %>% check_stats(gb=c('category','id'),write = TRUE)  
df %>% check_stats(gb=c('category','id'),test_name = 'asdf')  


source('functions.R')
df %>% check_zero_balance('value')  
df %>% check_zero_balance('value','category')  
df %>% check_zero_balance('value','category',write = TRUE)  

df %>% check_zero_balance('value',gb=c('category','id'))  
df %>% check_zero_balance('value',gb=c('category','id'),test_name = 'asdf')  


mk_test_name(df)

df %>% 
  mutate(test_name = paste(!!!syms(c('id','category')))) %>% 
  mutate(test = replace(test_name,' ','_'))

df
df_result
df_result <- df %>% check_diff(ref,'value',gb='category')  

get_group_samples(df_result,df,n = 10,add_cols = c('id'))
get_group_samples(df_result,df,all_cols = TRUE)
get_group_samples(df_result,df,all_cols = TRUE)
get_group_samples(df_result,df,all_cols = TRUE)

df %>% check_diff(ref,'value',gb=c('category'))  
df %>% check_diff(ref,'value',gb=c('id','category'),write = TRUE)  
df %>% check_diff(ref,'value',gb=c('category'))  
df %>% check_diff(ref,'value',gb=c('category','id'),test_name = 'asdf')  



df_result %>% filter(result=='Fail')


# Example of how to build a function that accepts a vector of filter criteria that can be used with dbplyr
dynamic_filter <- function(df, conditions){
  condition_str <- paste(conditions, collapse=" & ")
  df %>% filter(!!rlang::parse_expr(condition_str))
}

df %>% dynamic_filter(conditions = c("category=='A'","id<=2")) %>% sql_render()
mpg %>% dynamic_filter(c("year==1999","cyl==4"))

conditions = c("category=='A'","id<=2")
paste(conditions, collapse=" & ")

df_summarised <- summarise_results('output/2023-02-18/')
df_summarised %>% as.data.frame()
df_summarised %>% write_csv('output/df_result_summary.csv')

df_summarised %>% 
  group_by(test_name,result) %>% 
  summarise_if(is.numeric,sum) %>% 
  pivot_wider(id_cols = test_name,names_from = result,values_from = pct) %>% 
  select(test_name,Pass,Fail,Info,Warning)

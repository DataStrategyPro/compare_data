source('functions.R')
source('run_helpers.R')

# Setup test data ---------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mpg)

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

### White space data

df_ws <- mpg %>% head(10) %>% 
  mutate_if(is.character,~str_c(.,' ')) 

db_mpg %>% count()

db_mpg %>% 
  rows_append(df_ws,in_place = TRUE,copy = TRUE) 

db_mpg %>% count()

### Unique key data
df_unique_key <- tibble(ID=c(1:5,4,5), combined_key_part = 1:7)
# run tests ---------------------------------------------------------------

log_name <- log_init()
log_name

run(check_unique_keys(df_unique_key,'ID',write = TRUE),log_name)

run(check_white_space(db_mpg,write = TRUE),log_name)

run(check_null_columns(df,write = TRUE),log_name)

run(check_distinct_count(df,write = TRUE),log_name)

run(check_stats(df,gb=c('category','id'),write = TRUE),log_name)

run(check_zero_balance(df,'value','category',write = TRUE),log_name)

run(df_diff <- check_diff(df,ref,'value',gb=c('id','category'),write = TRUE, df_name = 'x', ref_name = 'y'),log_name)
df_diff %>% label_transactions(df, match_on = c('id','category'))

rt_check_diff_result_details <- display_detail_summary(df_diff)

match_on = c('id', 'category')
match_on = c('category')

df_results <- check_diff(df,ref,'value',gb=c(match_on), df_name = 'df', ref_name = 'ref')

source_with_results <-  label_transactions(df_results, df, match_on = match_on)


get_transaction_sample(source_with_results,n = 10, replace = TRUE)
get_transaction_sample(source_with_results)

data <- fs::dir_ls('output/2023-03-14/',glob = '*.csv') %>%
  consolidate_results2(test_detail_file = 'data/test_details.csv')

data

# The idea of this code is that you can label the transaction table for every test
# The challenge is knowing what is the transaction table and what to match it on?
# Potentially provide all the meta data in another table that can actually call all of the tests
# and pass of the details between all the related stages of tests

# data %>% 
#   mutate(
#     label_transactions = map(data,label_transactions)
#   )

test_results <- data %>% display_results()

test_results

output_folder <- paste0("output/",lubridate::today())

rmarkdown::render('report_results.Rmd',
                  output_dir = output_folder, 
                  params = list(
                    test_results = test_results,
                    test_info = 'useuful info like environment, output folder etc'
                    ))


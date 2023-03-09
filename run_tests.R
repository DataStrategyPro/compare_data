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

run(check_diff(df,ref,'value',gb=c('id','category'),write = TRUE, df_name = 'x', ref_name = 'y'),log_name)

df_summarised <- summarise_results('output/2023-02-18/')
df_summarised %>% as.data.frame()
df_summarised %>% write_csv('output/df_result_summary.csv')

data <- fs::dir_ls('output/2023-02-18/',glob = '*.csv') %>% 
  consolidate_results(test_descriptions_file = 'data/test_descriptions.csv')

data <- fs::dir_ls('output/2023-02-18/',glob = '*.csv') %>% 
  consolidate_results2(test_detail_file = 'data/test_details.csv')

data
data %>% 
  select(test_name, summary, to_do) %>% 
  unnest(summary, keep_empty = TRUE) %>% 
  reactable()

data %>% display_results()
data
output_folder <- paste0("output/",lubridate::today())

rmarkdown::render('report_results.Rmd',
                  output_dir = output_folder, 
                  params = list(
                    summarise_folder = "output/2023-02-18/",
                    test_description_file = "data/test_descriptions.csv"
                    ))

rmarkdown::render('report_results2.Rmd',
                  output_dir = output_folder, 
                  params = list(
                    summarise_folder = "output/2023-02-18/",
                    test_detail_file = "data/test_details.csv"
                    ))


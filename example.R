source('functions.R')

set.seed(2)
n = 10

df_src <- tibble(
  category = sample(c(LETTERS[1:4]), n, replace = TRUE)
  ,amt = rnorm(n)
  ,date = lubridate::today() + sample(1:5, n, replace = TRUE)
) %>% 
  mutate(amt = ifelse(row_number() < 4, NA, amt))


df_map <- tibble(
  ID = LETTERS[c(1, 1:3)]
  ,category2 = letters[c(1, 1:3)]
)


df_target <- inner_join(df_src, df_map, by = c("category" = "ID") ,multiple = 'all')
df_target


results <- check_diff(
  df = df_src
  ,ref = df_target
  ,df_value_col = 'amt'
  ,gb = 'category'
) 

results

ml <- results %>% 
  make_features()

ml %>% glimpse()


results <- check_diff(
  df = df_src
  ,ref = df_target
  ,gb = c('category','date')
) 

results

results <- check_diff(
  df = df_src
  ,ref = df_target
  ,gb = c('category','date')
  ,df_value_col = 'amt'
  ,n_equal = TRUE
) 

results

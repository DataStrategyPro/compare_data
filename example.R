
# Setup Data --------------------------------------------------------------
source('functions.R')
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

set.seed(2)
n = 10

df_src <- tibble(
  category = sample(c(LETTERS[1:4]), n, replace = TRUE)
  ,amt = rnorm(n)
  ,date = lubridate::today() + sample(1:5, n, replace = TRUE)
) %>% 
  mutate(amt = ifelse(row_number() < 4, NA, amt))

copy_to(con,df_src)

db_src <- tbl(con,'df_src')

df_map <- tibble(
  ID = LETTERS[c(1, 1:3)]
  ,category2 = letters[c(1, 1:3)]
)


df_target <- inner_join(df_src, df_map, by = c("category" = "ID") ,multiple = 'all')

copy_to(con, df_target)

db_target <- tbl(con,'df_target')


# Preview Data ------------------------------------------------------------
db_src

df_map

df_target

# Run check ---------------------------------------------------------------

result <- check_diff(db_src, df_target, 'amt', gb='category',) 

result

result %>% sql_render()




# ML ----------------------------------------------------------------------


df_target %>% 
  mutate_all(list(isna = is.na))

df_target %>% 
  make_features() %>% 
  glimpse()
  
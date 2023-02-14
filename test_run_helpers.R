source('run_helpers.R')

log_name <- log_init()
log_name

fn_a <- function(x){
  return(x)
}

result <- run(fn_a('asdf'),log_name = log_name)
result <- run(fn_a('asdf','asdf'),log_name = log_name)
result

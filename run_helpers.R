# run(fn,log_file) adds trycatch, run times and outputs results to a log file

log_init <- function(log_name = paste0('log/log_',lubridate::today(),'.csv')){
  if(fs::file_exists(log_name)){
    fs::file_delete(log_name)
  }
  fs::dir_create(fs::path_dir(log_name))
  fs::file_touch(log_name)
  cat('fn,start_time,end_time,elapsed_time,run_status\n',file = log_name,append = TRUE)
  return(log_name)
}

log_append <- function(fn_text,start_time,end_time,elapsed_time,run_status,log_name){
  cat(paste(c(fn_text,start_time,end_time,elapsed_time,paste0(run_status,'\n')),collapse = '|'),file = log_name,append = TRUE)
}


run <- function(fn,log_name){
  run_status <- 'Fail'
  start_time <- Sys.time()
  fn_text <- deparse(substitute(fn))
  tryCatch({
    result <- eval(fn)
    run_status <- 'Ok'
  },
  error = function(e){
    print(e)
  })
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  log_append(fn_text,as.character(start_time),as.character(end_time),as.character(format(elapsed_time,digits=2)),run_status,log_name)
  return(result)
}

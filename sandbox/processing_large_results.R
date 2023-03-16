# For larger more granular datasets it's important to pull back results as a 
# lazy dataframe. This is so we don't clog up network, local compute and reporting by passing
# through millions of records. 

# Save results to lazy df lazy_results

# Generate summary from lazy_results

# label transactions with lazy_results

# Return a sample of labelled transactions


# the idea behind this function is that meta data for a test can be loaded from CSV file 
# completed by a non-technical user. This then executes the tests and saves the result_obj.

# The result_obj maybe a lazy dataframe pointing to millions of records in a database.
# As such we only want to return either an aggregate summary or sample of the data for reporting. 

# When there are lots of tests we'll need a way to easily refer to this result_obj for different tests



run_test <- function(x_table, y_table, check_fn, match_on, result_obj){
  dfx <- get(x_table)
  dfy <- get(y_table)
  fn <- get(check_fn)
  
  assign(result_obj, fn(dfx, dfy, match_on))
  
  
}

# label the transactions by specifying the result_obj string loaded from a CSV file

label_transaction_ <- function(result_obj, label_result_obj_name = NULL, ...){
  x <- get(result_obj)
  if(is.null(label_result_obj_name)){
    label_result_obj_name <- paste0('label_',result_obj)
  }
  assign(label_result_obj_name, label_transactions(x, ...))
  return(label_result_obj_name)
}


df
label_transaction_('df_diff', source='df', match_on = str_split_1("id,category", ','))

label_transactions(df_diff, df, c('id','category'))





# 
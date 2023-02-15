# Compare Tables

This library helps to validate data for data migration projects. 

It can carry out the following checks on either database or local tables. Often database tables compared to a local reference table. 

check_unique_key
check_white_space
check_null_columns
check_distinct_count
check_stats
check_zero_balance
check_complete
check_diff

It enable non-technical users to configure tests via spreadsheets and reference tables

Reduce the amount of technical resources required to setup tests by generating all of the SQL to test and investigate. discrepancies

## What it does
A set of tools to compare Local and SQL Tables to validate data is correct and free from corruption at the end of a data pipeline. 

## How it works
When data travels through a pipeline there is always risk for data corruption. 

Often this occurs when using joins to merge datasets. 
Missing data from the lookup table can cause dropped records.
Duplicate keys in the lookup table can cause duplicates in your final results. 
Calculated fields that are dependent on fields with incorrect or NULL values

Data is often aggregated or split when moving through a data pipeline which means source and target tables cannot be compared directly as the record level identifier will no longer be applicable. 

To get around this both tables will be aggregated by a common set of fields which then becomes the fields that the 2 tables are joined on. 

Often there will be a numeric field that can be summed in both tables and then subtracted from each other to find a difference. 

1. Makes 2 tables comparable
2. Compares them to find variances
3. Summarise the results of multiple tests / comparisons
4. Drill down to view the details for variances
5. Use machine learning to assist with identifying the cause of variances.

## How to use it

### Specify the table you want to validate

Table1 is the table to check. Often this is a database table that might have too many records to pull into a local dataframe

### Specify the reference table/s

Table2 is the reference table that table1 should conform to. Often there will be several reference files setup to conduct different types of tests. 

For example
Lets say we know our table to check contains a number of categorical fields that we believe should remain constant between the beginning and end of a data pipeline. One reference table might be setup aggregated those categorical fields to validate no lost or duplicated data.

Validating Data
Some reference tables can be setup to show valid combinations. 

### Review Summary Test Results
Running multiple tests can often results containing thousands of records which can be difficult to review. From a high level view it's useful to simply review a record count / percentage pass and fail rate for each test. If there is a metric column such as dollar amount, it be useful to review this too to view impact / materiality of errors. 

Summary reports contain the follow details

test_name
result
result_detail
common_grouping_columns
x_value
y_value
diff: x_value - y_value
n: count(x) + replace_na(y_value,1), this is done so that missing data can also be accounted for
pct: this should sum to 100% for each test / grouping (need to think about this one to make a meaningful summary. often clients want to split by tests by grouping to see if each group = 100%, however that means pct at a test level needs to be a weighted pct calculated off the grouping pct)
acceptable_values
top_n_detail_fields
source: server.schema.table


1. Groups by common non-numeric fields. This ensures join fields are unique and comparable between tables.
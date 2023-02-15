
# Efficient Validation of Database Tables

When data travels through a pipeline there is always risk for data corruption. 

Often this occurs when using joins to merge datasets. 
Missing data from the lookup table can cause dropped records.
Duplicate keys in the lookup table can cause duplicates in your final results. 
Calculated fields that are dependent on fields with incorrect or NULL values

Data is often aggregated or split when moving through a data pipeline which means source and target tables cannot be compared directly as the record level identifier will no longer be applicable. 

## How database testing / validation is typically done

Databases tables often contain millions of records and hundreds of columns that makes it unpractical to pull down full data sets to tools like Excel, Python or R without first aggregating away important details. 

Thus tests typically have to be written in SQL. Summary SQL results are then pulled down to different tools to evaluate against a set of rules or reference data set.

While rules can be written directly in SQL, this can involve a lot of custom code that doesn't result in a standard output. 

Often write access to the database is not available for testing, which necessitates a solution for storing reference data and results. 

After test results are obtained there needs to be an efficient method of *investigating defects* 

Writing code in R or Python can add the ability to store and work with data that exists both inside and outside of the database. It can provide automatic SQL generation, execution and summarisation of SQL results. 

R and Python are similar however R has one major advantage over Python when working with SQL. Complex checks and queries can be written very concisely in R and then automatically translated into a number of SQL back ends using dbplyr and executed on a remote SQL engine. Often this can reduce the amount of code that needs to be maintained by 10x to 100x compared to SQL depending on the complexity of the query and number of columns involved. These R queries can work with both remote SQL data sources and local data frames with exactly the same code. 

The challenge is even with R or Python code, there is still a lot of custom code that needs to be written which doesn't results in a unified standard for reporting. 

## Common Testing Patterns

check_unique_key
check_white_space
check_null_columns
check_distinct_count
check_stats
check_zero_balance
check_complete
check_diff
check_acceptable_values

investigate_details

## Standardising Output

By ensuring tests have a standard set of columns, multiple tests can easily be summarised together for high level reporting

standard columns include

test_name
result
result_detail
n
pct
link_to_detail

Additional columns can also be included in tests, however these non-generic columns are generally specific to a certain test or dataset. 


## Enablling non-technical users to configure tests 

Providing reference tables as csv files means non-technical users can configure tests without needing to write code. 

These tables are then loaded to the database as temporary tables and uses the available checking functions to write, execute and summarise the tests in SQL. 

For example a valid list of values for a column can be provided as a table instead of coding as rules. 

A snapshot of the correct data can be used as a reference table to regression test your table after database changes. 

This can be helpful as it makes it easier for SMEs to create, update, validate and understand the rules. 

## Investigating Defects

When defects are detected it can take a significant amount of time to investigate the causes of defects. 

Typically more detail is required which means writing more custom SQL to subset the data on the failed results and then in the case of large data sets reducing the amount of data returned either via aggregation or sampling.

Often there can be a lot of going back and forth between the teams who write the SQL and the teams who review the data. 

There are several approaches that can be utilised to reduce the effort involved in defect investigation.

### Provide a sample of key fields with summarised test results
For example you could return 10 IDs from 1 million failed IDs. This can make it easier to investigate specific records. 

### Provide a list of the acceptable values
When a value doesn't match a list of acceptable values, return the list of acceptable values along with the failed result to assist with error diagnosis.

### Machine Learning Defect Investigation

Machine learning techniques have already been successfully used to diagnose some types of defects. This works well when there is a pattern in a table with hundreds columns and you're not sure which columns are relevant to the issue. 

For example data can end up dropping because it is dependent on data in another column. 

By scanning all of the columns the machine learning algorithm can help isolate the relevant columns. 

More sophisticated use cases are also possible, and available in some of my more technical documentation. 

### Self Service Data Exploration 

Self service data exploration tools such as Power BI and Tableau can be used to assist non-technical SMEs to explore and investigate defects on their own. However there are a number of drawbacks to using these tools.

1. They don't have the test results or capability to automatically generate exploratory database queries based on those test results. Users will instead need to write the SQL to connect to the appropriate data sources and then manually apply filters to find the relevant subset, which could be the same as reimplementing the test logic. 

2. These tools can have a high license cost associated with them.

R can also provide a no code self service data exploration tool specifically tailored to investigating defects. R already has knowledge of the test logic and is capable of dynamically generating SQL. There are a number of options for deploying a solution which range from free to very low cost. 


## R Deployment Options

R is a free open source tool which can easily be installed on any users computer

The renv package can be used to easily replicate the r environment for different users to ensure consistent running of code. 

An R Shiny application offers non-techincal users a no-code interface explore results and drill deeper into the database to investigate issues. 

Shiny can run on any computer running R.

Shiny can also quickly and easily be deployed using docker containers on cloud service providers such as Azure, AWS or GCP. This enables users to access the application via an intranet web site without the requirement to install any software. 


## Logging Test Runs

Sometimes a test may fail to run, either due to availability of data or incorrect test implementation. Failed tests don't return results, however they will output to a log file along with successfully run tests, displaying information such as the test name, run times, test run status, (also looking to add server name, schema name, table name)







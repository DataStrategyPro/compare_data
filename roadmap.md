# Drill down
Create a drillable table that reports test results and their appropriate details

This illustrates the hierarchy for displaying test results
match_on

result > result_detail > custom_grouping > transaction_sample

This hierarchy does not represent the order of implementation though. Implementation needs to be done in a certain order, otherwise the data will not be available to implement that step. 

Those steps are

1. match_on
2. result & result_detail
3. transaction_label
4. transaction_sample
5. custom_group_reporting

## step 1: match_on

When we first compare a dataset we want to match_on the lowest common aggregate between datasets. Sometimes this is the primary key which means we are already comparing at the record level. However often 2 datasets are only comparable when they are aggregated. For example when records are merged or split. The lower the level the aggregate the more precisely we can identify where issues occur. 

Often it is required to compare data sets with millions of records. As it's not practical to pull millions of records from the database to do a comparison, people often result to higher level aggregates. 

Higher level aggregates lead to more investigation work as you then need to take several more steps to filter and drill down and aggregate again on the right dimension to isolate the affected records. 

If we leave processing inside the database engine we can easily processing millions of records at the transaction level, isolate the defects on the first pass and then return high level aggregates only for reporting purposes. 

## Step 2: result & result_detail
depending on the level of granularity available the match_on step may have more records than is practical to return 

result & result_detail are high level aggregates of the match_on result set where the permutations are limited to
result: Pass, Fail, Warning, Info
result_detail: x = y, x > y, x < y, x has a record but value is NULL, x record is missing, y has a record but value is NULL, y record is missing

This limited number of permutations means we can always return 100% of the aggregated data without overloading a report with potentially millions of records

## step 3: transaction_label 
Once we have the Pass / Fail results and result_details from the match_on process we can augment the original source data with these 2 fields which means we have Pass / Fail labelled at the transaction level. We won't use this data directly as it will often return too many records to a report. 

We will however use this as a data source for steps 4 & 5.


## Step 4: transaction_sample
Often we want to see the transaction level detail behind failed records so that we can understand and diagnose why an error has occurred. Since there are potentially millions of records at the transaction level we limit the number of records returned to a sample size of say 10 to 100 records per result_detail for reporting purposes. 

Basically just enough for a human to be able to scan the records and get a sense of the data without overloading a report which can be problematic for both humans and local computer systems. 

Note that larger transaction samples of 1,000 to 10,000 records per result_detail can be done for machine learning models to help diagnose the cause of issues without requiring human intervention. 


## Step 5: custom_group_reporting
Often clients have logical ways of grouping their data and they would like to explictly see Pass / Fail results for each grouping. 

As this view is aggregated it often won't contain the details necessary to diagnose issues. 

We also need to be careful during implementation that the clients specified groupings contain a number of permutation that won't overload the report and cause system failure. 

Potential solutions to this include using a factor_lump type method to limit the number of records reported. 

Aggregations can be carried out from the transaction_label step as this provides as this will contain all of the fields available for aggregation. 

If available this can also potentially be done from the match_on step as this will contain records from both x and y tables. Note though that the fields for reporting will not be available unless they were part of the match_on fields. 







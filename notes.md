
target environment is R 4.1.2 using SQL Server 2016 

Look to use simulate_mssql(version = '2016') 

eg

df <- lazy_frame(df,con = simulate_mssql(version = '2016'))

Note that this doesn't actually connect to a db or execute a connection, so may need to setup a SQL server instance for proper testing as functions don't always return a tbl_sql object

All result pct should total to 100% per test

updates to fix compatibility and performance issues with SQL server including casting bit to int so it can be summed, error handling for dates mis-selected as character, collect before pivot longer to avoid duplications of calcs for every column
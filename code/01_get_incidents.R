source('code/00_dependencies.R')

con <- dbConnect(odbc(),
								 Driver = "SQL Server",
								 Server = "DEV-SQL01",
								 Database = "TheBigEasy")

incidents <- tbl(con, 'EMS_Incidents') 

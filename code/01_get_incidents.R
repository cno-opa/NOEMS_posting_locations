source('code/00_dependencies.R')

con <- dbConnect(odbc(),
								 Driver = "SQL Server",
								 Server = "DEV-SQL01",
								 Database = "TheBigEasy")

incidents <- tbl(con, 'EMS_Incidents') %>%
	collect() %>%
	write_csv('data/source_incidents/incidents.csv') %>%
	write_feather('data/source_incidents/incidents.feather')

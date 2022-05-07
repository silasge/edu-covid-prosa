library(DBI)

export_to_sqlite <- function(table_name, table_value) {
  con <- dbConnect(RSQLite::SQLite(), "./edu-covid-data/data/projeto_prosa/processed/prosa_tables.db")
  dbWriteTable(conn = con, name = table_name, value = table_value, overwrite = TRUE)
  dbDisconnect(con)
}

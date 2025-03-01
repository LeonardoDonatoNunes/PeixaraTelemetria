library(DBI)
library(RPostgres)


connect_db <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )

  return(con)

}


write_table <- function(dados, schema, table) {

  con <- connect_db()
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema=schema, table=table),
    value = dados,
    overwrite = TRUE,
    row.names = FALSE
  )
  DBI::dbDisconnect(con)

}

read_table <- function(schema, table) {
  con <- connect_db()
  dados <- DBI::dbReadTable(con, name = DBI::Id(schema='telemetria', table='base_fixa'))
  DBI::dbDisconnect(con)
  return(dados)
}

get_sql <- function(stmt) {

  con <- connect_db()
  dados <- DBI::dbGetQuery(con, stmt)
  DBI::dbDisconnect(con)
  return(dados)

}

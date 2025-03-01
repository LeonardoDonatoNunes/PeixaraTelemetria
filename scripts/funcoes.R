library(DBI)
library(RPostgres)
library(logger)
library(glue)
library(stringr)
library(dplyr)

connect_db <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )

  logger::log_info("Conexão realizada")

  return(con)

}


inserir_dados <- function(dados, schema, table) {

  con <- connect_db()
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema=schema, table=table),
    value = dados,
    overwrite = TRUE,
    row.names = FALSE
  )
  DBI::dbDisconnect(con)

  logger::log_info(glue::glue("Dados inseridos com sucesso na tabela {schema}.{table}"))

}

carregar_dados <- function(schema, table) {
  con <- connect_db()
  dados <- DBI::dbReadTable(con, name = DBI::Id(schema=schema, table=table))
  DBI::dbDisconnect(con)
  return(dados)
}

get_sql <- function(stmt) {

  con <- connect_db()
  dados <- DBI::dbGetQuery(con, stmt)
  DBI::dbDisconnect(con)
  return(dados)

}

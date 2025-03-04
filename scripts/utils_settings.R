library(DBI)
library(RPostgres)
library(logger)
library(glue)
library(stringr)
library(dplyr)
library(lubridate)
library(sf)

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

criar_schema <- function(schema) {
  con = connect_db()
  DBI::dbExecute(con, glue::glue("create schema if not exists {schema};"))
  logger::log_info(glue::glue("Schema {schema} criado!"))
  DBI::dbDisconnect(con)
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



executar_sql <- function(stmt) {

  con <- connect_db()
  DBI::dbSendQuery(con, stmt)
  DBI::dbDisconnect(con)
  logger::log_info("Query executada")
}


carregar_geom <- function(query) {
  con <- connect_db()
  tabela <- sf::st_read(con, query = query)
  DBI::dbDisconnect(con)
  return(tabela)
}



executar_sql("CREATE EXTENSION IF NOT EXISTS postgis;")


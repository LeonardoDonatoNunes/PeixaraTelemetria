source('scripts/utils_settings.R')

# Cria o schema no banco caso não exista
criar_schema('telemetria')

# Insere a tabela de bases_fixas
base_fixa = read.csv('dados/filtro/gerencia_radio.csv', encoding = 'latin1')

base_fixa_clean <-
  base_fixa %>%
  dplyr::mutate(
    data_hora_intalacao = lubridate::dmy_hm(data_hora_intalacao, tz = "UTC"),
    lat = as.numeric(lat),
    long = as.numeric(long),
    dist = dplyr::case_when(
        base_id == 'BAN' ~ 687,
        base_id == 'ARA' ~ 340,
        base_id == 'ARU' ~ 527,
        base_id == 'TOR' ~ 260,
        base_id == 'COC' ~ 589,
        base_id == 'RIB' ~ 210,
        base_id == 'BRC' ~ 409
      )
    )

inserir_dados(base_fixa_clean, 'telemetria', table = "base_fixa")
executar_sql("ALTER TABLE telemetria.base_fixa ADD COLUMN geom geometry(Point, 4326);")
executar_sql("UPDATE telemetria.base_fixa SET geom = ST_SetSRID(ST_MakePoint(long, lat), 4326);")

# Insere a tabela de marcacao
marcacao <- read.csv2('dados/filtro/cmr.csv', sep =",")



marcacao_clean <-
  marcacao %>%
  dplyr::mutate(
    data_hora_soltura = lubridate::dmy_hm(data_hora_soltura, tz = "UTC"),
    data_hora_remocao = lubridate::dmy_hm(data_hora_remocao, tz = "UTC"),
    lat = as.numeric(lat_soltura),
    long = as.numeric(long_soltura)
  ) %>%
  dplyr::filter(!transmissor_id %in% c(5020, 5022)) %>%
  dplyr::select(-lat_soltura, -long_soltura)

inserir_dados(marcacao_clean, schema = "telemetria", table = "marcacao")
executar_sql("ALTER TABLE telemetria.marcacao ADD COLUMN geom geometry(Point, 4326);")
executar_sql("UPDATE telemetria.marcacao SET geom = ST_SetSRID(ST_MakePoint(long, lat), 4326);")



# Insere os dados geográficos
## Cria o schema caso não exista
criar_schema('geografia')


## Inserir a linha do rio tocantins_araguaia
rio <- sf::st_read('dados/shp/linha_tocantins_araguaia/linha_tocantins_araguaia.shp')
cursos_dagua <- sf::st_transform(rio, 4326)
inserir_dados(cursos_dagua, schema = 'geografia', table = 'tocantins_araguaia')




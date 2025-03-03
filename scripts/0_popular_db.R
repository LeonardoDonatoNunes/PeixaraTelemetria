source('scripts/funcoes.R')

# Cria o schema no banco caso não exista
criar_schema('telemetria')

# Insere a tabela de bases_fixas
base_fixa = read.csv('dados/filtro/gerencia_radio.csv', encoding = 'latin1')

base_fixa_clean <-
  base_fixa %>%
  dplyr::mutate(data_hora_intalacao = lubridate::dmy_hm(data_hora_intalacao, tz = "UTC"))

inserir_dados(base_fixa_clean, 'telemetria', table = "base_fixa")


# Insere a tabela de marcacao
marcacao <- read.csv2('dados/filtro/cmr.csv')

marcacao_clean <-
  marcacao %>%
  dplyr::mutate(
    data_hora_soltura = lubridate::dmy_hm(data_hora_soltura, tz = "UTC"),
    data_hora_remocao = lubridate::dmy_hm(data_hora_remocao, tz = "UTC")
  ) %>%
  dplyr::filter(!transmissor_id %in% c(5020, 5022))

inserir_dados(marcacao_clean, schema = "telemetria", table = "marcacao")

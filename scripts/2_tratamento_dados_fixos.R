library(dplyr)
library(stringr)
library(DBI)
source('scripts/funcoes.R')

dir_name = 'dados_combinados'
arquivos = list.files(dir_name)
marcacao = read.csv('dados/filtro/transmissor.csv')
bases = openxlsx::read.xlsx('dados/filtro/gerencia_radio.xlsx')


dados_fixo <- data.frame()
for (arquivo in arquivos) {
  arquivo_path <- file.path(dir_name, arquivo)
  dados_i <- read.table(arquivo_path, sep = '\t', header = T)
  dados_fixo <- rbind(dados_fixo, dados_i)
}

# Filtro 1
## Remover resitros com radio codigo 999
## Remover registros que não sejam de transmissores ativos
## Remover registros duplicados em todas as colunas
dados_filtro_1 <-
  dados_fixo %>%
  dplyr::filter(stringr::str_sub(radio_id, -3) != 999) %>%
  dplyr::inner_join(marcacao, by = 'radio_id') %>%
  dplyr::mutate(data_hora = as.POSIXct(data_hora)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(base_id, radio_id, data_hora)

options(scipen = 999)

# Filtro 2
## Manten registros consecutivos com diferença de tempo de até x segundos (x = tempo_entre_deteccoes_consecutivas)
## Cria blocos de detecções com quebras de tempo entre detecções consecutivas de mais de x segundos (x = tempo_entre_blocos_deteccoes)
## Mantém somente blocos de detecção com no mínimo x detecções (x = numero_minimo_deteccoes_bloco)

tempo_entre_deteccoes_consecutivas = 3 * 60 # minutos * seguntos
tempo_entre_blocos_deteccoes = 12 * 60 * 60 # horas * minutos * segundos
numero_minimo_deteccoes_bloco = 3 # Número mínimo de detecções para manter um bloco

dados_filtro_2 <-
  dados_filtro_1 %>%
  dplyr::group_by(base_id, radio_id) %>%
  dplyr::mutate(time_diff = difftime(data_hora, dplyr::lag(data_hora, 1), units = 'secs')) %>%
  dplyr::filter(time_diff <= tempo_entre_deteccoes_consecutivas | is.na(time_diff)) %>%
  dplyr::mutate(
    time_diff2 = difftime(data_hora, dplyr::lag(data_hora, 1), units = 'secs'),
    quebra = ifelse(time_diff2 < tempo_entre_blocos_deteccoes | is.na(time_diff2), 0, 1),
    bloco = cumsum(quebra)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(base_id, radio_id, bloco) %>%
  dplyr::summarise(
    data_hora_ini = min(data_hora),
    data_hora_fim = max(data_hora),
    n_deteccoes = n(),
    tempo_residencia_s = as.integer(difftime(data_hora_fim, data_hora_ini, units = 'secs'))
  ) %>%
  dplyr::filter(n_deteccoes >= numero_minimo_deteccoes_bloco) %>%
  dplyr::select(-bloco)

## Salvar os dados no DB
conn = connect_db()

DBI::dbWriteTable(
  conn = conn,
  name = DBI::Id(schema='telemetria', table='deteccoes_radio_fixo'),
  value = dados_filtro_2,
  overwrite = TRUE,
  row.names = FALSE
)

DBI::dbDisconnect(conn)


source('scripts/utils_settings.R')

dir_name = 'dados/dados_combinados'
arquivos = list.files(dir_name)
marcacao = carregar_dados(schema = 'telemetria', table = 'marcacao')

marcacao_clean <-
  marcacao %>%
  dplyr::select(
    radio_id = transmissor_id,
    data_hora_soltura,
    data_hora_remocao) %>%
  tail(-1) # Remove a primeira linha que tem um transmissor duplicado (precisa  verificar se está errado)


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
  dplyr::inner_join(marcacao_clean, by = 'radio_id') %>%
  dplyr::mutate(data_hora = as.POSIXct(data_hora)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(base_id, radio_id, data_hora) %>%
  dplyr::filter(data_hora >= data_hora_soltura & (data_hora <= data_hora_remocao | is.na(data_hora_remocao))) %>%
  dplyr::select(-c('data_hora_soltura', 'data_hora_remocao'))



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
  dplyr::mutate(n_deteccoes = n()) %>%
  dplyr::filter(n_deteccoes >= numero_minimo_deteccoes_bloco) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c("time_diff","time_diff2","quebra","bloco","n_deteccoes"))


## Salvar os dados no DB
inserir_dados(dados_filtro_2, schema = 'telemetria', table = 'deteccao_radio_fixo')


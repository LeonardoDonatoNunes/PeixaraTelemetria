source('scripts/utils_settings.R')

dir_name = 'dados/dados_combinados'
arquivos = list.files(dir_name)
marcacao = carregar_dados(schema = 'telemetria', table = 'marcacao')
df_bi <- read.csv('dados/filtro/transmissor.csv', sep = ";") %>% dplyr::select(-transmissor_id, -sp, radio_id, bi = intervalo_radio, -duracao_estimada)

marcacao_clean <-
  marcacao %>%
  dplyr::select(
    radio_id = transmissor_id,
    data_hora_soltura,
    data_hora_remocao) %>%
  tail(-1) %>% # Remove a primeira linha que tem um transmissor duplicado (precisa  verificar se está errado)
  dplyr::left_join(df_bi) %>%
  dplyr::mutate(bi = ifelse(is.na(bi), 3, bi))

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
  dplyr::mutate(radio_id = ifelse(radio_id == 15022, 10022, radio_id)) %>%
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

tempo_entre_deteccoes_consecutivas = 5 * 60 # minutos * seguntos
numero_minimo_deteccoes_bloco = 5 # Número mínimo de detecções para manter um bloco

dados_filtro_2 <-
dados_filtro_1 %>%
  dplyr::arrange(radio_id, data_hora) %>%
  dplyr::group_by(radio_id) %>%
  dplyr::mutate(
    time_diff = as.integer(difftime(data_hora, dplyr::lag(data_hora, 1), units = 'secs')),
    lag_base_id = lag(base_id, 1),
    quebra = dplyr::case_when(
      base_id != lag_base_id ~ 1,
      time_diff%%bi != 0 ~ 1,
      time_diff >= tempo_entre_deteccoes_consecutivas ~ 1,
      is.na(time_diff) ~ 1,
      TRUE ~ 0
    ),
    grupo = cumsum(quebra)
  ) %>%
  dplyr::arrange(radio_id, data_hora) %>%
  dplyr::group_by(base_id, radio_id, grupo) %>%
  dplyr::mutate(n_obs = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n_obs >= numero_minimo_deteccoes_bloco) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c("bi", "time_diff","quebra","grupo","n_obs", "lag_base_id"))


## Salvar os dados no DB
inserir_dados(dados_filtro_2, schema = 'telemetria', table = 'deteccao_radio_fixo')


source('scripts/utils_settings.R')

base_fixa = carregar_dados(schema = 'telemetria', table = 'base_fixa')
marcacao = carregar_dados(schema = 'telemetria', table = 'marcacao')
detec_movel = carregar_dados(schema = 'telemetria', table = 'deteccao_radio_movel')
detec_fixo = carregar_dados(schema = 'telemetria', table = 'deteccao_radio_fixo')

marcacao_clean <-
  marcacao %>%
  dplyr::mutate(
    receptor_id = 0000,
    base_id = 'MAR',
    antena_id = 1,
    n_deteccoes = 1
  ) %>%
  dplyr::select(
    radio_id = transmissor_id,
    base_id,
    lat = lat_soltura,
    long = long_soltura,
    data_hora_ini = data_hora_soltura,
    data_hora_fim = data_hora_soltura,
    n_deteccoes
  )

base_fixa_clean <-
  base_fixa %>%
  dplyr::select(base_id, lat, long)

detec_movel_clean <-
  detec_movel %>%
  dplyr::mutate(
    receptor_id = 9999,
    base_id = 'MOV',
    antena_id = 1
  ) %>%
  dplyr::select(
    "receptor_id",
    "base_id",
    "antena_id",
    "radio_id",
    "data_hora",
    "lat",
    "long"
  )


detec_fixo_clean <-
  detec_fixo %>%
  dplyr::select(-potencia) %>%
  left_join(base_fixa_clean, by = "base_id")



dados_consolidados <-
  detec_fixo_clean %>%
  rbind(detec_movel_clean) %>%
  dplyr::arrange(radio_id, data_hora) %>%
  dplyr::group_by(radio_id) %>%
  dplyr::mutate(
    diff_time = difftime(data_hora, dplyr::lag(data_hora, 1)),
    diff_pos = lat - dplyr::lag(lat, 1),
    quebra = dplyr::case_when(
      diff_time >= 6*60*60 ~ 1,
      diff_pos > 0 ~ 1,
      is.na(diff_time) ~ 1,
      TRUE ~ 0
    ),
    grupo = cumsum(quebra)
  ) %>%
  dplyr::group_by(radio_id, base_id, lat, long) %>%
  dplyr::summarise(
    data_hora_ini = min(data_hora),
    data_hora_fim = max(data_hora),
    n_deteccoes = n()
  ) %>%
  dplyr::ungroup() %>%
  rbind(marcacao_clean) %>%
  dplyr::arrange(radio_id, data_hora_ini)

  inserir_dados(dados_consolidados, 'telemetria', 'dados_consolidados')

  executar_sql("ALTER TABLE telemetria.dados_consolidados ADD COLUMN geom geometry(Point, 4326);")
  executar_sql("UPDATE telemetria.dados_consolidados SET geom = ST_SetSRID(ST_MakePoint(long::double precision, lat::double precision), 4326);")

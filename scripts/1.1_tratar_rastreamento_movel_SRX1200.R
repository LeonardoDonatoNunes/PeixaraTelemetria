source('scripts/utils_settings.R')

diretorio = 'dados/rastreamento_movel_SRX1200'
arquivos <- list.files(diretorio)

marcacao <- carregar_dados(schema = 'telemetria', table = 'marcacao')

df_bi <- read.csv('dados/filtro/transmissor.csv') %>%
  dplyr::select(-transmissor_id,
                transmissor_id = radio_id,
                bi = intervalo_radio)

marcacao <- marcacao %>%
  dplyr::select(transmissor_id, data_hora_soltura, data_hora_remocao) %>%
  dplyr::left_join(df_bi, by = "transmissor_id") %>%
  dplyr::mutate(
    transmissor_id = as.character(transmissor_id),
    bi = ifelse(is.na(bi), 3, bi)
  ) %>%
  dplyr::rename(radio_id = transmissor_id)

# Junta os arquivos do SRX1200
movel_bruto <- data.frame()
for (arquivo in arquivos) {

  # Definir nomes de colunas
  dados_ii <- read.table(
    file.path(diretorio, arquivo),
    sep = "",
    dec = ",",
    header = FALSE,
    fill = TRUE,
    quote = "",
    check.names = FALSE)

 dados_ii <-dados_ii %>%
   dplyr::mutate(
      canal = dplyr::case_when( # mapeia frequência para canal
        V6 == 149.78 ~ 780,
        V6 == 150.10 ~ 100,
        TRUE ~ NA_real_
      )) %>%
 dplyr::select(
   data_or = V2, hora = V3, id = V5, canal, potencia  = V11,
   lat = V12, long = V13) %>%
 dplyr::mutate(
      data = as.Date(data_or, format = "%m/%d/%y"),
      data_hora = as.POSIXct(paste(data, hora), format = "%Y-%m-%d %H:%M:%S"),
      radio_id = paste0(canal, id)
    ) %>%
    dplyr::select(data, data_hora, canal, id, potencia, lat, long, radio_id)

  movel_bruto <- dplyr::bind_rows(movel_bruto,dados_ii)
}


movel_filtrado_SRX1200 <-
  movel_bruto %>%
  dplyr::inner_join(marcacao, by = 'radio_id') %>%
  dplyr::filter((data_hora >= data_hora_soltura &
                   (data_hora <= data_hora_remocao | is.na(data_hora_remocao)))) %>%
  dplyr::arrange(radio_id, data_hora) %>%
  dplyr::group_by(data, radio_id) %>%
  dplyr::mutate(
    n = n(),
    time_lag = as.integer(difftime(data_hora, lag(data_hora), units = 'secs')),
    quebra = dplyr::case_when(
      time_lag %% bi != 0 ~ 1,
      time_lag > 5 * 60 ~ 1,
      is.na(time_lag) ~ 1,
      TRUE ~ 0
    ),
    grupo = cumsum(quebra)
  ) %>%
  dplyr::group_by(data, radio_id, grupo) %>%
  dplyr::mutate(n_obs = n()) %>%
  dplyr::filter(n_obs > 5) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(data, radio_id) %>%
  dplyr::filter(potencia == max(potencia)) %>%
  dplyr::summarise(
    data_hora = max(data_hora),
    lat = max(lat),
    long = max(long),
    .groups = "drop"
  ) %>%
  dplyr::arrange(radio_id, data_hora)


if (!dir.exists('dados/movel_filtrado')) {
  dir.create('dados/movel_filtrado')
}

write.csv(movel_filtrado_SRX1200,
          'dados/movel_filtrado/movel_filtrado_SRX1200.csv', row.names = FALSE)

## Inserir os dados no banco de dados
inserir_dados(movel_filtrado_SRX1200, schema = 'telemetria', table = 'deteccao_radio_movel_SRX1200')



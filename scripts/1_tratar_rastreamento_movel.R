source('scripts/utils_settings.R')

diretorio = 'dados/rastreamento_movel'
arquivos <- list.files(diretorio)
marcacao <- carregar_dados(schema = 'telemetria', table = 'marcacao')
df_bi <- read.csv('dados/filtro/transmissor.csv') %>% dplyr::select(-transmissor_id,transmissor_id = radio_id, bi = intervalo_radio)

marcacao <-
  marcacao %>%
    dplyr::select(transmissor_id, data_hora_soltura, data_hora_remocao) %>%
    dplyr::left_join(df_bi) %>%
    dplyr::mutate(
      transmissor_id = as.character(transmissor_id),
      bi = ifelse(is.na(bi), 3, bi)
      ) %>%
    dplyr::rename(radio_id=transmissor_id)

# Junta os arquivos do monitoramento movel e corrige a data pela data escrita no arquivo do arquivo
movel_bruto <- data.frame()
for (arquivo in arquivos) {

  data_arquivo = stringr::str_split(arquivo, '_')[[1]][2]
  data_arquivo = as.Date(data_arquivo, format = "%d%b%Y")
  dados_i = read.table(file.path(diretorio, arquivo), col.names = c('data_or', 'hora', 'canal', 'id', 'antena', 'potencia', 'lat', 'long'))

  dados_i =
    dados_i %>%
      dplyr::mutate(
        data = as.Date(data_or, format="%m/%d/%y"),
        canal = ifelse(canal == 50, 100, canal),
        canal = ifelse(canal == 150, 100, canal)
        ) %>%
    dplyr::filter(data_or != "01/05/80")

  dados_i =
    dados_i %>%
      dplyr::mutate(
        data_diff = data - min(dados_i$data),
        data = data_arquivo + data_diff,
        data_hora = as.POSIXct(paste(data, hora, sep = " ")),
        radio_id = paste0(canal, id)
        ) %>%
      dplyr::select(-data_diff, -data_or, data, -hora)

  movel_bruto = rbind(movel_bruto, dados_i)
}

# Filtra os dados brutos
movel_filtrado <-
  movel_bruto %>%
    dplyr::inner_join(marcacao, by = 'radio_id') %>%
    dplyr::filter((data_hora >= data_hora_soltura & (data_hora <= data_hora_remocao | is.na(data_hora_remocao)))) %>%
    dplyr::arrange(radio_id, data_hora) %>%
    dplyr::group_by(data, radio_id) %>%
    dplyr::mutate(
      n = n(),
      time_lag = as.integer(difftime(data_hora, lag(data_hora), units = 'secs')),
      quebra = dplyr::case_when (
        time_lag%%bi != 0 ~ 1,
        time_lag > 5*60 ~ 1,
        is.na(time_lag) ~ 1,
        TRUE ~ 0
        ),
      grupo = cumsum(quebra),
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
        long = max(long)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(radio_id, data_hora)


if (!dir.exists('dados/movel_filtrado')) {
  dir.create('dados/movel_filtrado')
}

write.csv(movel_filtrado, 'dados/movel_filtrado/movel_filtrado.csv', row.names = FALSE)


## Inserir os dados no banco de dados
inserir_dados(movel_filtrado, schema = 'telemetria', table = 'deteccao_radio_movel')


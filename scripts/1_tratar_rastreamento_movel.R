library(dplyr)
library(stringr)
source('scripts/funcoes.R')

diretorio = 'rastreamento_movel'
arquivos <- list.files(diretorio)
transmissores <- read.csv('dados/filtro/cmr.csv')

# Junta os arquivos do monitoramento movel e corrige a data pela data escrita no arquivo do arquivo
movel_bruto <- data.frame()
for (arquivo in arquivos) {

  data_arquivo = stringr::str_split(arquivo, '_')[[1]][2]
  data_arquivo = as.Date(data_arquivo, format = "%d%b%Y")
  dados_i = read.table(file.path(diretorio, arquivo), col.names = c('data_or', 'hora', 'canal', 'id', 'antena', 'potencia', 'lat', 'long'))

  dados_i =
    dados_i %>%
      dplyr::mutate(
        data = as.Date(data_or, format="%m/%d/%y")
        )

  dados_i =
    dados_i %>%
      dplyr::mutate(
        data_diff = data - min(dados_i$data),
        data = data_arquivo + data_diff,
        data_hora = as.POSIXct(paste(data, hora, sep = " ")),
        radio_id = paste0(canal, id)
        ) %>%
      select(-data_diff, -data_or, data, -hora)

  movel_bruto = rbind(movel_bruto, dados_i)
}

# Filtra os dados brutos
movel_filtrado =
  movel_bruto %>%
    dplyr::filter(radio_id %in% transmissores$transmissor_id) %>%
    dplyr::arrange(radio_id, data_hora) %>%
    dplyr::group_by(data, radio_id) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 5) %>%
    dplyr::group_by(data, radio_id) %>%
    dplyr::filter(potencia == max(potencia)) %>%
    dplyr::summarise(
      data_hora = max(data_hora),
      lat = max(lat),
      long = max(long)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(radio_id, data_hora)


if(!dir.exists('movel_filtrado')) {
  dir.create('movel_filtrado')
}

write.csv(movel_filtrado, 'movel_filtrado/movel_filtrado.csv', row.names = FALSE)


## Inserir os dados no banco de dados
conn = connect_db()

DBI::dbWriteTable(
  conn = conn,
  name = DBI::Id(schema='telemetria', table='deteccoes_radio_movel'),
  value = movel_filtrado,
  overwrite = TRUE,
  row.names = FALSE
)

DBI::dbDisconnect(conn)


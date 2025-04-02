source('scripts/utils_settings.R')

base_fixa = carregar_dados(schema = 'telemetria', table = 'base_fixa')
marcacao = carregar_dados(schema = 'telemetria', table = 'marcacao')
detec_movel = carregar_dados(schema = 'telemetria', table = 'deteccao_radio_movel')
detec_fixo = carregar_dados(schema = 'telemetria', table = 'deteccao_radio_fixo')
area_estudo <- raster::raster('dados/geograficos/distancias.tiff') # Raster com as distancias na area de estudo

crs_utm = CRS("+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")
crs_geo = CRS("+proj=longlat +datum=WGS84")

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
    antena_id,
    receptor_id,
    lat,
    long,
    data_hora = data_hora_soltura
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


dados_consolidados_total <-
  detec_fixo_clean %>%
  rbind(detec_movel_clean) %>%
  rbind(marcacao_clean) %>%
  dplyr::arrange(radio_id, data_hora)



dados_consolidados_total_sp <- sp::spTransform(as(st_as_sf(dados_consolidados_total, coords = c("long", "lat"), crs = crs_geo), 'Spatial'),  crs_utm)
valores_extraidos <- extract(area_estudo, dados_consolidados_total_sp)
dados_consolidados_total$distancia <- valores_extraidos/1000
inserir_dados(dados_consolidados_total, 'telemetria', 'dados_consolidados_total')



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
  dplyr::group_by(grupo, radio_id, base_id, lat, long) %>%
  dplyr::summarise(
    data_hora_ini = min(data_hora),
    data_hora_fim = max(data_hora),
    n_deteccoes = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-grupo) %>%
  rbind(marcacao_clean %>% dplyr::mutate(
    n_deteccoes = 1,
    data_hora_fim = data_hora
  ) %>%  dplyr::select(
    radio_id,
    base_id,
    lat,
    long,
    data_hora_ini=data_hora,
    data_hora_fim,
    n_deteccoes
  )) %>%
  dplyr::arrange(radio_id, data_hora_ini)

  dados_consolidados_sp <- sp::spTransform(as(st_as_sf(dados_consolidados, coords = c("long", "lat"), crs = crs_geo), 'Spatial'),  crs_utm)
  valores_extraidos <- extract(area_estudo, dados_consolidados_sp)
  dados_consolidados$distancia <- valores_extraidos/1000
  inserir_dados(dados_consolidados, 'telemetria', 'dados_consolidados')


  executar_sql("ALTER TABLE telemetria.dados_consolidados ADD COLUMN geom geometry(Point, 4326);")
  executar_sql("UPDATE telemetria.dados_consolidados SET geom = ST_SetSRID(ST_MakePoint(long::double precision, lat::double precision), 4326);")

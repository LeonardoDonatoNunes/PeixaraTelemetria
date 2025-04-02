source('scripts/utils_settings.R')

dados <- carregar_geom('select *, row_number() over (partition by radio_id order by data_hora_ini) as ordem_registro from telemetria.dados_consolidados')

crs_utm = CRS("+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")
crs_geo = CRS("+proj=longlat +datum=WGS84")

area_estudo_dist <- raster::raster('dados/geograficos/distancias.tiff')
area_estudo = raster::projectRaster(area_estudo_dist, crs = crs_utm)
area_estudo <- gdistance::transition(area_estudo, mean, directions = 8)

criar_caminho <- function(radio_id, coord_inicial, coord_final, data_hora_ini, data_hora_fim, dist_ini, dist_fim, dist_m=1000, DEBUG=FALSE) {

  if ( any(is.na(coord_inicial), is.na(coord_final)) ) {
    return(1)
  }

  if ( all(coord_inicial == coord_final) ) {
    return(1)
  }

  if ( data_hora_ini == data_hora_fim ) {
    return(2)
  }

  if (DEBUG) {
    browser()
  }

  origem <- sp::spTransform(sp::SpatialPoints(matrix(coord_inicial, ncol = 2), , proj4string = crs_geo), crs_utm)
  destino <- sp::spTransform(sp::SpatialPoints(matrix(coord_final, ncol = 2), , proj4string = crs_geo), crs_utm)
  caminho <- gdistance::shortestPath(area_estudo, origem, destino, output = "SpatialLines")
  crs(caminho) <- crs_utm
  distancia <- SpatialLinesLengths(caminho)

  if ( distancia <= 1000 ) {
    return(3)
  }

  n_pontos = floor(distancia/dist_m)

  caminho_sf_utm <- sf::st_transform(sf::st_as_sf(caminho), crs_utm)
  pontos_sf_utm <- sf::st_line_sample(caminho_sf_utm, sample = seq(0, 1, length.out = floor(distancia/1000)))

  caminho_sf_geo <- sf::st_transform(caminho_sf_utm,crs_geo)
  pontos_sf_geo <- sf::st_cast(sf::st_transform(pontos_sf_utm,crs_geo), "POINT")
  pontos_sf_geo <- sf::st_as_sf(data.frame(geometry = pontos_sf_geo), crs = crs_geo)

  df_pontos_sp <- sp::spTransform(as(st_as_sf(pontos_sf_geo, coords = c("long", "lat"), crs = crs_geo), 'Spatial'),  crs_utm)
  valores_extraidos <- extract(area_estudo_dist, df_pontos_sp)

  tempo = as.integer(difftime(data_hora_fim, data_hora_ini, units = 'secs'))
  intervalo = (tempo/n_pontos) - 1

  df_pontos <-
    pontos_sf_geo %>%
    dplyr::mutate(
      radio_id = radio_id,
      dist = valores_extraidos/1000
    ) %>%
    dplyr::filter(!is.na(dist))

  if ( dist_ini < dist_fim ) {
    df_pontos <- df_pontos %>% dplyr::arrange(dist)
  } else {
    df_pontos <- df_pontos %>% dplyr::arrange(dplyr::desc(dist))
  }

  df_pontos <-
    df_pontos %>%
    dplyr::mutate(
      ordem_ponto = 1:n(),
      data_hora = data_hora_ini + (intervalo*ordem_ponto)
    )%>%
    dplyr::mutate(
      lat = sf::st_coordinates(.)[, 2],
      long = sf::st_coordinates(.)[, 1]
    ) %>%
    as.data.frame()

  return(df_pontos)

}

dados_ <- dados %>%
  dplyr::group_by(radio_id) %>%
  dplyr::mutate(
    lat_dest = lead(lat),
    long_dest = lead(long),
    data_hora_dest = lead(data_hora_ini),
    distancia_dest = lead(distancia)
  ) %>%
  dplyr::ungroup()


df_pontos <- data.frame()
for (i in 1:nrow(dados_)) {

  df_i <- criar_caminho(
    radio_id = dados_[i,]$radio_id,
    coord_inicial = c(dados_[i,]$long, dados_[i,]$lat),
    coord_final = c(dados_[i,]$long_dest, dados_[i,]$lat_dest),
    data_hora_ini = dados_[i,]$data_hora_fim,
    DEBUG = FALSE,#ifelse(dados_[i,]$radio_id == '10012', TRUE, FALSE),
    data_hora_fim = dados_[i,]$data_hora_dest,
    dist_ini = dados_[i,]$distancia,
    dist_fim = dados_[i,]$distancia_dest,
  )

  if ( is.numeric(df_i) ) {
    mensagem = dplyr::case_when(
      df_i == 1 ~ "Coordenadas iguais",
      df_i == 2 ~ "Datas iguais",
      df_i == 3 ~ "Distancias menores que a distancia min"
    )
    logger::log_info(paste0(stringr::str_pad(i, 3, 'left', pad = '0'), "/", nrow(dados), ". radio_id: ", dados_[i,]$radio_id, " ⚠️ ", mensagem))
    next
  }
  logger::log_info(paste0(stringr::str_pad(i, 3, 'left', pad = '0'), "/", nrow(dados), ". radio_id: ", dados_[i,]$radio_id, " ✔️"))

  df_pontos <- rbind(df_pontos, df_i)

}


df_pontos <-
  df_pontos %>%
  dplyr::mutate(base_id = 'EST') %>%
  dplyr::select(radio_id,base_id,data_hora,lat,long,distancia=dist)



dados_total <- get_sql('select radio_id, base_id, data_hora, lat, long, distancia from telemetria.dados_consolidados_total;')

dados_final <-
  dados_total %>%
  rbind(df_pontos) %>%
  dplyr::arrange(radio_id, data_hora)

inserir_dados(dados_final, schema = 'telemetria', table = 'movimentos', append = FALSE)


bases <- get_sql('select base_id, dist from telemetria.base_fixa bf;') %>% dplyr::mutate(base_id = stringr::str_squish(base_id))


library(ggimage)

peixes <- unique(dados_final$radio_id)


for (peixe in peixes) {

  dados_i <- dados_final %>%
    filter(radio_id == peixe)

  dt_min <- min(dados_i$data_hora)
  dt_max <- max(dados_i$data_hora)




  grafico <- ggplot(dados_i, aes(x = data_hora, y = distancia) ) +
    geom_line() +
    geom_hline(data = bases, aes(yintercept = dist), linetype = 2, alpha = 0.3, col='red') +
    geom_text(data = bases, aes(x = dt_max, y = dist, label = base_id), hjust = -1, color='red', alpha=0.5) +
    geom_point(data=dados_i %>% filter(!base_id %in% c('EST', 'MAR', 'MOV')), aes(x = data_hora, y = distancia), shape = 16, size = 2) +
    geom_point(data=dados_i %>% filter(base_id == 'MOV'), aes(x = data_hora, y = distancia), shape = 17, size = 2, col='blue') +
    geom_image(data=dados_i %>% filter(base_id == 'MAR'), aes(x = data_hora, y = distancia, image = 'img/piraiba.png'), size = 0.1) +
    coord_cartesian(clip = 'off') +
    scale_y_reverse() +
    theme_classic() +
    theme(
      plot.margin = margin(4,40,4,4),
      axis.line = element_blank()) +
    labs(
      x=NULL,
      y='Distância a partir do km 0 de rio (Km)',
      title = paste0('Movimentos do indivíduo: ', peixe)
    )

  ggsave(filename = paste0('img/graficos/', peixe, '.png'), grafico, width = 8, height = 4)

}






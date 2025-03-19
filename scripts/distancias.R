source('scripts/utils_settings.R')
library(sf)
library(raster)
library(gdistance)
library(dplyr)
library(sp)
#library(rgdal)
#install.packages("gdistance")
utm_crs <-
area = raster::raster('dados/geograficos/area_estudo.tif')
area = raster::projectRaster(area, crs = utm_crs)
origem <- SpatialPoints(matrix(c(-53.07728, -18.03163), ncol = 2), proj4string = CRS("+proj=longlat +datum=WGS84"))
origem <- spTransform(origem, crs(area))
area[is.na(area)] <- 0
area <- gdistance::transition(area, mean, directions = 8)
area <- geoCorrection(area, type = "c")
distancia <- accCost(area, origem)
crs(distancia) <- utm_crs
writeRaster(distancia, "dados/geograficos/distancias.tiff", format = "GTiff", overwrite = TRUE)


criar_caminho <- function(id = 1, coord_inicial, coord_final) {
  crs_inicial = CRS("+proj=longlat +datum=WGS84")
  crs_utm = CRS("+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")

  origem <- sp::spTransform(sp::SpatialPoints(matrix(coord_inicial, ncol = 2), , proj4string = crs_inicial), crs_utm)
  destino <- sp::spTransform(sp::SpatialPoints(matrix(coord_final, ncol = 2), , proj4string = crs_inicial), crs_utm)
  caminho <- gdistance::shortestPath(area, origem, destino, output = "SpatialLines")
  crs(caminho) <- crs_utm
  distancia <- SpatialLinesLengths(caminho)
  caminho_sf <- sf::st_transform(sf::st_as_sf(caminho), 4326)
  caminho_sf <-
    caminho_sf %>%
    dplyr::mutate(
      id = 2,
      distancia = distancia
      )

  return(caminho_sf)
}

# TODO Criar um loop para medir os caminhos entre todas as detecçõe
caminho <- criar_caminho(1, c(-52.628078, -16.517579), c(-50.8015695, -13.6908621))
inserir_dados(caminho, schema = 'geografia', table = 'caminho', append = TRUE, overwrite = TRUE)

# TODO Usar o raster com as distancias para identificar a posicao da deteccao na área de estudo

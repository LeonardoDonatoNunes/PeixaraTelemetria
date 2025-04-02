source('scripts/utils_settings.R')

crs_utm = CRS("+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")

area_estudo_or = raster::raster('dados/geograficos/area_estudo.tif')
area_estudo_utm = raster::projectRaster(area_estudo_or, crs = crs_utm)
origem <- SpatialPoints(matrix(c(-53.07728, -18.03163), ncol = 2), proj4string = CRS("+proj=longlat +datum=WGS84"))
origem <- spTransform(origem, crs(area_estudo_utm))
area_estudo_utm[is.na(area_estudo_utm)] <- 0
area_estudo_utm <- gdistance::transition(area_estudo_utm, mean, directions = 8)
area_estudo_utm <- geoCorrection(area_estudo_utm, type = "c")
distancia <- accCost(area_estudo_utm, origem)
crs(distancia) <- crs_utm
writeRaster(distancia, "dados/geograficos/distancias.tiff", format = "GTiff", overwrite = TRUE)

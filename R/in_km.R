#' Trasforma le coordinate da metri a km
#' 
#' @description 
#' Per dati spaziali con epsg=32632 trasforma lecoordinate da metri a kilometri
#' 
#' @param .x oggetto sf
#' 
#' @return oggetto sf con coordinate in kilometri
#' 
#' @export
#'  
in_km<-function(.x){ 
  sf::st_transform(.x,crs=sp::CRS("+proj=utm +zone=32 +datum=WGS84 +units=km +no_defs"))
}
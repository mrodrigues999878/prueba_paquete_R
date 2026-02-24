#' Buffer GIS tool
#'
#' \code{sig_buffer} devuelve una zona de influencia entorno a las geometrias de entrada
#'
#' @param capa_entrada Un objeto vectorial de la clase sf
#' @param distancia Distancia de vecindad para determinar la zona de influencia (m)
#' @param dissolve Parametro para controlar la fusion de geometrias
#' @param campo Parametro para fusionar por un atributo. Opcional
#'
#' @return Un objeto vectorial de la clase sfc_POLYGON
#'
#' @importFrom sf st_buffer st_union
#' @importFrom dplyr summarise group_by
#'
#' @examples
#' library(sf)
#' ciudades_data <- data.frame(
#'     nombre = c("Madrid", "Barcelona", "Sevilla"),
#'     poblacion = c(3.3, 1.6, 0.7),
#'     lon = c(-3.70, 2.17, -5.98),
#'     lat = c(40.41, 41.38, 37.38)
#'     )
#' ciudades_sf <- st_as_sf(ciudades_data, coords = c("lon", "lat"), crs = 4326)
#'
#' sig_buffer(ciudades_sf, distancia=200)
#' sig_buffer(ciudades_sf, distancia=200, dissolve = TRUE)
#' sig_buffer(ciudades_sf, distancia=200, dissolve = FALSE)
#'
#' @export
sig_buffer <- function(capa_entrada, distancia, dissolve=TRUE, campo=NA){

  if(dissolve==TRUE){
    buffer <- st_buffer(capa_entrada, dist = distancia) |>
      summarise(geometry = st_union(geometry))

    if(!is.na(campo)){
      buffer <- st_buffer(capa_entrada, dist = distancia) |>
        group_by({{campo}}) |>
        summarise(geometry = st_union(geometry))
    }

  }else{
    buffer <- st_buffer(capa_entrada, dist = distancia)
  }

  return(buffer)
}


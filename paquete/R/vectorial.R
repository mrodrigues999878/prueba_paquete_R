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
#' @examples 
#' \dontrun
#' sig_buffer(parcelas, 1000)
#' sig_buffer(parcelas, 1000, dissolve = T)
#' sig_buffer(parcelas, 1000, dissolve = T, 'Especie')
#' 

sig_buffer <- function(capa_entrada, distancia, dissolve=TRUE, campo){
  
  if(dissolve==TRUE){
    buffer <- st_buffer(capa_entrada, dist = distancia) |>
      summarise(geometry = st_union(geometry))
    
    if(exists(campo)){
      buffer <- st_buffer(capa_entrada, dist = distancia) |>
        group_by({{campo}}) |>
        summarise(geometry = st_union(geometry))
    }
    
  }else{
    buffer <- st_buffer(capa_entrada, dist = distancia)
  }
  
  return(buffer)
}


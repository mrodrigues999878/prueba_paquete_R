#' Normalized difference tool
#'
#' \code{ndiff} calcula el índice de diferencia normalizada
#'
#' @param x Un objeto con valores numéricos
#' @param y Un objeto con valores numéricos de la misma clase que x
#'
#' @return valores numéricos del índice
#'
#' @examples
#' ndiff(c(0.8,0.6),c(0.05,0.06))
#' ndiff(0.1,0.09)
#' @export
ndiff <- function(x,y){
  (x-y)/(x+y)
}




#' To plot roc curve
#'
#' @param score numeric score
#' @param y class
#' @param YIndex yue deng point
#'
#' @return plot
#' @export
#'
#' @examples plot_roc(score,y)
plot_roc <- function(score,y,YIndex = TRUE){
  suppressWarnings(library(ROCit))
  roc.p=rocit(score = score,class = y)
  plot(roc.p,legend = FALSE,grid=FALSE,YIndex = YIndex)  
}

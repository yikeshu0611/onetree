#' To scale every row seperately
#'
#' @param dataframe you want to scale
#'
#' @return a dataframe
#' @export
#'
#' @examples scale_row(dataframe)
scale_row <- function(dataframe){
  for (i in 1:ncol(dataframe)) {
    dataframe[i,]=scale(dataframe[i,])
  }
  return(dataframe)
}

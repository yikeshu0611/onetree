#' To scale every column seperately
#'
#' @param dataframe you want to scale
#'
#' @return a dataframe
#' @export
#'
#' @examples scale_col(dataframe)
scale_col <- function(dataframe){
  for (i in 1:ncol(dataframe)) {
    dataframe[,i]=scale(dataframe[,i])
  }
  return(dataframe)
}

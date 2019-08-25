#' Any column true is true in a dataframe
#'
#' @param dataframe
#'
#' @return character
#' @export
#'
#' @examples any_col(dataframe)
any_col <- function(dataframe){
  for (i in 1:ncol(dataframe)) {
    if (i==1){result=c()}
    result=c(result,any(dataframe[,i]))
  }
  return(result)
}

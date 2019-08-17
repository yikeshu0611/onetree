#' Any row true is true in a dataframe
#'
#' @param dataframe
#'
#' @return character
#' @export
#'
#' @examples any_row(dataframe)
any_row <- function(dataframe){
  for (i in 1:nrow(dataframe)) {
    if (i==1){result=c()}
    result=c(result,any(dataframe[i,]))
  }
  return(result)
}

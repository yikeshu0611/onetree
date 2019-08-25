#' all column true is true in a data
#'
#' @param data dataframe or matrix
#'
#' @return character
#' @export
#'
#' @examples any_col(data)
all_col <- function(data){
  if (any(is.data.frame(data),is.matrix(data))){
    for (i in 1:ncol(data)) {
      if (i==1){result=c()}
      result=c(result,all(data[,i]))
    }
  }else{
    result=data
  }
  
  return(result)
}

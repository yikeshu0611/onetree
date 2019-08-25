#' To caculate the power of some data
#'
#' @param data data
#' @param power power
#'
#' @return data
#' @export
#'
#' @examples Power(c(1,2,4),2)
Power <- function(data,power){
  if (any(c(is.data.frame(data),is.matrix(data)))){
    for (row.i in 1:nrow(data)) {
      for (column.i in 1:ncol(data)) {
        data[row.i,column.i]=`^`(data[row.i,column.i],power)
      }
    }
    return(data)
  }else if (any(c(is.character(data),is.numeric(data)))){
    for (char.i in 1:length(data)) {
      data[char.i]=`^`(data[char.i],power)
    }
    return(data)
  }
}

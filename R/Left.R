#' cut out a string from left
#'
#' @param x string
#' @param n length
#'
#' @return string
#' @export
#'
#' @examples Left("abcd",3)
Left <- function(x, n){
  if (any(is.data.frame(x),is.matrix(x))){
    for (i in 1:ncol(x)) {
      x[,i]=substr(x[,i], 1, n)
    }
    x
  }else{
    substr(x, 1, n)
  }
}

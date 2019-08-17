#' cut out a string from right
#'
#' @param x string
#' @param n length
#'
#' @return string
#' @export
#'
#' @examples Right("abcd",3)
Right <- function(x, n){
  if (any(is.data.frame(x),is.matrix(x))){
    for (i in 1:ncol(x)) {
      x.i=as.character(x[,i])
      x[,i]=substr(x.i, nchar(x.i)-n+1, nchar(x.i))
    }
    x
  }else if(is.factor(x)){
    x.i=as.character(x)
    substr(x.i, nchar(x.i)-n+1, nchar(x.i))
  }else{
    substr(x, nchar(x)-n+1, nchar(x))
  }
}


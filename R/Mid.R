#' cut out a string from start and length
#'
#' @param x string
#' @param start start
#' @param n length
#'
#' @return string
#' @export
#'
#' @examples Mid("abcd",2,2)
Mid <- function(x,start,n){
  if (n < 0){
    Right(Left(x,start),abs(n))
  }else if(n >0) {
    if (any(is.data.frame(x),is.matrix(x))){
      for (i in 1:ncol(x)) {
        x.i=as.character(x[,i])
        x[,i]=substr(x.i,start,start+n-1)
      }
      x
    }else{
      substr(x,start,start+n-1)
    }
  }
}


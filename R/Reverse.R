#' Reverse string to the opposite order
#'
#' @param data data
#'
#' @return string
#' @export
#'
#' @examples Reverse("abc123")
#' @examples Reverse(c("abc123","if"))
Reverse <- function(data){
  #creat a single function
  Reverse.i <- function(pattern,joint=""){
      for (i in 1:nchar(pattern)) {
        if (i==1){pattern.r=c()}
        pattern.r=c(Mid(pattern,i,1),pattern.r)
      }
      inner_Add_Symbol(pattern.r,joint)
  }
  if (any(is.data.frame(data),is.matrix(data))){
    for (i in 1:ncol(data)) {
      data.i=as.character(data[,i])
      data[,i]=unlist(lapply(data.i,Reverse.i))
    }
    data
  }else{
    unlist(lapply(data,Reverse.i))
  }
}

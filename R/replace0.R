#' replace
#' @description using gsub to be more power
#' @param data data
#' @param from frmo
#' @param to to
#'
#' @return data replaced
#' @export
#'
#' @examples replace(222,2)
replace0 <- function(data,from){
  replace1<-function(data,from,to){
    if (any(is.data.frame(data),is.matrix(data))){
      for (i in 1:ncol(data)) {
        data[,i]=gsub(from,to,data[,i])
      }
    }else{
      data=gsub(from,to,data)
    }
    data
  }
    for (i in 1:length(from)) {
      data=replace1(data,from[i],to="")
    }
  data
}
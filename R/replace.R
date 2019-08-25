#' replace
#' @description using gsub to be more power
#' @param data data
#' @param from frmo
#' @param to to
#'
#' @return data replaced
#' @export
#'
#' @examples replace(222,2,1)
replace <- function(data,from,to,pattern){
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
  if (all(!missing(from),!missing(to))){
    for (i in 1:length(from)) {
      data=replace1(data,from[i],to)
    }
  }
  if (!missing(pattern)){
    for (j in 1:length(pattern)) {
      from=gsub(":.*","",pattern[j])
      to=gsub(".*:","",pattern[j])
      data=replace1(data,from,to)
    }
  }
data
}
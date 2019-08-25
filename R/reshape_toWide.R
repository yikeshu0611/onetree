#' To reshape long to wide
#'
#' @param data data
#' @param value.var.prefix value variable prefix
#' @param id id
#' @param j j
#'
#' @return dataframe
#' @export
#'
#' @examples reshape_toWide(data,id,j,value.var.prefix)
reshape_toWide <- function(data,id,j,value.var.prefix){
  if (!is.data.frame(data)){
    data=as.data.frame(data)
  }
  id.2.names= colnames(data)[-unlist(c(id,j,value.var.prefix) %==% colnames(data))]
  m3=reshape(data = data, idvar = c(id,id.2.names),
          timevar = j, direction = "wide",sep = "")
  m4=is.na(m3)
  for (jh in 1:ncol(m4)) {
    if (jh==1){na.col=c()}
    if (all(m4[,jh])){
      na.col=c(na.col,jh)
    }
  }
  rownames(m3)=1:nrow(m3)
  if (!is.null(na.col)){
    m3[,-na.col]
  }else{
    m3
  }
}

#' To flat and strech data
#'
#' @param data dataframe
#' @param strech string of name
#'
#' @return a dataframe
#' @export
#'
#' @examples flat_strech(data)
#' @examples flat_strech(data,"strech")
flat_strech<-function(data,strech=NULL){
  if (is.null(strech)){
    data_unique=unique(data)
    data_unique_nrow=nrow(data_unique)
    data_nrow=nrow(data)
    freq=c()
    for (i in 1:data_unique_nrow){
      data_unique_line_i=data_unique[i,]
      rownames(data_unique_line_i)=NULL
      linshijishu=0
      for (j in 1:data_nrow){
        data_linej=data[j,]
        rownames(data_linej)=NULL
        if (identical(data_unique_line_i,data_linej)){
          linshijishu=linshijishu+1
        }
      }
      freq=rbind(freq,linshijishu)
    }
    rownames(freq)=NULL
    zuihoudeshuju<-cbind(data_unique,freq)
    return(zuihoudeshuju)
  }else{
    strech_ncol=match(strech,names(data))
    freq_dataf=data.frame(data[,strech_ncol])
    rongqi=c()
    for (i in 1:nrow(data)){
      timesrep=freq_dataf[i,]
      linshirongqi=data[rep(i,timesrep),]
      rongqi=rbind(rongqi,linshirongqi)
    }
    return(rongqi)
  }
}

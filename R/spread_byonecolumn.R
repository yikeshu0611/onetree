#' To spread by one column
#'
#' @param data data frame
#' @param bycolumn column index, number
#' @param joint joint between words in by column data
#'
#' @return data.frame
#' @export
#'
#' @examples spread_byonecolumn(data,bycolumn,",")
spread_byonecolumn <- function(data,bycolumn,joint){
  bycolumn=colnames(data)[bycolumn]
  for (i in 1:nrow(data)) {
    if (i==1){df=data.frame()}
    df.i1=data.frame(spread.word= unlist(strsplit(as.character(data[i,bycolumn]),
                                                  joint)))
    df.i2=cbind(data[i,-(bycolumn %==% colnames(data))],df.i1)
    df=rbind(df,df.i2)
  }
  colnames(df)[-("spread.word" %==% colnames(df))]=colnames(data)[-(bycolumn %==% colnames(data))]
  colnames(df)["spread.word" %==% colnames(df)]=colnames(data)[(bycolumn %==% colnames(data))]
  df[,colnames(data)]
}

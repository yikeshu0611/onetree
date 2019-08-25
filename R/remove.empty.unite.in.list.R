#' To delet empty unit in list
#'
#' @param TRFdata list with only 1 degree
#'
#' @return list without empty
#' @export
#'
#' @examples remove.empty.unite.in.list(list)
remove.empty.unite.in.list <-function(TRFdata){
  for (i in 1:length(TRFdata)) {
    if (i==1){deletn=c()}
    if (nchar(TRFdata[i])==0){
      deletn=c(deletn,i)
    }
  }
  if (length(deletn)>=1){
    TRFdata=TRFdata[-deletn]
  }
  TRFdata
}

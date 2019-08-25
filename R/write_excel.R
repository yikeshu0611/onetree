#' To write an excle
#'
#' @param dataframe dataframe
#' @param xlsename excle name
#' @importFrom openxlsx write.xlsx
#' @return an excel
#' @export
#'
#' @examples write_excel(dataframe)
#' @examples dataframe(dataframe,"data.xlsx")
write_excel <- function(dataframe,xlsename){
  if (missing(xlsename)){
    xlsename=gsub(":",";",paste0(Sys.time(),".xlsx"))
  }
  print(paste("Excel name:",xlsename))
  suppressMessages(write.xlsx(dataframe,file=xlsename))
}

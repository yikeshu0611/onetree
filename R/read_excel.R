#' To read excel by xlsx::read.xlsx
#'
#' @param filename excel name
#' @param sheetIndex 1 as default
#' @return an excel
#' @export
#'
#' @examples read_excel("1.xlsx")
read_excel <- function(filename,sheetIndex=1){
  as.data.frame(readxl::read_excel(path =filename,sheet=sheetIndex))
}

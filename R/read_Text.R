#' To read text in rstudio source to dataframe
#'
#' @param text
#'
#' @return dataframe
#' @export
#'
#' @examples read_text()
read_Text <- function(text){
  read.table(header = TRUE,text = text)
}


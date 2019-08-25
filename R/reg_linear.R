#' regress for linear regressiong using lm
#'
#' @param data data
#' @param y y
#' @param x x, missing means all the rest variables
#'
#' @return list
#' @export
#'
#' @examples reg_linear(data,"y","x")
reg_linear <- function(data,y,x){
  if (missing(x)){
    x=colnames(data)[y %!=% colnames(data)]
  }
  formu=paste0(y,"~",inner_Add_Symbol(x))
  lm(as.formula(formu),data=data)
}
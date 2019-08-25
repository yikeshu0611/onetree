#' regress for logistic regression using glm
#'
#' @param data data
#' @param y y
#' @param x x, missing means all the rest variables
#'
#' @return list
#' @export
#'
#' @examples reg_logistic(data,"y","x")
reg_logistic <- function(data,y,x){
  if (missing(x)){
    x=colnames(data)[y %!=% colnames(data)]
  }
  formula=as.formula(paste0(y,"~",inner_Add_Symbol(x)))
  glm(formula = formula,family = binomial(link = 'logit'),data = data)
}

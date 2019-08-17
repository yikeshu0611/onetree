#' regress for coxph from survival package
#'
#' @param data data
#' @param time time
#' @param ending ending or status
#' @param x x, missing means all the rest variables
#' @importFrom survival Surv coxph
#' @return list
#' @export
#'
#' @examples reg_coxph(data,"time","ending",c("x1","x2"))
reg_coxph <- function(data,time,ending,x){
  if (missing(x)){
    x=colnames(data)[c(time,ending) %!=% colnames(data)]
  }
  formu=paste0("Surv(",time,",",ending,") ~ ",inner_Add_Symbol(x))
  cat("formu=",formu,"\n")
  coxph(as.formula(formu),data=data)
}



#' To do UnivarReg of logistic for adjust vars
#'
#' @param data data
#' @param y y
#' @param adjust adjust vars
#'
#' @return dataframe
#' @export
#'
#' @examples UnivarReg_logistic_adjust(data,"y",c("adj1","adj2"))
UnivarReg_logistic_adjust <- function(data,y,adjust){
  for (i in 1:ncol(data)) {
    if (i==1){
      temporay=data.frame()
    }
    if (i %in% c(match(y,names(data)),match(adjust,names(data)))){
      next(i)
    }
    fumula=paste0(y,"~",inner_Add_Symbol(adjust),"+",names(data)[i])
    reg = summary(glm(fumula, data = data,family = binomial(link = "logit")))
    temporay_coef <- data.frame(t(as.matrix(round(reg$coefficients[nrow(reg$coefficients),],3))))
    rownames(temporay_coef)=rownames(reg$coefficients)[-(1:(length(adjust)+1))]
    temporay=rbind(temporay,temporay_coef)
  }
  temporay$star=ifelse(temporay[,4]<=0.05,"*","")
  temporay
}



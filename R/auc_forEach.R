#' To get auc for each variable
#'
#' @param data data
#' @param y classification
#'
#' @return dataframe
#' @export
#'
#' @examples auc_forEach(data,"y")
auc_forEach <- function(data,y){
  data1=data[,-grep(y,colnames(data))]
  data2=data.frame(y=data[,grep(y,colnames(data))],data1)
  colnames(data2)

  suppressWarnings(library(ROCit))
  for (i in 2:ncol(data2)){
    if (i==2){auc=c();cutoff.postive=c();cutoff.negetive=c()}
    roc.p=rocit(data2[,i],data2$y)
    auc.i=round(roc.p$AUC,3)
    cutoff.postive.i = min(roc.p$Cutoff[which.max(roc.p$TPR-roc.p$FPR)])
    cutoff.negetive.i = max(roc.p$Cutoff[which.min(roc.p$TPR-roc.p$FPR)])
    auc=c(auc,auc.i)
    cutoff.negetive=c(cutoff.negetive,cutoff.negetive.i)
    cutoff.postive=c(cutoff.postive,cutoff.postive.i)
  }
  auc.data=data.frame(var=names(data2)[2:ncol(data2)],auc,cutoff.postive,cutoff.negetive)
  auc.data=auc.data[order(auc.data$auc,decreasing = TRUE),]
  rownames(auc.data)=1:nrow(auc.data)
  auc.data
}


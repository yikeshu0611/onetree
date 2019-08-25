#' To unique and stack dataframe
#'
#' @param data data
#' @param unique number
#' @param stack number
#' @param joint joint of different words
#'
#' @return a dataframe
#' @export
#'
#' @examples unique_1_stack_n(data,1,2)
#' @examples unique_1_stack_n(data,1:2,3)
unique_1_stack_n <- function(data=dataframe,unique=1,stack=2,joint=","){
  unique1=data[,unique]
  stack1=data[,stack]
  if (length(unique)>1){
    for (j in 2:length(unique)) {
      if (j==2){ud=unique1[,1]}
      ud=paste0(ud,joint,unique1[,j])
    }
  }else{
    ud=unique1
  }
  dd=data.frame(name=ud,tel=stack1)
  dd2=dd[order(dd$name),]
  dd2$tel=as.character(dd2$tel)
  for (i in 2:nrow(dd2)) {
    if (i==2){
      dd3=data.frame()
      dd3=rbind(dd3,dd2[1,])
    }
    if (dd2$name[i]==dd3$name[nrow(dd3)]){
      dd3[nrow(dd3),"tel"]=
        paste0(dd3[nrow(dd3),"tel"],joint,dd2$tel[i])
    }else{
      dd3=rbind(dd3,dd2[i,])
    }
  }
  for (ij in 1:length(dd3$tel)) {
    if (ij==1){lengn=c()}
    locatin.n=gregexpr(joint,dd3$tel[ij])[[1]]
    if (length(locatin.n) ==1){
      if (locatin.n < 0){
        lengn=c(lengn,1)
      }else{
        lengn=c(lengn,2)
      }
    }else if (length(locatin.n) > 1){
      lengn=c(lengn,length(locatin.n)+1)
    }
  }
  dd4=cbind(dd3,lengn)
  if (length(unique)>1){
    dd4=cbind(colsplit(dd4[,1],",",colnames(unique1)),dd4[,2:3])
  }
  colnames(dd4)=c(colnames(unique1),colnames(data)[stack],"No_stack")
  return(dd4)
}

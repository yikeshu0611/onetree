#' Univariable logistci regression
#'
#' @param shuju data
#' @param y y
#'
#' @return dataframe
#' @export
#'
#' @examples UniverReg_logistcic(shuju,y)
UniverReg_logistcic <- function(shuju,y){
  j=match(y,names(shuju));y=shuju[,j]
  name=names(shuju);number_of_var=length(name)
  UniVarReg_file=c()
  for (i in 1:number_of_var){
    if (i==1){
      UniVarReg_file=data.frame()
    }
    if (i == j) next
    x=shuju[,i]
    if (     all(is.numeric(x))) {
      #do regression and get the coefficients
      reg=glm(y~x,family=binomial(link = 'logit'));reg=summary(reg)
      temporay_coef<-as.data.frame(round(reg$coefficients,3),row.names = FALSE)
      #creat left UniVar names
      UniVar=rbind("intercept",name[c(i)])
      #combine left UniVar names and coeficients together
      temporay_coef=cbind(UniVar,temporay_coef)
      temporay_coef=temporay_coef[-1,]
      #store data
      UniVarReg_file=rbind(UniVarReg_file,temporay_coef)
    }else if(all(is.factor(x)))  {
      #do regression and get the coefficients
      reg=glm(y~x,family=binomial(link = 'logit'));reg=summary(reg)
      temporay_coef<-as.data.frame(round(reg$coefficients,3),row.names = FALSE)
      #creat left UniVar names
      UniVar=c('intercept')
      UniVar_no=nrow(temporay_coef)
      for (k in 2:UniVar_no){
        UniVar_REPET=paste(name[c(i)],k,sep='')
        UniVar=rbind(UniVar,UniVar_REPET)
      }
      UniVar<-as.data.frame(UniVar,row.names=FALSE)
      colnames(UniVar)=NULL
      #combine left UniVar names and coeficients together
      temporay_coef=cbind(UniVar,temporay_coef)
      #store data
      UniVarReg_file=rbind(UniVarReg_file,temporay_coef)

    }else {cat("**********The following is UniVars which cannot be calculated**********\n",
               name[c(i)],"\t\ttype:",class(x),"\n",
               "**********The above is UniVar which cannot be calculated**********\n",
               "**********The above is UniVar which cannot be calculated**********\n")
    }
  }
  p.value=UniVarReg_file[,ncol(UniVarReg_file)]
  for (ij in 1:nrow(UniVarReg_file)) {
    if (ij == 1){ star=c()}
    if (p.value[ij] <= 0.001){
      star.ij= "0.001***"
    } else if (p.value[ij] <= 0.01){
      star.ij= "0.01**"
    } else if (p.value[ij] <= 0.05){
      star.ij= "0.05*"
    } else {
      star.ij= " "
    }
    star= c(star,star.ij)
  }
  finalResult=data.frame(UniVarReg_file,star)
  finalResult$Estimate=round(exp(finalResult$Estimate),3)
  colnames(finalResult)[2]="OR"
  return(finalResult)
}



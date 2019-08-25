#' To get summary table
#'
#' @param data data
#' @param group variable names
#'
#' @return dataframe
#' @export
#'
#' @examples summary_table(data,"sex")
summary_table <- function(data,group){
  #get function
  #sum.func1 no group object
  sum.func1 <- function(data){
    for (i in 1:ncol(data)) {
      i=1
      if (i==1){ sum=data.frame(); del=c()}
      if (is.numeric(data[,i])){
        sh.p=shapiro.test(data[,i])$p.value
        if (sh.p >=0.05){
          sum.i=data.frame(var=paste0(round(mean(data[,i]),2),'+',round(sd(data[,i]),2)))
        }else{
          sum.i=data.frame(var=paste0(round(median(data[,i]),2),
                                      "(",
                                      inner_Add_Symbol(round(quantile(data[,i], probs = c(0.25,0.75)),2),","),
                                      ")"))
        }
        rownames(sum.i)=colnames(data)[i]
        sum=rbind(sum,sum.i)
      }else if (is.factor(data[,i])){
        m1=table(data[,i])
        m1.prop=round(prop.table(table(data[,i])),4)*100
        for (j in 1:length(unique(data[,i]))) {
          sum.i=data.frame(var=paste0(m1[j],"(",m1.prop[j],'%)'))
          rownames(sum.i)=paste0(colnames(data)[i],j)
          sum=rbind(sum,sum.i)
        }
      }else{
        del=c(del,names(data)[i],"\n")
      }
    }
    if (!is.null(del)){
      cat("\n","varibles did not caculate:",del)
    }
    sum
  }
  #sum.func has group object
  sum.func <- function(data){
    for (i in 1:ncol(data)) {
      if (i==1){ sum=data.frame(); del=c()}

      if (any(colnames(data)[i]==group)){next(i)}

      if (is.numeric(data[,i])){
        sh.p=shapiro.test(data[,i])$p.value
        if (sh.p >=0.05){
          sum.i=data.frame(var=paste0(round(mean(data[,i]),2),'+',round(sd(data[,i]),2)))
        }else{
          sum.i=data.frame(var=paste0(round(median(data[,i]),2),
                                      "(",
                                      inner_Add_Symbol(round(quantile(data[,i], probs = c(0.25,0.75)),2),","),
                                      ")"))
        }
        rownames(sum.i)=colnames(data)[i]
        sum=rbind(sum,sum.i)
      }else if (is.factor(data[,i])){
        m1=table(data[,i])
        m1.prop=round(prop.table(table(data[,i])),4)*100
        for (j in 1:length(unique(data[,i]))) {
          sum.i=data.frame(var=paste0(m1[j],"(",m1.prop[j],'%)'))
          rownames(sum.i)=paste0(colnames(data)[i],j)
          sum=rbind(sum,sum.i)
        }
      }else{
        del=c(del,names(data)[i],"\n")
      }
    }
    if (!is.null(del)){
      cat("\n","varibles did not caculate:",del)
    }
    sum
  }
   # do
  if (missing(group)){
    sum.func1(data)
  }else{
    if (length(group)==1){
      group.paste0=group
    }else{
      for (g.i in 1:length(group)) {
        if (g.i==1){
          group.paste0=paste0(group[1],data[,group[1]])
        }else{
          group.paste0=paste0(group.paste0,"&",paste0(group[g.i],data[,group[g.i]]))
        }
      }
    }

    for (gi in 1:length(unique(group.paste0))){
      group.i=unique(group.paste0)[gi]
      data.gi=data[group.paste0==group.i,]
      sum.gi.i=sum.func(data.gi)
      colnames(sum.gi.i)=group.i
      if (gi==1){SUM=data.frame(row.names = rownames(sum.gi.i))}
      SUM=cbind(SUM,sum.gi.i)
    }
    SUM
  }

}

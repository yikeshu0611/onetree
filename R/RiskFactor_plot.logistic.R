#' To plot Risk Factor plot for logistic regression
#'
#' @param data the data you want to plot
#' @param y the purpose var
#' @param patient.names patient id
#' @param scatter.margin scatter.margin
#' @param heatmap.cellheight heatmap.cellheight
#' @param heatmap.cellwidth heatmap.cellwidth
#' @param cluster_cols cluster_cols
#' @param cluster_rows cluster_rows
#' @param heatmap.color heatmap.color
#' @param scatter.color scatter.color
#' @param scatter.xlab xlab
#' @param scatter.ylab ylab
#' @param scatter.legend.text text
#' @param scatter.location scatter.location
#' @param scatter.size scatter.size
#' @param heatmap.size heatmap.size
#'
#' @return four pictures
#' @export
#'
#'@examples RiskFactor_plot.logistic(
#'                          data=logisticData,
#'                          y="y2",
#'                          patient.names="name",
#'
#'                          scatter.margin = c(low=0,left=4,top=3,right=8),
#'                          scatter.xlab="patient",
#'                          scatter.ylab ="Risk Score",
#'                          scatter.color=c(code1="red",code0="black"),
#'                          scatter.legend.text=c(code1="Death",code0="Alive"),
#'                          scatter.location=c(cutoff.x=4,cutoff.y=0.5,
#'                                             legend.x=10,legend.y=0.8),
#'
#'                          scatter.size=c(axis=1.5,lab=1.5,
#'                                         points=1.5,legend=1.6,
#'                                         cutoff=1.9),
#'
#'                          heatmap.cellheight=20,
#'                          heatmap.cellwidth=10,
#'                          cluster_cols = FALSE,
#'                          cluster_rows = FALSE,
#'                          heatmap.color=c("green", "black", "red"),
#'                          heatmap.size=c(row=15,column=15,legend=10)
#'                         )
RiskFactor_plot.logistic <- function(data,
                                     y,
                                     patient.names=NULL,
                                     scatter.margin = c(low=0,left=4,top=3,right=8),
                                     scatter.xlab="patient",
                                     scatter.ylab ="Risk Score",
                                     scatter.color=c(code1="red",code0="black"),
                                     scatter.legend.text=c(code1="Death",code0="Alive"),
                                     scatter.location=c(cutoff.x=4,cutoff.y=0.5,,
                                                        legend.x=10,legend.y=0.8),
                                     scatter.size=c(axis=1.5,lab=1.5,,
                                                    points=1.5,legend=1.6,,
                                                    cutoff=1.9),
                                     heatmap.cellheight=20,
                                     heatmap.cellwidth=10,
                                     cluster_cols = FALSE,
                                     cluster_rows = FALSE,
                                     heatmap.color=c("green", "black", "red"),
                                     heatmap.size=c(row=15,column=15,legend=10)
){

  colnames(data)[grep(y,colnames(data))]="ys"
  if (!is.null(patient.names)){
    rownames(data)=data[,grep(patient.names,colnames(data))]
    data=data[,-grep(patient.names,colnames(data))]
  }
  dd2=data[order(data[,grep("ys",colnames(data))]),]
  ##suppressMessages(suppressWarnings(library(rms)))
  ##dd<<-datadist(dd2)
  ##options(datadist="dd")
  ##coxm <- lrm(ys~.,x=T,y=T,data=dd2)
  ##nom <- nomogram(coxm,lp=TRUE,fun = function(x) x)
  ##rm(dd,envir = .GlobalEnv)
  fitaic=logit_reg(ys~.,data=dd2)
  survi.x=dd2[,-grep("ys",colnames(dd2))]

  coef=data.frame(row.names = 1:nrow(survi.x))
  for (cofi in names(survi.x)) {
    coefi=survi.x[,cofi]*(fitaic$coefficients[cofi])
    coef=cbind(coef,coefi)
    names(coef)[ncol(coef)]=cofi
  }

#  for (i in 1:ncol(survi.x)) {
#    if (i==1){score=data.frame(row.names = row.names(data))}
#    m=nom[i]
#    m2=gsub(colnames(survi.x)[i],"x22222",m)
#    origindata=unlist(strsplit(gsub(" ","",sub("list\\(x22222 = c\\(","",sub("[) ,]{0,4}Xbeta.*","",m2))),","))
#    points.rms=as.numeric(as.character(remove.empty.unite.in.list(unlist(strsplit(gsub(")","",gsub(",.{0,10}=","",sub(".*points =",",",m2)))," ")))))
#    points.frame=data.frame(origindata,points.rms)
#    origin.frame.i=data.frame(origindata=survi.x[,i],od=1:length(survi.x[,i]))
#    md=merge(origin.frame.i,points.frame)
#    md2=md[order(md$od),]
#    score.i=md2[,"points.rms"]
#    score=cbind(score,score.i)
#  }
  dd3=cbind(dd2,py=rowSums(coef))
  dd3=dd3[order(dd3$py),]

  roc.p=ROCit::rocit(dd3$py,dd3$ys)
  names(roc.p)
  cutoff=roc.p$Cutoff[which.max(roc.p$TPR-roc.p$FPR)]
  xloc=(1:length(roc.p$Cutoff))[dd3$py==cutoff]
  dd4=scale_col(dd3[,-c(ncol(dd3),yl)])
  dd5=t(dd4)
  annotation_col = data.frame(
    Risk = ifelse(dd3$py>=cutoff,"high","low")
  )
  rownames(annotation_col) = rownames(dd3)
  ann_colors = list(
    Risk = c(low="#00dae0",high="#ff9289")
  )
  #heat map
  het<-pheatmap::pheatmap(mat = dd5,
                          fontsize_row = heatmap.size["row"],
                          fontsize_col = heatmap.size["column"],
                          fontsize = heatmap.size["legend"],
                          annotation_col = annotation_col,
                          annotation_names_col = FALSE,
                          annotation_colors = ann_colors,
                          cluster_cols = cluster_cols,
                          cluster_rows = cluster_rows,
                          cellheight=heatmap.cellheight,
                          cellwidth=heatmap.cellwidth,
                          color = colorRampPalette(heatmap.color)(50))
  #plot 2
  #library(gridBase)
  par(mfrow=c(2,1),mar = scatter.margin,xpd=TRUE)
  plot(y=dd3$py,x=1:nrow(dd3),
       col=ifelse(dd3$ys==1,scatter.color["code1"],
                  scatter.color["code0"]),
       cex.axis=scatter.size["axis"],
       cex.lab=scatter.size["lab"],
       cex=scatter.size["points"],
       xlab =scatter.xlab,ylab =scatter.ylab,pch = 19)

  legend(x = scatter.location["legend.x"],
         y = scatter.location["legend.y"],
         cex = scatter.size["legend"],
         inset = -0.5,
         legend = c(scatter.legend.text["code1"],
                    scatter.legend.text["code0"]),
         title = "Status",
         box.col = "transparent",bg="transparent",
         pch=c(19, 19),
         col=c(scatter.color["code1"], scatter.color["code0"]))
  par(xpd=FALSE)
  abline(h=cutoff,v=xloc,lty=2)
  text(x = scatter.location["cutoff.x"],
       y = scatter.location["cutoff.y"],
       cex = scatter.size["cutoff"],
       labels = paste("Cutoff: ",round(cutoff,2)))
  plot.new()
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$figure)
  print(het)
}

#' To plot Risk factor plot for survival plot
#'
#' @param data dataframe
#' @param time time
#' @param status status
#' @param patient.names patient.names
#' @param scatter.legend.text scatter.legend.text
#' @param scatter.ylab scatter.ylab
#' @param scatter.margin scatter.margin
#' @param scatter.color scatter.color
#' @param scatter.location scatter.location
#' @param scatter.size scatter.size
#' @param heatmap.cellheight heatmap.cellheight
#' @param heatmap.cellwidth heatmap.cellwidth
#' @param cluster_cols cluster_cols
#' @param cluster_rows cluster_rows
#' @param heatmap.color heatmap.color
#' @param heatmap.size heatmap.size
#'
#' @return pictures
#' @export
#'
#' @examples RiskFactor_plot.survival(
#'                        data=survivalData,
#'                        time="time",
#'                        status="status",
#'                        patient.names="name",
#'
#'                        scatter.legend.text=c(code1="Death",code0="Alive"),
#'                        scatter.ylab =c(ylab1="Risk Score",ylab2="Survival Time"),
#'                        scatter.margin = c(low=1,left=7.5,top=4,right=19),
#'                        scatter.color=c(code0="#00dae0",code1="#ff9289"),
#'                        scatter.location=c(cutoff.x=23,cutoff.y=95,
#'                                           legend1.x=35,legend1.y=100,
#'                                           legend2.x=35,legend2.y=40),
#'                        scatter.size=c(axis=1.5,lab=1.5,
#'                                       points=1.5,legend=1.6,
#'                                       cutoff=1.9),
#'
#'                        heatmap.cellheight=20,
#'                        heatmap.cellwidth=15,
#'                        cluster_cols = FALSE,
#'                        cluster_rows = FALSE,
#'                        heatmap.color=c("green", "black", "red"),
#'                        heatmap.size=c(row=15,column=15,legend=10)
#'                      )
RiskFactor_plot.survival <- function(data,
                                     time,
                                     status,
                                     patient.names=NULL,
                                     scatter.legend.text=c(code1="Death",code0="Alive"),
                                     scatter.ylab =c(ylab1="Risk Score",ylab2="Survival Time"),
                                     scatter.margin = c(low=1,left=7.5,top=4,right=19),
                                     scatter.color=c(code0="#00dae0",code1="#ff9289"),
                                     scatter.location=c(cutoff.x=23,cutoff.y=95,
                                                        legend1.x=35,legend1.y=100,
                                                        legend2.x=35,legend2.y=40),
                                     scatter.size=c(axis=1.5,lab=1.5,
                                                    points=1.5,legend=1.6,
                                                    cutoff=1.9),
                                     heatmap.cellheight=20,
                                     heatmap.cellwidth=15,
                                     cluster_cols = FALSE,
                                     cluster_rows = FALSE,
                                     heatmap.color=c("green", "black", "red"),
                                     heatmap.size=c(row=15,column=15,legend=10)
){
  {
    colnames(data)[grep(time,colnames(data))]="time"
    timel=grep("time",colnames(data))
    colnames(data)[grep(status,colnames(data))]="status"
    statusl=grep("status",colnames(data))
  if (!is.null(patient.names)){
    rownames(data)=data[,grep(patient.names,colnames(data))]
    data=data[,-grep(patient.names,colnames(data))]
  }
  }
  suppressMessages(suppressWarnings(library(rms)))
  dd<<-datadist(data)
  options(datadist="dd")
  coxm <- cph(Surv(time,status==1)~.,x=T,y=T,data=data,surv=T)
  nom <- nomogram(coxm)
  survi.x=data[,-c(timel,statusl)]
  for (i in 1:ncol(survi.x)) {
    if (i==1){score=data.frame(row.names = row.names(data))}
    m=nom[i]
    m2=gsub(colnames(survi.x)[i],"x22222",m)
    origindata=unlist(strsplit(gsub(" ","",sub("list\\(x22222 = c\\(","",sub("[) ,]{0,4}Xbeta.*","",m2))),","))
    points.rms=as.numeric(as.character(remove.empty.unite.in.list(unlist(strsplit(gsub(")","",gsub(",.{0,10}=","",sub(".*points =",",",m2)))," ")))))
    points.frame=data.frame(origindata,points.rms)
    origin.frame.i=data.frame(origindata=survi.x[,i],od=1:length(survi.x[,i]))
    md=merge(origin.frame.i,points.frame)
    md2=md[order(md$od),]
    score.i=md2[,"points.rms"]
    score=cbind(score,score.i)
    }
  data2=cbind(data,rms.score=rowSums(score))
  data3=data2[order(data2$rms.score),]
  roc.p=ROCit::rocit(score = data3$rms.score,
                     class = data3$status)
  names(roc.p)
  cutoff=roc.p$Cutoff[which.max(roc.p$TPR-roc.p$FPR)]
  xloc=min((1:length(roc.p$Cutoff))[data3$rms.score==cutoff])
  dd4=scale_col(data3[,-c(ncol(data3),timel,statusl)])
  dd5=t(dd4)
  annotation_col = data.frame(
    Risk = ifelse(data3$rms.score>=cutoff,"high","low")
  )
  rownames(annotation_col) = rownames(data3)
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
  #plot 3
  #library(gridBase)
  par(mfrow=c(3,1),mar = scatter.margin,xpd=TRUE)
  #scatter1
  plot(y=data3$rms.score,x=1:nrow(data3),
       col=ifelse(data3$rms.score >=cutoff,
                  scatter.color["code1"],
                  scatter.color["code0"]),
       cex.axis=scatter.size["axis"],
       cex.lab=scatter.size["lab"],
       cex=scatter.size["points"],
       xlab =NULL,ylab =scatter.ylab["ylab1"],pch = 19)
  legend(x = scatter.location["legend1.x"],
         y = scatter.location["legend1.y"],
         cex = scatter.size["legend"],
         inset = -0.5,title = "Risk",
         legend = c("high","low"),
         box.col = "transparent",bg="transparent",
         pch=c(19, 19),
         col=c(scatter.color["code1"], scatter.color["code0"]))
  par(xpd=FALSE)
  abline(h=cutoff,v=xloc,lty=2)
  text(x = scatter.location["cutoff.x"],
       y = scatter.location["cutoff.y"],
       cex = scatter.size["cutoff"],
       labels = paste("Cutoff: ",round(cutoff,2)))
  #scatter2
  plot(y=data3$time,x=1:nrow(data3),
       col=ifelse(data3$status==1,
                  scatter.color["code1"],
                  scatter.color["code0"]),
       cex.axis=scatter.size["axis"],
       cex.lab=scatter.size["lab"],
       cex=scatter.size["points"],
       xlab =NULL,ylab =scatter.ylab["ylab2"],pch = 19)
  par(xpd=TRUE)
  legend(x = scatter.location["legend2.x"],
         y = scatter.location["legend2.y"],
         cex = scatter.size["legend"],
         inset = -0.5,title = "Status",
         legend = c(scatter.legend.text["code1"],
                    scatter.legend.text["code0"]),
         box.col = "transparent",bg="transparent",
         pch=c(19, 19),
         col=c(scatter.color["code1"], scatter.color["code0"]))
  par(xpd=FALSE)
  abline(v=xloc,lty=2)
  plot.new()
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$figure)
  print(het)
}

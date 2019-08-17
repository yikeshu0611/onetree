#' To plot Risk Factor plot for linear regression
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
#' @param scatter.location scatter.location
#' @param scatter.size scatter.size
#' @param heatmap.size heatmap.size
#'
#' @return four pictures
#' @export
#'
#'@examples RiskFactor_plot.linear(
#'            data=linearData,
#'            y="y2",
#'            patient.names="name",
#'
#'            scatter.margin = c(low=0,left=4,top=3,right=8),
#'            scatter.color=c(Prediction="red",Reality="black"),
#'            scatter.location=c(legend.x=10,legend.y=20),
#'            scatter.size=c(axis=1.5,lab=1.5,
#'                           points=1.5,legend=1.6,
#'                           cutoff=1.9),
#'
#'            heatmap.cellheight=20,
#'            heatmap.cellwidth=10,
#'            cluster_cols = FALSE,
#'            cluster_rows = FALSE,
#'            heatmap.color=c("green", "black", "red"),
#'            heatmap.size=c(row=15,column=15,legend=10)
#'          )
RiskFactor_plot.linear <- function(data,
                                   y,
                                   patient.names="name",
                                   scatter.margin = c(low=0,left=4,top=3,right=8),
                                   scatter.color=c(Prediction="red",Reality="black"),
                                   scatter.location=c(legend.x=10,legend.y=20),
                                   scatter.size=c(axis=1.5,lab=1.5,
                                                  points=1.5,legend=1.6,
                                                  cutoff=1.9),
                                   heatmap.cellheight=20,
                                   heatmap.cellwidth=10,
                                   cluster_cols = FALSE,
                                   cluster_rows = FALSE,
                                   heatmap.color=c("green", "black", "red"),
                                   heatmap.size=c(row=15,column=15,legend=10)
){
  if (is.numeric(y)){
    colnames(data)[y]="ys"
    yl=y
  }else if (is.integer(y)){
    colnames(data)[y]="ys"
    yl=y
  }else{
    colnames(data)[grep(y,colnames(data))]="ys"
    yl=grep("ys",colnames(data))
  }
  if (!is.null(patient.names)){
    rownames(data)=data[,grep(patient.names,colnames(data))]
    data=data[,-grep(patient.names,colnames(data))]
  }
  dd2=data[order(data[,yl]),]
  fit=lm(ys~.,data = dd2)
  py=predict(fit,dd2[,-yl])
  dd3=cbind(dd2,py)
  dd3=dd3[order(dd3$py),]
  dd4=scale_col(dd2[,-yl])
  dd5=t(dd4)
  #heat map
  het<-pheatmap::pheatmap(mat = dd5,cluster_cols = cluster_cols,
                          cluster_rows = cluster_rows,
                          fontsize_row = heatmap.size["row"],
                          fontsize_col = heatmap.size["column"],
                          fontsize = heatmap.size["legend"],
                          cellheight=heatmap.cellheight,
                          cellwidth=heatmap.cellwidth,
                          color = colorRampPalette(heatmap.color)(50))
  #plot 2
  #library(gridBase)
  par(mfrow=c(2,1),mar = scatter.margin,xpd=TRUE)
  plot(y=dd3$ys,x=1:nrow(dd3),
       col=scatter.color["Reality"],
       cex.axis=scatter.size["axis"],
       cex.lab=scatter.size["lab"],
       cex=scatter.size["points"],
       xlab = "patient",ylab = "y",pch = 19)
  points(x = 1:nrow(dd3),y = dd3$py,
         cex=scatter.size["points"],pch=19,
         col=scatter.color["Prediction"])

  legend(x = scatter.location["legend.x"],
         y = scatter.location["legend.y"],
         cex = scatter.size["legend"],
         legend=c("Prediction","Reality"),
         box.col = "transparent",bg="transparent",
         pch=c(19, 19), col=c("red", "black"))

  plot.new()
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$figure)
  print(het)
}

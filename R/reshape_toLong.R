#' reshape wide to long
#'
#' @param data data
#' @param value.var.prefix value vars
#' @param id id
#' @param j j new colname
#' @importFrom reshape2 melt colsplit
#' @return data.frame
#' @export
#'
#' @examples reshape_toLong(data,id,j,value.var.prefix)
reshape_toLong <- function(data,id,j,value.var.prefix){
  if (any(duplicated(data[,id]))){stop(paste(id,"is not unique"))}
  var.c=c()
  for (var.i in value.var.prefix) {
    left.char=Left(colnames(data),nchar(var.i))
    var.c=c(var.c,var.i %==% left.char)
  }
  var.c=unique(var.c)
  subdata=data[,-var.c]
  reshapedata=data[,c(match(id,colnames(data)),var.c)]
  value.var.prefix=value.var.prefix[order(nchar(value.var.prefix),decreasing = TRUE)]

  for (var.i in value.var.prefix) {
    for (resh.d.var in 1:length(names(reshapedata))) {
      if (!grepl("_r_s_hape_",names(reshapedata)[resh.d.var])){
        names(reshapedata)[resh.d.var]=gsub(var.i,paste0(var.i,"_r_s_hape_"),
                                names(reshapedata)[resh.d.var])
      }
    }
  }
  #melt id value vars
  dfL <- melt(data = reshapedata, id.vars=id)
  dfl2=cbind(dfL, colsplit(dfL$variable, "_r_s_hape_", c("prefix", "appendix")))
  dfl3=dfl2[,-match("variable",colnames(dfl2))]
  data.merge=reshape_toWide(dfl3,id,"prefix","value")
  for (jj in 1:ncol(data.merge)) {
    if (Left(colnames(data.merge)[jj],5)=="value"){
      colnames(data.merge)[jj]=sub("value","",colnames(data.merge)[jj])
    }
  }
  if (is.data.frame(subdata)){
    data.merge=merge(x = subdata,y = data.merge,by=id,all=TRUE)
  }
  colnames(data.merge)[colnames(data.merge)=="appendix"]=j
  return(data.merge)
}

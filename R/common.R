#' Find Common
#'
#' @param data must be list
#'
#' @return vectors
#' @export
#'
#' @examples 
#' \dontrun{
#' data1=c('a','e','d')
#' data2=c('a','c','e')
#' data3=c('a','e','j')
#' data=list(data1,data2,data3)
#' common(data)
#' }
common <- function(data){
    if (!is.list(data)){
        ChiCheck=any(grepl("Chinese",sessionInfo()))
        if (ChiCheck) stop(tmcn::toUTF8('data\u5FC5\u987B\u662Flist'))
        if (ChiCheck) stop('data must be list')
    }
    for (i in 1:length(data)) {
        data=list(data1,data2,data3)
        for (i in 1:length(data)) {
            if (i==1){
                common = unlist(data[i])
            }else{
                common=unlist(data[i])[unique(unlist(common %==% unlist(data[i])))]
            }
        }
    }
}


data1=c('a','e','d')
data2=c('a','c','e')
data3=c('a','e','j')


common

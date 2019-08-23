#' similarly match by grep
#'
#' @param a a
#' @param b b
#'
#' @return vector
#' @export
#'
#' @examples 1 "%s=%" c(1,12,3)
"%s=%" <- function(a,b){
    loc=list()
    for (i in 1:length(a)) {
        loc=c(loc,list(grep(a[i],b)))
        names(loc)[i]=a[i]
    }
    loc
}



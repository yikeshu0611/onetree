#' sum for each column
#'
#' @param data dataframe or matrix
#'
#' @return vector
#' @export
#'
#' @examples sum_col(data)
sum_col <- function(data){
    if (any(is.data.frame(data),is.matrix(data))){
        for (i in 1:ncol(data)) {
            if (i==1){col.sum=c()}
            col.sum=c(col.sum,sum(data[,i],na.rm = TRUE))
        }
        col.sum
    }
}

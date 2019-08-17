#' sum for each row
#'
#' @param data dataframe or matrix
#'
#' @return vector
#' @export
#'
#' @examples sum_col(data)
sum_row <- function(data){
    if (any(is.data.frame(dataframe),is.matrix(dataframe))){
        for (i in 1:ncol(dataframe)) {
            if (i==1){col.row=c()}
            col.row=c(col.row,sum(dataframe[i,],na.rm = TRUE))
        }
        col.row
    }
}

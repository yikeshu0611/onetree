#' add something to strings to be equal length
#'
#' @param df string, vectors, dataframe or matrix
#' @param add appengdix
#' @param colname logistic value for dataframe or matrix
#' @param rowname logistic value for dataframe or matrix
#'
#' @return equal length results
#' @export
#'
#' @examples 
#' a=c(123,1,24,5,1.22554)
#' add_to_equal(a)
add_to_equal <- function(df,add=" ",colname=FALSE,rowname=FALSE){
    max_length=max(max(Nchar(df)),max(Nchar(colnames(df))))
    if (any(is.data.frame(df),is.matrix(df))){
        for (i in 1:nrow(df)) {
            for (j in 1:ncol(df)) {
                df[i,j]=paste0(df[i,j],
                               inner_Add_Symbol(rep(add,max_length-nchar(df[i,j])),""))
            }
        }
        if (colname){
            #CN is short for colnames
            CN=colnames(df)
            for (i in 1:length(CN)) {
                CN[i]=paste0(CN[i],
                             inner_Add_Symbol(rep(add,max_length-nchar(CN[i])),""))
            }
            colnames(df)=CN
        }
        if (rowname){
            #RN is short for rownames
            RN=rownames(df)
            max_length_RN=max(Nchar(RN))
            for (i in 1:length(RN)) {
                RN[i]=paste0(RN[i],
                             inner_Add_Symbol(rep(add,max_length_RN-nchar(RN[i])),""))
            }
            rownames(df)=RN
        }
    }else{
        for (i in 1:length(df)) {
            df[i]=paste0(df[i],
                         inner_Add_Symbol(rep(add,max_length-nchar(df[i])),""))
        }
    }
    df
}

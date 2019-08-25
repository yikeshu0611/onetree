#' Cat Dataframe and Matrix
#'
#' @param df dataframe or matrix
#' @param n now of row, default is 5
#'
#' @return dataframe or matrix
#' @export
#'
#' @examples
#' \donttest{
#'df = data.frame(a=c(1,2,3,450,4567,1),
#'                bc=c(1,2,4,5,2,5),
#'                c=c(1,2,4,5,2,5),
#'                d=c(1,2,4,5,2,5),
#'                e=c(1,2,4,5,2,5),
#'                f=c(1,2,4,5,2,5))
#'catdf(df,4)
#'catdf(df)
#'}
catdf <- function(df,n=5){
    #build functions
    {
        #build function 1 add_to_equal
        add_to_equal <- function(df,add=" ",colname=FALSE,rowname=FALSE){
            max_length=max(max(Nchar(df),na.rm = TRUE),max(Nchar(colnames(df)),na.rm = TRUE))
            if (any(is.data.frame(df),is.matrix(df))){
                for (i in 1:nrow(df)) {
                    for (j in 1:ncol(df)) {
                        df[i,j]=paste0(df[i,j],
                                       inner_Add_Symbol(rep(add,max_length-nchar(df[i,j],keepNA = FALSE)),""))
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
        #build function 2 Nchar
        Nchar <- function(x){
            if (any(is.data.frame(x),is.matrix(x))){
                for (i in 1:ncol(x)) {
                    x[,i]=nchar(x[,i],keepNA = FALSE)
                }
                return(x)
            }else{
                return(nchar(x))
            }
        }
        #build function 3 inner_Add_Symbol
        inner_Add_Symbol <- function(character,symbol="+"){
            if (length(character)>=2){
                for (character.i in 1:length(character)) {
                    if (character.i==1){
                        adj=character[1]
                    }else{
                        adj=paste0(adj,symbol,character[character.i])
                    }
                }
            }else{
                adj=character
            }
            adj
        }
    }
    #cat data frame
    df=add_to_equal(df = df,add = ' ',colname = TRUE,rowname = TRUE)
    if (n==1) stop('n must be more than 1')
    if (n > ncol(df)) stop('n must be less than the column number of df')
    ncoln=ncol(df) %/% n
    ncoln_left=ncol(df)-n*ncoln
    if (ncoln != 0){
        for (m in 1:ncoln) {
            df.m = df[,(n*m-(n-1)):(n*m)]
            cat(inner_Add_Symbol(rep(" ",nchar(rownames(df.m))[1]),""),
                colnames(df.m),'\n')
            for (i in 1:nrow(df.m)) {
                cat(rownames(df.m)[i],as.character(df.m[i,]),'\n')
            }
        }
    }
    if (ncoln_left != 0 ){
        if (ncoln_left==1){
            df.m= data.frame(df[,ncol(df)])
            colnames(df.m)=colnames(df)[ncol(df)]
            rownames(df.m)=rownames(df)
        }else{
            df.m = df[,(n*m+1):ncol(df)]
        }
        cat(inner_Add_Symbol(rep(" ",nchar(rownames(df.m))[1]),""),
            colnames(df.m),'\n')
        for (i in 1:nrow(df.m)) {
            cat(rownames(df.m)[i],as.character(df.m[i,]),'\n')
        }
    }
}

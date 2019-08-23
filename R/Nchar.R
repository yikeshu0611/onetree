Nchar <- function(x){
    if (any(is.data.frame(x),is.matrix(x))){
        for (i in 1:ncol(x)) {
            x[,i]=nchar(x[,i])
        }
        return(x)
    }else{
        return(nchar(x))
    }
}

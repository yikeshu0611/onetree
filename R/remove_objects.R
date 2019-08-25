#' remove all objects in Global Environment
#'
#' @return nothind
#' @export
#'
#' @examples remove_objects()
remove_objects <- function(){
    rm(list = ls(envir = .GlobalEnv),envir = .GlobalEnv)
}
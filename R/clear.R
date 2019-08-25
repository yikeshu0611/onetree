#' remove all object and detach all loaded packages
#'
#' @return nothing
#' @export
#'
#' @examples clear()
clear <- function(){
  remove_objects()
  detach()
}

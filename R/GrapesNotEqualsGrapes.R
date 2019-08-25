#' return locations which b does not equal a in b
#'
#' @param a object
#' @param b character
#'
#' @return vector
#' @export
#'
#' @examples "a" %!=% c("a","ab","a","b","c")
#' @examples c("a","b") %!=% c("a","ab","a","b","c")
"%!=%"<- function(a,b){
  if (length(a)==1){
    (1:length(b))[a != b]
  }else if(length(a) > 1){
    for (i in 1:length(a)) {
      if (i==1){location=c()}
      location.i=(1:length(b))[a[i] != b]
      location=c(location,location.i)
    }
    location[duplicated(location)]
  }
}

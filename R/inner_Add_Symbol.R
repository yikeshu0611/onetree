#' To inner join character by some symbol
#'
#' @param character character
#' @param symbol symbol
#'
#' @return character
#' @export
#'
#' @examples inner_Add_Symbol(c(1,2,4),"+")
#' @examples inner_Add_Symbol(c(1),"+")
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

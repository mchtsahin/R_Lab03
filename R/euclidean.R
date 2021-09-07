


#' Euclidean Algorithm
#'
#' The goal of this algorithm is to find the greatest common divisor of two input (a,b) numbers
#' @param a Must be numeric scalar or integer
#' @param b Must be numeric scalar or integer
#'
#' @return The greatest common divisor of \code{a} and \code{b}
#' @export
#'
#' @examples
#' euclidean(100,10)
#'
#' \dontrun{
#' euclidean('dog','cat')
#' }
#'
#'For more information on algorithm check \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#'
euclidean<-function(a,b){

  if (!is.numeric(a) | !is.numeric(b)) stop('Wrong input')

  while (b!=0){
    x=b
    b=a%%b
    a=x
  }
  return(a)
}

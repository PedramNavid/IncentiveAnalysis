#' Find level given minimum threshold
#'
#'\code{findLevel} is a wrapper for the \code{findInterval} function
#'  in base R. Given a vector of results x, and a set of thresholds
#'  \emph{t1..tn}, it returns a vector of levels with the same length as x,
#'  with levels \emph{1 to n+1}. The lowest level is 1, and any result acheiving
#'  >=t1 is given level 2, any result acheiving >=t2 is given level 3 and so on.
#'
#' @param x A vector of results to classify.
#' @param thresh A vector of minimum level thresholds, sorted increasingly.
#' @return vector of length \code{length(x)} with values \code{1:(N+1)}.
#' @examples x <- 1:100
#'  thresh <- c(10, 20, 50)
#'  findLevel(x, thresh)
#' @export
findLevel <- function(x, thresh) {
 return(findInterval(x, thresh) + 1)
}

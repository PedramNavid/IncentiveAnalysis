#' Annualize results with less than 12 months of observations.
#'
#'\code{annualizer} performs naive annualization of results with less than
#' 12 months worth of observations, essentialy multiplying each result by
#' \emph{12/nmonths} for results < 12 months.
#'
#' @param x A vector of results to annualize.
#' @param nmonths Either a single number that represents the number of months
#' used for observations, or a vector of length(x) that represents the number
#' of months each result was observed for.
#' @return vector of length \code{length(x)} with results annualized.
#' @examples x <- rep(10,20)
#'  n <- seq(1,20)
#'  annualize(x, n)
#' @export
annualizer <- function(x,nmonths) {
  if (length(nmonths) != 1 && length(nmonths) != length(x)) {
    stop("nmonths must be of length 1 or of length(x) to annualize.")
  }
  return(x * pmax(12/nmonths, 1))
}

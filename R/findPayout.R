#' Find payout given a result and a payout grid.
#'
#'\code{findPayout} determines the payout for each level based on a payout grid.
#' Essentially, for a vector of results against a vector of payouts \emph{p1..pn}
#' it returns \emph{p[l]} where l is the level, presumably found through
#' \code{findLevel}. Note that while \code{findLevel} potentially results in one
#' level greater than the number of thresholds, \code{findPayout} requires a
#' payout for every level in the vector x.
#'
#' @param x A vector of levels to translate to a payout.
#' @param p A vector of payout thresholds, sorted increasingly.
#' @return vector of length \code{length(x)}.
#' @examples x <- sample(rep(1:4, 20))
#'  p <- c(0, 500, 1000, 2000)
#'  findPayout(x, p)
#'\dontrun{
#'# Not enough payouts for the number of levels provided
#'x <- sample(rep(1:5, 20))
#'findPayout(x, p)
#'}
#' @export
findPayout <- function(x,p) {
  if(max(x) > length(p)) stop("More levels than there are payout grids.")
  if(min(x) < 1) stop("Lowest level must not be less than 1 for payouts.")
  return(p[x])
}

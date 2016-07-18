#' Create a threshold object
#'
#' @param x
#' @param ...
#'
#' @return
#' @export make_threshold
#'
#' @examples
make_threshold <- function(x, ...) {
  UseMethod("make_threshold", x)
}

make_threshold.default <- function(vec) {
  if (!identical(FALSE, is.unsorted(vec)))
    stop("'vec' must be sorted non-decreasingly and not contain NAs")
  z <- as.numeric(vec)
  names(z) <- seq(1:length(z))
  return(z)
}

make_threshold.rgrid <- function(rgrid, n = 5, min_q = 0.1, max_q = 0.9)
{
  qs <- seq(min_q, max_q, length.out = n)
  y = quantile(rgrid$pop$result, p = qs, names = F)
  z <- make_threshold(y)
  return(z)
}


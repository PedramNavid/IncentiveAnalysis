rmdl <- function(x, ...) UseMethod("rmdl")

#' Reward Modelling
#'
#' @param id unique identifier representing individuals being compensated
#' @param group optional grouping for individual employees, e.g. region, market
#' @param result the result of the compensable event
#' @param threshold a numeric sorted increasingly against which results are compared
#' @param payout_grid a numeric sorted of length: length(threshold) + 1 against which results are paid
#'
#' @return An object of class \code{"rmdl"}, which is a list with the following components:
#'   id, group, result, level, payout
#' @export rmdl rmdl.default mean.rmdl sum.rmdl
#'
#' @examples
rmdl.default <- function(id, group=NA, result, threshold = NA, payout_grid = NA,
  target_payout = NA) {

  if(any(is.na(result)))
      stop("rmdl results must not have NA values")

  if(is.na(threshold))
    threshold <- make_threshold(result)

  level = findLevel(result, threshold)

  if(is.na(payout_grid) && !is.na(target_payout))
    payout_grid = make_payout(threshold, level, target_payout)

  if(is.na(threshold) || is.na(payout_grid))
    stop("threshold and payout_grid must be defined.")

  payout = findPayout(level, payout_grid)

  z <- list(
    id = id,
    group = group,
    result = result,
    level = level,
    payout = payout,
    sample.n = length(id)

  )
  class(z) <- "rmdl"
  return(z)
}

# Methods
mean.rmdl <- function(rmdl, ...) {
  if(any(is.na(rmdl$group)))
    return(mean(rmdl$payout, ...))
  else
    {
      z <- aggregate(rmdl$payout, by = list(Group = rmdl$group), mean)
      return(setNames(z, c("group", "avg_payout")))
    }
}

sum.rmdl <- function(rmdl, na.rm = TRUE) {
  if(any(is.na(rmdl$group)))
    return(sum(rmdl$payout, na.rm = na.rm))
  else
  {
    z <- aggregate(rmdl$payout, by = list(Group = rmdl$group), sum)
    return(setNames(z, c("group", "total_payout")))
  }
}

summary.rmdl <- function(rmdl, pop.count = rmdl)
{
  # does nothing
}

# Private Methods
make_threshold <- function(x, n=5) {
  qs <- seq(0.1, 0.9, length.out = n)
  return(quantile(x, p = qs, names = F))
}

make_payout <- function(threshold, level, target_payout) {
  n = length(threshold) + 1
  m = Mode(level)
  step = target_payout / (m - 1)
  return(seq(0, by = step, length.out = n))
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

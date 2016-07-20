#' Title
#'
#' @param pop_x
#' @param pop_y
#'
#' @return
#' @export
#'
#' @examples
impact <- function(pop_x, pop_y) {
  x <- as.data.frame(pop_x)
  y <- as.data.frame(pop_y)
  combined <- inner_join(x, y, by = c("id", "group")) %>%
    dplyr::mutate(result.chg = result.y - result.x,
           level.chg = level.y - level.x,
           payment.chg = payment.y - payment.x,
           result.rank.x = rank(-result.x, ties.method="min"),
           result.rank.y = rank(-result.y, ties.method="min"),
           result.rank.chg = result.rank.y - result.rank.x,
           payment.rank.x = rank(-payment.x, ties.method="min"),
           payment.rank.y = rank(-payment.y, ties.method="min")) %>%
    dplyr::select(id, group, result.x, result.y, result.chg, result.rank.x, result.rank.y,
           result.rank.chg, level.x, level.y, level.chg, payment.x, payment.y, payment.chg
           )
  class(combined) <- c("impact", "data.frame")
  return(combined)
}

#' @export
sum.impact <- function(impact, measure="payment", ...) {
  with(impact,
  switch(measure,
         payment = sum(payment.chg, ...),
         result = sum(result.chg, ...))
  )
}

#' @export
head.impact <- function(impact, source="x", ...) {
  z <- switch(source,
         x = arrange(impact, result.rank.x),
         y = arrange(impact, result.rank.y))
  head(z, ...)
}


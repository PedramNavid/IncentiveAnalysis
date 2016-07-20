impact <- function(pop_x, pop_y) {
  x <- as.data.frame(pop_x)
  y <- as.data.frame(pop_y)
  combined <- inner_join(x, y, by = c("id", "group")) %>%
    mutate(result.chg = result.y - result.x,
           level.chg = level.y - level.x,
           payment.chg = payment.y - payment.x)

}

#' Create payout grid using target threshold
#'
#' @param threshold
#' @param target_payout
#'
#' @return
#' @export
#'
#' @examples
make_payout <- function(n, target_payout, first_payment=0) {
  step = target_payout / (n - 1)
  return(seq(first_payment, by = step, length.out = n))
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

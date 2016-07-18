
### CLASS DEFINITIONS ------
############################

#' Population Object
#'
#' @param id
#' @param group
#' @param result
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
pop <- function(id, group, result, scale=1)
{
  if(!is.numeric(scale) || length(scale) > 1)
      stop("scale must be a single number against which results will be divided")

  result = result / scale
  z <- list(
    id = id,
    group = group,
    result = result,
    scale = scale
  )

  ## Set the name for the class
  class(z) <- append(class(z),"pop")
  return(z)
}

#' @export
pop_tenure <- function(id, group, result, scale=1, tenure=NA, annualize=FALSE)
{
  if(annualize) {
    if(all(is.na(tenure))) stop("Can't annualize without tenure")
    result = annualize(result, tenure)
  }
  tpop <- pop(id, group, result, scale)
  tpop$tenure = tenure

  ## Set the name for the class
  class(tpop) <- append(class(tpop),"tenure_pop")
  return(tpop)
}

### METHOD DEFINITIONS ------
############################
#' @export
is.pop <- function(x)
  inherits(x, "pop")

#' @export
is.pop_tenure <- function(x)
  inherits(x, "pop_tenure")

## POPULATION COUNTS
#' @export
pop_n <- function(pop, ...) {
  UseMethod("pop_n", pop)
}

#' @export
pop_n.default <- function(pop, ...)
{
  stop("pop_n doesn't know how to deal with data of class ",
    paste(class(pop), collapse = "/"), call. = FALSE)
}

#' @export
pop_n.pop <- function(pop, group.rm = FALSE)
{
  if(all(is.na(pop$group)) || group.rm == TRUE)
    return(length(pop$id))
  z <- aggregate(pop$id, by = list(Group = pop$group), length)
  names(z) <- c("group", "n")
  return(z)
}

## RESULT VARIATION
#' @export
pop_var <- function(pop) {
  UseMethod("pop_var", pop)
}
#' @export
pop_var.default <- function(pop)
{
  stop("pop_var doesn't know how to deal with data of class ",
    paste(class(pop), collapse = "/"), call. = FALSE)
}
#' @export
pop_var.pop <- function(pop)
{
  cat("Result Scale: ", pop$scale)
  cat("\n\nStandard Deviation: ", sd(pop$result))
  cat("\nResult quantiles: \n")
  print(quantile(pop$result))
}

## PLOT RESULT
#' @export
plot.pop <- function(pop, bins=10, ...) {
  DF <- as.data.frame(x)
  g <- ggplot(data=DF, aes(x=result), ...)

  if(all(is.na(pop$group))) g + geom_histogram(bins = bins, ...)
    else g + geom_freqpoly(aes(colour = group), bins = bins, ...)

}

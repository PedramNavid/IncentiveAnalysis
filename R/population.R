
### CLASS DEFINITIONS ------
############################

pop <- function(id, group, result, scale=1)
{
  if(!is.numeric(scale) || length(scale) > 1)
      stop("scale must be a single number against which results will be divided")

  result = result / scale
  z <- list(
    id = id,
    group = group,
    result = result,
    scale = scale,
    df = data.frame(id, group, result)
  )

  ## Set the name for the class
  class(z) <- append(class(z),"pop")
  return(z)
}

tenure_pop <- function(id, group, result, scale=1, tenure=NA, annualize=FALSE)
{
  if(annualize) {
    if(all(is.na(tenure))) stop("Can't annualize without tenure")
    result = annualize(result, tenure)
  }
  tpop <- pop(id, group, result, scale)
  tpop$tenure = tenure
  tpop$df$tenure = tenure

  ## Set the name for the class
  class(tpop) <- append(class(tpop),"tenure_pop")
  return(tpop)
}

### METHOD DEFINITIONS ------
############################

is.pop <- function(x)
  inherits(x, "pop")

is.tenure_pop <- function(x)
  inherits(x, "tenure_pop")

## POPULATION COUNTS

pop_n <- function(pop) {
  UseMethod("pop_n", pop)
}

pop_n.default <- function(pop)
{
  stop("pop_n doesn't know how to deal with data of class ",
    paste(class(pop), collapse = "/"), call. = FALSE)
}

pop_n.pop <- function(pop_n)
{
  return(length(pop_n$id))
}

## RESULT VARIATION

pop_var <- function(pop) {
  UseMethod("pop_var", pop)
}

pop_var.default <- function(pop)
{
  stop("pop_var doesn't know how to deal with data of class ",
    paste(class(pop), collapse = "/"), call. = FALSE)
}

pop_var.pop <- function(pop)
{
  cat("Result Scale: ", pop$scale)
  cat("\n\nStandard Deviation: ", sd(pop$result))
  cat("\nResult quantiles: \n")
  print(quantile(pop$result))
}

## PLOT RESULT

plot.pop <- function(pop, bins=10, ...) {
  g <- ggplot(data=pop$df, aes(x=result), ...)

  if(all(is.na(pop$group))) g + geom_histogram(bins = bins, ...)
    else g + geom_freqpoly(aes(colour = group), bins = bins, ...)

}

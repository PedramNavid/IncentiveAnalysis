### CLASS DEFINITIONS ------
############################

#' Population Object
#'
#' @param id
#' @param group
#' @param result
#' @param scale
#' @param tenure
#' @param annualize
#'
#' @return
#' @export
#'
#' @examples
pop <- function(id, result, group=NULL, scale=1, tenure=NULL, annualize=FALSE,
                threshold=NULL, paygrid=NULL)
{
  if(!is.numeric(scale) || length(scale) > 1)
      stop("scale must be a single number against which results will be divided")

  if(annualize) {
    if(is.null(tenure)) stop("Can't annualize without tenure")
    result = annualize(result, tenure)
  }

  if(!is.null(threshold))
    lvl <- findLevel(result, threshold)
  else
    lvl <- NA

  if(!is.null(paygrid)){
    if(is.null(threshold))
      stop("Cannot use paygrid without thresholds")
    payment = findPayout(lvl, paygrid)
  }
  else
    payment <- NA

  result = result / scale

  z <- list(
    id = id,
    group = group,
    result = result,
    scale = scale,
    tenure = tenure,
    level = lvl,
    payment = payment,
    call = match.call()
  )

  class(z) <- "pop"
  return(z)
}

### METHOD DEFINITIONS ------
############################
#' @export
is.pop <- function(x)
  inherits(x, "pop")

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
  if(is.null(pop$group) || group.rm == TRUE)
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

#' @export
mean.pop <- function(pop, measure="result", ...)
{
  if(measure=="result")
    return(mean(pop$result, ...))
  if(measure=="payment")
    return(mean(pop$payment, ...))
  stop(gettextf("measure = '%s' is not supported.",
                measure))
}

#' @export
sum.pop <- function(pop, measure="result", ...)
{
  if(measure=="result")
    return(sum(pop$result, ...))
  if(measure=="payment")
    return(sum(pop$payment, ...))
  if(measure=="id")
    return(length(pop$id))
  stop(gettextf("measure = '%s' is not supported.",
                        measure))
}

#' @export
print.pop <- function(pop, measure="summary", ...) {
  if(measure=="summary") {
    cat(paste0("n=", sum(pop, measure="id")))
    cat("\n\nlevel dist: ")
    print.table(table(pop$level) / length(pop$level))
  }
  else if(measure=="level") {
    cat("\n\nlevel dist: ")
    print.table(table(pop$level) / length(pop$level))
  }
  else if(measure=="payment") {
    cat("\n\npayment dist: ")
    print.table(table(pop$payment) / length(pop$level))
    return()
  }
  else {
    stop(gettextf("measure = '%s' is not supported.",
              measure))
  }
}

#' @export
quantile.pop <- function(quantile, measure="result", ...)
{
  if(measure=="result")
    return(quantile(pop$result, ...))
  if(measure=="payment")
    return(quantile(pop$payment, ...))
}
## PLOT RESULT
#' @export
plot.pop <- function(x, bins=10, ...) {
  DF <- as.data.frame(x)
  g <- ggplot(data=DF, aes(x=result), ...)

  if(is.null(x$group)) g + geom_histogram(bins = bins, ...)
    else g + geom_freqpoly(aes(colour = group), bins = bins, ...)

}

## DATA FRAME
#' @export
as.data.frame.pop <- function(x) {
  z <- with(x,
       list(id=id,
            result = result,
            group = group,
            level = level,
            payment = payment))
  as.data.frame(z)
}

## GGPLOT Fortify
#' @export
fortify.pop <- function(model, data, ...)
  as.data.frame(model)

# Test Data
emp <- as.character(round(rnorm(100, 50000000, 100000)),0)
grp <- rep(c("east", "west", "north", "south"), length.out = 100)
result <- sort(abs(rnorm(100, 100000, 45000))) + seq(5, 60000, length.out = 100)
mypop <- pop(emp, result, grp, tenure = seq(1, 10, length.out = 100), annualize = TRUE)
popdf <- as.data.frame(mypop)
threshold = c(seq(50000, by=50000, length.out = 4))
poplvl <- pop(emp, result, grp, threshold = threshold)
poppmt <- pop(emp, result, grp, scale=1000, threshold = threshold, paygrid = c(0, 5000, 15000, 35000, 55000))
popdf2 <- as.data.frame(poppmt)
print(poppmt)
print(poppmt, "level")
print(poppmt, "payment")

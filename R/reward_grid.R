
### CLASS DEFINITIONS ------
############################

rgrid <- function(pop, lift=0, thresholds=NULL, payments=NULL) {

  pop$result = pop$result * ( 1 + lift)
  z = list(
    pop = pop,
    lift = lift,
    thresholds = thresholds,
    payments = payments
  )

  class(z) <- append(class(z),"rgrid")
  return(z)
}

### METHOD DEFINITIONS -----
############################



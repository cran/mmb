#' @title Creates a closure over a variable and returns its getter and setter.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param initVarVal the initial value of the closed variable.
#' @param valValidator an optional function to validate each value. Function
#' must return boolean. If this function returns FALSE for any value, an
#' error is thrown.
#' @keywords internal
#' @return list with entries 'get' and 'set' which are getter/setter for the
#' variable that a closure was made over.
make.varClosure <- function(initVarVal = NULL, valValidator = NULL) {
  varClosed <- initVarVal
  valValidator <- if (is.function(valValidator)) valValidator else function(x) TRUE

  temp <- list(
    get = function() varClosed,
    set = function(val) {
      if (!valValidator(val)) {
        stop(paste("Attempted to set invalid value:", val))
      }
      varClosed <<- val
    }
  )

  if (!missing(initVarVal)) {
    temp$set(initVarVal)
  }

  return(temp)
}

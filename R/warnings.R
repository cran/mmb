varWarn <- make.varClosure(TRUE)


#' @title Enable or disable warnings system-wide.
#'
#' @description Setter for enabling or disabling warnings. Warnings are
#' enabled by default.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param enable a boolean to indicate whether to enable warnings or not.
#' @return Boolean the state of enabled
#' @export
setWarnings <- function(enable = TRUE) {
  varWarn$set(!!enable)
  varWarn$get()
}

#' @title Get a boolean indicating whether warnings are enabled system-wide.
#'
#' @description Getter for the state of warnings. Returns true if enabled.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @return Boolean to indicate whether warnings are enabled or not.
#' @export
getWarnings <- function() varWarn$get()


varMsg <- make.varClosure(FALSE)

#' @title Enable or disable messages system-wide.
#'
#' @description Setter for enabling or disabling messages. Messages are
#' disabled by default. Use these to enable high verbosity.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param enable a boolean to indicate whether to enable messages or not.
#' @return Boolean the state of enabled
#' @export
setMessages <- function(enable = TRUE) {
  varMsg$set(!!enable)
  varMsg$get()
}

#' @title Get a boolean indicating whether messages are enabled system-wide.
#'
#' @description Getter for the state of messages. Returns true if enabled.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @return Boolean to indicate whether messages are enabled or not.
#' @export
getMessages <- function() varMsg$get()

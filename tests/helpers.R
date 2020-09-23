expect_does_throw <- function(expr, doExpect = TRUE) {
  doesThrow <- tryCatch({
    expr
    return(FALSE)
  }, error=function(cond) {
    if (interactive()) warning(paste("Caught:", cond))
    return(TRUE)
  })

  if (doExpect) {
    expect_true(doesThrow)
  }
  return(doesThrow)
}

expect_does_not_throw <- function(expr, doExpect = TRUE) {
  threw <- expect_does_throw(expr, doExpect = FALSE)

  if (doExpect) {
    expect_false(threw)
  }
  return(!threw)
}


install.mmb <- function() {
  if (base::Sys.getenv("IS_BUILD_COMMAND") != "TRUE") {
    return(0)
  }

  if (!("mmb" %in% rownames(installed.packages()))) {
    buildPath <- base::normalizePath(devtools::build(vignettes = FALSE), mustWork = TRUE)
    # Necessary or sometimes the package cannot be installed (in use)
    remove.mmb(detachOnly = TRUE)

    install.packages(buildPath, repos = NULL, type = "source")
    devtools::load_all()
  }
}

remove.mmb <- function(detachOnly = FALSE) {
  if (base::Sys.getenv("IS_BUILD_COMMAND") != "TRUE") {
    return(0)
  }

  if (!detachOnly) {
    tryCatch({
      remove.packages("mmb")
    }, error=function(cond) {})
  }

  tryCatch({
    detach("package:mmb", unload = TRUE, character.only = TRUE)
  }, error=function(cond) {})
}

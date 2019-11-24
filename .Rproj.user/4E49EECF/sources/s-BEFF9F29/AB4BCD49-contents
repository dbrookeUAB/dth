.simplifyWarning <- function(expr){
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  result <- withCallingHandlers(tryCatch(expr, error = function(e) e),
                                warning = w.handler)
  if(!is.null(W)){
    warning(W)
  }
  return(result)
}
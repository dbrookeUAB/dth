#' Filter Numeric
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
dth_numeric <- function(x){
  if(!data.table::is.data.table(x)){
    stop('Not a data.table.',call. = FALSE)
  }
  x[,.SD,.SDcols = colnames(x)[vapply(x, is.numeric, T)]]
}

#' Filter Integer
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
dth_integer <- function(x){
  if(!data.table::is.data.table(x)){
    stop('Not a data.table.',call. = FALSE)
  }
  x[,.SD,.SDcols = colnames(x)[vapply(x, is.integer, T)]]
}


#' Filter Double
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
dth_double <- function(x){
  if(!data.table::is.data.table(x)){
    stop('Not a data.table.',call. = FALSE)
  }
  x[,.SD,.SDcols = colnames(x)[vapply(x, is.double, T)]]
}



dth_character <- function(x){
  if(!data.table::is.data.table(x)){
    stop('Not a data.table.',call. = FALSE)
  }
  x[,.SD,.SDcols = colnames(x)[vapply(x, is.character, T)]]
}


dth_factor <- function(x){
  if(!data.table::is.data.table(x)){
    stop('Not a data.table.',call. = FALSE)
  }
  x[,.SD,.SDcols = colnames(x)[vapply(x, is.factor, T)]]
}
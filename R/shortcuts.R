#' Shortcuts for a lazy person
#'
#' @param x 
#' @param keep.rownames 
#' @param ... 
#'
#' @return
#' @export
#' @import data.table
#' @note This is to be used only for console work and not to be used in functions. 
#' @examples
adt <- function(x, keep.rownames=FALSE, ...){
  data.table::as.data.table(x, keep.rownames=FALSE, ...)
}

sk <- function(x, ..., verbose=getOption("datatable.verbose"), physical = TRUE){
  data.table::setkey(x, ..., verbose=getOption("datatable.verbose"), physical = TRUE)
}

sn <- function(x,old,new,skip_absent=FALSE){
  data.table::setnames(x,old,new,skip_absent=FALSE)
}
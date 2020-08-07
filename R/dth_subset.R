#' Subset list columns within data.tables
#'
#' @param column 
#' @param FUN 
#'
#' @return
#' @export
#'
#' @examples
dth_subset.list <- function(column, FUN){
  
  try(if(!is.list(column)) stop('column needs to be a list'))
  
  # captures function to be used for subsetting
  test <- call('which', substitute(FUN))
  
  # outputs the newly subset list
  res <- mapply('[', column, sapply(column, function(x) eval(test)))
  
  # removes character(0) from list
  res[lengths(res)==0] <- NA_character_
  
  # return vector or list?
  if(all(lengths(res)==1)){
    res <- sapply(res, '[',1)
  } else {
    message('returned list')
    return(list(res))
  }
  
}
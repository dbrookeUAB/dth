#' Combine a list of data.tables column-wise
#'
#' @param list 
#' @param key
#' @param col_names
#'
#' @return
#' @export
#'
#' @examples
cbindlist <- function(list, key, col_names = NULL){
  result <- list[[1]]
  lapply(list, function(x) data.table::setkeyv(x, key))
  for(i in list[-1]){
    result <- result[i]
  }
  
  if(!is.null(col_names)){
    
    if(length(col_names)!=length(colnames(result)[-1])){
      stop('Vector with new column names must be the same length as the list', call. = FALSE)
    }
    
    colnames(result)[-1] <- col_names
  }
  
  return(result)
}





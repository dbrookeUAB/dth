#' Convert a data.table to a matrix easily
#'
#' @param DT  data.table
#' @param rownames.col  character of which column to use for the matrix rownames. (Default=NULL)
#' @param set.rownames  logical whether to set the rownames from a column within the data.table. (Default=TRUE)
#'
#' @return
#' @export
#'
#' @examples
dth_as_matrix <- function(DT, rownames.col = NULL, set.rownames= TRUE){
  if(set.rownames){
    
    if(is.null(rownames.col)){
      rn <- DT[[1]]
      mat <- as.matrix(DT[,-1])
      rownames(mat) <- rn
    } else {
      rn <- DT[[rownames.col]]
      mat <- as.matrix(DT[,.SD,.SDcols = colnames(DT)[!colnames(DT)==rownames.col]])
      rownames(mat) <- rn
    }
  } else {
    mat <- as.matrix(DT)
  }
  
  if(!is.numeric(as.matrix(mat))){
    stop('Matrix is not numeric. Likely contains character vectors.')
  }
  
  return(mat)
  
}

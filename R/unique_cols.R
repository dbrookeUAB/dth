#' Remove columns with only one unique value from data.tables 
#'
#' @param df 
#' @param return.DT 
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' 
#' 
unique_cols <- function(df, return.DT = TRUE){
  dt <- data.table::as.data.table(data.table::copy(df))
  result <- sapply(colnames(dt), function(x) dt[,length(unique(get(x)))])
    if(return.DT){
    result <- dt[,.SD,.SDcols = names(result[result>1])]
    }
  
  return(result)
}
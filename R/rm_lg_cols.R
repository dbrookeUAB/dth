#' Remove columns with long character data
#'
#' @param DT 
#' @param nchars 
#'
#' @return
#' @export
#'
#' @examples
rm_lg_cols <- function(DT, nchars = 40){
  res <- DT[,.SD,.SDcols = as.character(data.table::melt(DT[,lapply(.SD, nchar)])[value<20, unique(variable)])]
  return(res)
}
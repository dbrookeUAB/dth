#' Remove columns with long character data
#'
#' @param DT 
#' @param nchars 
#'
#' @importFrom data.table .SD
#'
#' @return
#' @export
#'
#' @examples
rm_lg_cols <- function(DT, nchars = 40){
  res <- DT[,.SD,.SDcols = DT[,colnames(DT[,lapply(.SD, function(x) mean(nchar(x)))]<nchars)]]
  
  col_test <- DT[,sapply(.SD, function(x) mean(nchar(x)))]< nchars
  col_test <- col_test[!is.na(col_test)]
  col_test <- col_test[col_test]
  res <- DT[,.SD,.SDcols = names(col_test)]
                    
  return(res)
}
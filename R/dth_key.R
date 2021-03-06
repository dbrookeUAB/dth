#' Setkey for multiple data.tables in one function
#'
#' @param ... 
#'
#' @return
#' @export
#' @import data.table 
#'
#' @examples
#' # Make data.tables
#' require(data.table)
#' dt1 <- as.data.table(iris[,c(1,3,5)])
#' dt2 <- as.data.table(iris[,c(2,3,5)])
#' dt3 <- as.data.table(iris[,c(4,3,5)])
#' 
#' # they have not key set
#' sapply(list(dt1,dt2,dt3), key)
#' 
#' # First key set
#' dth_key(dt1, dt2, dt3, Species)
#' sapply(list(dt1,dt2,dt3), key)
#' 
#' # Key can change
#' dth_key(dt1, dt2, dt3, Petal.Length)
#' sapply(list(dt1,dt2,dt3), key)
#' 
#' # Key values can be a mix of quoted and non-quoted text
#' dth_key(dt1, dt2, dt3, Petal.Length, 'Species')
#' sapply(list(dt1,dt2,dt3), key)
#' 
#' # order doesn't matter
#' dt1 <- as.data.table(iris[,c(1,3,5)])
#' dt2 <- as.data.table(iris[,c(2,3,5)])
#' dt3 <- as.data.table(iris[,c(4,3,5)])
#' sapply(list(dt1,dt2,dt3), key)
#' dth_key(dt1,Petal.Length, dt3,'Species',dt2 )
#' sapply(list(dt1,dt2,dt3), key)
#' 
#' 
dth_key <- function(...){
  input_list <- as.character(substitute(list(...))[-1L])
  tmp <- which(sapply(input_list, exists, USE.NAMES = TRUE))
  cols <- input_list[-tmp]
  test <- sapply(tmp, function(x) data.table::is.data.table(...elt(x)))

# errors messages for including non-data.tables ---------------------------

  if(!all(test)){
    non_dts <- names(test)[!test]
    N <- length(non_dts)
    if(N>2){
      message <- paste(paste0(paste0(non_dts[-N], collapse = ", "),", and ", non_dts[N] ), 'are not data.tables')
    } else if(N==2) {
      message <- paste0(paste(non_dts, collapse = " and "), ' are not data.table')
    } else {
      message <- paste(non_dts, "is not a data.table")
    }
    
    stop(message, call. = FALSE)
  }
  
  dt_list <- names(test)[test]

# check if each data.table has those columns ------------------------------

  col_test <- sapply(dt_list, function(x) cols %in% colnames(get(x)))
  
  if(!all(col_test)){
    message <- paste0('data.tables missing columns: ',paste(names(col_test)[!col_test]), collapse = ", ")
    stop(message, call. = FALSE)
  }

# setkey ------------------------------------------------------------------
  for(i in dt_list){
    data.table::setkeyv(get(i), cols = cols) 
  }
}



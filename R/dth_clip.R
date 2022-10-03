#' grab data from the clipboard as a data.table
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(dth)
#' 
#' DT <- dth_read_clip()
#' }
#' 
dth_read_clip <- function(...){
  
  # which clipboard method to use
  switch(eval(.Platform$OS.type),
         unix = data.table::fread(cmd = 'pbpaste', ...),
         windows = data.table::fread(cmd = 'clipboard', ...)
  )
}


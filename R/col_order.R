#' Reorder data.table columns
#'
#' @param DT 
#' @param columns 
#'
#' @return
#' @importFrom data.table .SD %chin% copy
#' @export
#'
#' @examples
col_order <- function(DT, columns, preview = FALSE){
  
  if(!'data.table' %chin% class(DT)){
    stop(paste(deparse(substitute(DT)), 'is not a data.table.'))
  } 
  
  
  if(preview){
    head(switch(class(columns),
           numeric = DT[,.SD, .SDcols = colnames(DT)[columns]],
           character = DT[,.SD, .SDcols = columns],
           logical = DT[,.SD, .SDcols = columns]
    ), 3)
  } else {
    switch(class(columns),
           numeric = DT[,.SD, .SDcols = colnames(DT)[columns]],
           character = DT[,.SD, .SDcols = columns],
           logical = DT[,.SD, .SDcols = columns]
    )
  }
   

  
}

col_order2 <- function(DT, columns, preview = FALSE){
  
  if(preview){
    head(switch(class(columns),
                numeric = DT[,.SD, .SDcols = colnames(DT)[columns]],
                character = DT[,.SD, .SDcols = columns],
                logical = DT[,.SD, .SDcols = columns]
    ), 3)
  } else {
    switch(class(columns),
           numeric = DT[,.SD, .SDcols = colnames(DT)[columns]],
           character = DT[,.SD, .SDcols = columns],
           logical = DT[,.SD, .SDcols = columns]
    )
  }
  
  
  
}
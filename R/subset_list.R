#' Subset a `list` type column in a `data.table`
#'
#' @param column list column in data.table to subset
#' @param by a character or character vector for subsetting. Can also use function test (e.g. is.na)
#' @param type return rows that either have 'any' [default] or 'all' of the `by` values. type = 'none' returns rows with none of the by values. 'only' returns rows that only have the by values.
#'
#' @return
#' @export
#'
#' @examples
#' require(data.table)
#' 
#' DT <- data.table(
#'   x = 1:1e4, 
#'   y = sapply(sapply(1:1e4, function(x) sample(1:26,1)), function(x) sample(letters, x))
#' )
#' 
#' # return rows with any letters a, j, z
#' DT[subset_list(y, c('a','j','z'), type = 'any')]
#' 
#' # return rows with all letters a, j, z
#' DT[subset_list(y, c('a','j','z'), type = 'all')]
#' 
#' # return rows without the letters a, j, z
#' DT[subset_list(y, c('a','j','z'), type = 'not')]
#' 
#' # return rows with only the letter a
#' DT[subset_list(y, c('a'), type = 'only')]
#' 
#' # return rows with only the letters a and b
#' DT[subset_list(y, c('a', 'b'), type = 'only')]  # may return an empy data.table
#' 
#' # subset using a test function 
#' DT$y[c(7,29, 1989)] <- NA
#' DT[subset_list(y, is.na)] 
#' DT[subset_list(y, is.na, type = 'not)] 
#' 
#' 

subset_list <- function(column,
                        by,
                        type = 'any' # type = c('all','any','not', 'only')
                        ){
  if(is.function(by)){
    switch(type,
           all = sapply(column, function(x) all(eval(call(as.character(substitute(by)),x)))),
           any = sapply(column, function(x) any(eval(call(as.character(substitute(by)),x)))),
           not = sapply(column, function(x) !any(eval(call(as.character(substitute(by)),x)))),
           only = lengths(column)==length(by)&sapply(column, function(x) all(data.table:::`%chin%`(by,x)))
    ) 
    
  } else {
    switch(type,
           all = sapply(column, function(x) all(data.table:::`%chin%`(by,x))),
           any = sapply(column, function(x) any(data.table:::`%chin%`(by,x))),
           not = sapply(column, function(x) !any(data.table:::`%chin%`(by,x))),
           only = lengths(column)==length(by)&sapply(column, function(x) all(data.table:::`%chin%`(by,x)))
    )
  }
  
}



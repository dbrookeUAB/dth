#' Dcast using list columns
#'
#' @param data 
#' @param formula 
#' @param fun.aggregate 
#' @param sep 
#' @param ... 
#' @param margins 
#' @param subset 
#' @param fill 
#' @param drop 
#' @param value.var 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
dcast2 <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., 
                   margins = NULL, subset = NULL, fill = NULL, drop = TRUE, 
                   value.var = data.table:::guess(data), verbose = getOption("datatable.verbose")){
  
  by_var <- all.vars(formula[-3])  # a vector with the names of the left hand terms
  
  data.table::dcast(data,
        formula,
        value.var = value.var,
        fun.aggregate = list)[,lapply(.SD,unlist),by_var]
}
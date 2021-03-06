#' X-Y Correlation and Statistical Tests for multiple categories using data.table
#'
#' @param DT 
#' @param x 
#' @param y 
#' @param by 
#' @param method 
#' @param alternative 
#' @param exact 
#' @param conf.level 
#' @param continuity 
#' @param trim 
#'
#' @return
#' @export
#'
#' @examples
#' require(data.table)
#' DT <- as.data.table(iris)
#' # uses spearman method by default
#' dth_cor(DT, x = 'Sepal.Length', y = 'Sepal.Width', by = 'Species')
#' 
#' # set trim = F if you want all the extra information (mostly useless)
#' dth_cor(DT, x = 'Sepal.Length', y = 'Sepal.Width', by = 'Species',trim = FALSE)
#' 

dth_cor <- function(DT, x, y, by,method = 'spearman',
                    alternative = 'two.sided', exact = NULL,
                    conf.level = 0.95, continuity = FALSE, trim = TRUE) {
# cor.test ----------------------------------------------------------------
  # spearman parameter sometimes comes back NULL and throws off rbindlist for data.table
    DT_name <- paste0(substitute(DT))
    var_names <- paste(substitute(x), substitute(y))
    result <- .simplifyWarning(
      DT[,{
      res <-  stats::cor.test(
        x = get(x),
        y = get(y),
        method = method,
        alternative = alternative,
        exact = exact,
        conf.level = conf.level,
        continuity = continuity
      )
      class(res$parameter) <- 'integer'
      res[['data.name']] <- DT_name
      res[['variables']] <- var_names
      res
    }, by]
    )
  return(result)
}



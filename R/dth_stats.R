#' Anova formatted for use inside data.tables
#'
#' @param formula 
#' @param DT
#' @param by 
#'
#' @return
#' @export
#' @examples
dth_aov <- function(formula, DT, by){
  total <- length(unique(DT[[by]]))
  
  pb <- progress::progress_bar$new(
    format = "anova  :current/:total (:percent) [:bar] :elapsed eta: :eta",
    clear = FALSE, total =  total, width = 70)
  
  if(!data.table::is.data.table(DT)){
    DT <- data.table::as.data.table(DT)
  }
  result <- DT[,{
    pb$tick()
    tmp <- stats::anova(stats::aov(formula, .SD))
    tmp1 <- data.table::as.data.table(tmp[1,])
    tmp2 <-  data.table::as.data.table(tmp[2,1:3])
    tmp <-  data.table::as.data.table(cbind(tmp1,tmp2))[,c(4,5,1,6,2,7,3,8)]
    colnames(tmp) <- c("F value", "Pr(>F)", 'DF',
                       'DF Residuals','Sum Sq', "Sum Sq Residuals",
                       'Mean Sq', 'Mean Sq Residuals')
    tmp
  },by]
  
  class(result) <- c('dth_aov',class(result))
  result[,]
  return(result)
}

#' Another way to do an anova?
#'
#' @param formula 
#' @param DT 
#'
#' @return
#' @export
#'
#' @examples
dth_aov2 <- function(formula, DT){
  stats::anova(stats::aov(formula, data = DT))[1,-1]
}


#' Title
#'
#' @param formula 
#' @param DT 
#' @param by 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dth_wilcox <- function(formula, DT, by, ...){
  requireNamespace("data.table")
  .simplifyWarning(DT[,{
    res <- stats::wilcox.test(formula, data = .SD, ...)
    class(res$parameter) <- 'integer'
    res
  }, by])
}

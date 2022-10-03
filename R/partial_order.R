

.pos1 <- function(y,z, all){
  if(all==FALSE){
    p1 <- sapply(y, function(x) which(z==x))
  } else if(all==TRUE){
    p1 <- which(z %in% y)
  }

  tmp <- order(z)
  p2 <- tmp[which(!tmp %in% p1)]
  new_order <- c(p1,p2)
  result <- z[new_order]
  return(result)
}

.pos2 <- function(y,z, all){
  if(all==FALSE){
    p1 <- sapply(y, function(x) which(z==x))
  } else if(all==TRUE){
    p1 <- which(z %in% y)
  }

  tmp <- order(z)
  p2 <- tmp[which(!tmp %in% p1)]
  new_order <- c(p2,p1)
  result <- z[new_order]
  return(result)
}

.message_exclude <- function(x, pos){
  if(!pos %in% c('first','last')){
    stop('Where needs to be set to either \"\033[4mfirst\033[24m\" or \"\033[4mlast\033[24m\"', call. = FALSE)
  }

  if(length(x)>1){
    tmp <- paste0(c(paste0('\033[1m\033[33m',x[-length(x)],'\033[22m\033[39m', collapse = '\033[90m,\033[39m '), crayon::yellow(x[length(x)])), collapse = '\033[90m, &\033[39m ')
  } else {
    tmp <- paste0('\033[1m\033[33m',x,'\033[22m\033[39m')
  }

  if(pos=='first'){
    result <- paste('\033[90mOrdering\033[39m \033[31m\033[4mbeginning\033[24m\033[39m \033[90mwith\033[39m', tmp, collapse = ' ' )

  } else {
    result <- paste('\033[90mOrdering\033[39m \033[31m\033[4mending\033[24m\033[39m \033[90mwith\033[39m', tmp,collapse = ' ' )
  }
  cat(result,sep = '\n')
}

#' Partial Reordering
#'
#' @param x vector to be reorder
#' @param exclude  a vector of values to not be reordered. Default=NULL
#' @param where  where to place the excluded values. Default="first"
#' @param reorder_excluded whether to reorder the exclude vector. Default=FALSE
#' @param verbose console displays the values excluded and where. Default=FALSE
#'
#' @return
#' @export
#'
#' @examples
partial.order <- function(x, exclude = NULL, where = 'first', reorder_excluded = FALSE, verbose = FALSE){
  if(length(expr)==1 & is.null(exclude)){
    result[order(x)]
  } else {
    if(!all(exclude %in% x)){

      not_found <- exclude[!exclude %in% x]
      stop(paste('Values not found in',substitute(x),':',paste0(not_found, collapse = ', '), collapse = ' '), call. = FALSE)
    }
    if(verbose){
      .message_exclude(exclude, pos = where)
    }

    result <- switch(where,
                        first = .pos1(exclude, x, all = reorder_excluded),
                        last = .pos2(exclude, x, all = reorder_excluded)
                        )
  }

 return(result)

}


# Tests -------------------------------------------------------------------

# a <- c(22, 20, 89, 34, 45, 61, 71, 78, 1,56)
# partial.order(a,exclude = 22, where = 'last')  # works
# partial.order(a,exclude = c(78,45), where = 'first')  # 78 45 71 20 22 78 34 45 61 89
# partial.order(a, c(28,12, 1), where = 'last')  # fails
# .message_exclude(letters[1:3],'first')
# .message_exclude(letters[1:3],'last')
# .message_exclude(letters[1:3],'fart')


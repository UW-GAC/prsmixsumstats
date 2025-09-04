#' Make summary statistics
#' 
#' From input design matrix x and outcome vector y, compute x'x and x'y
#'
#' this version does not center x and y, anticipating centering later
#'
#' important: x and y must have the same number of subjects
#' and ordered the same way according to subject ID, but ID 
#' should not be a column of X
#' 
#' returns a sumstats object, which contains two list elements:
#' 1. matrix of x'x where x is nxp design matrix
#' 2. vector of x'y where y is nx1 trait vector
#' attributes of the sumstats object: nsubj, nmiss, nobs, colsum, ysum, yssq
#'
#' @param x design matrix with rows as individual observations
#' @param y vector with outcomes, length should match nrow(x)
#' @return sumstats object with elements xx and xy 
#' @export
make_sumstats <- function(x, y){
  stopifnot(nrow(x) == length(y))
  
  ## create sum stats xx and xy 
  xcol.names <-  colnames(x)
  names.miss <- xcol.names == "" | is.null(xcol.names)
  if(any(names.miss)){
    stop("col names of x are missing")
  }
  
  nsubj <- nrow(x)
  is.miss.x <- apply(is.na(x), 1, any)
  is.miss.y <-  is.na(y)
  is.miss <- is.miss.x | is.miss.y
  nmiss <- sum(is.miss)
  nobs <- nsubj - nmiss
  
  x <- x[!is.miss, , drop=FALSE]
  csum <- colSums(x)
  xx <- (t(x) %*% x)
  rownames(xx) <- xcol.names
  colnames(xx) <- xcol.names
  
  y <- y[!is.miss]
  ysum <- sum(y, na.rm=TRUE)
  xy <- t(x) %*% y
  #names(xy) <-  xcol.names
  
  yssq <- sum(y^2, na.rm=TRUE)
  
  ss <- new_sumstats(xx, xy, nsubj, nmiss, nobs, csum, ysum, yssq)
  
  return(ss)
}

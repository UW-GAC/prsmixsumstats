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
#' @param x design matrix with rows as individual observations
#' @param y vector with outcomes, length should match nrow(x)
#' @return list of x'x and x'y 
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
  names(xy) <-  xcol.names
  
  attr(xx, "nsubj") <- nsubj
  attr(xx, "nmiss") <- nmiss
  attr(xx, "nobs") <-  nobs
  attr(xx, "colsum") <- csum
  
  attr(xy, "ysum") <-  ysum
  attr(xy, "yssq") <-  sum(y^2, na.rm=TRUE)
  attr(xy, "nsubj") <- nsubj
  attr(xy, "nmiss") <- nmiss
  attr(xy, "nobs") <- nobs

  return(list(xx=xx, xy=xy))
}

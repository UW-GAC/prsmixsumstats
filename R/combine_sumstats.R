#' Combine summary statistics
#' @param sumstats list with elements xx and xy
#' @export
combine_sumstats <- function(sumstats){
  lapply(sumstats, validate_sumstats)

  ngroups <- length(sumstats)
  
  xx <- sumstats[[1]]$xx
  xy <- sumstats[[1]]$xy
  
  nsubj <- attr(sumstats[[1]], "nsubj")
  nmiss <- attr(sumstats[[1]], "nmiss")
  nobs <- attr(sumstats[[1]], "nobs")
  colsum <- attr(sumstats[[1]], "colsum")  
  ysum <- attr(sumstats[[1]], "ysum")
  yssq <- attr(sumstats[[1]], "yssq")
  
  for(index in 2:ngroups){
    xx <- xx + sumstats[[index]]$xx
    xy <- xy + sumstats[[index]]$xy
    nsubj <- nsubj + attr(sumstats[[index]], "nsubj")
    nmiss <- nmiss + attr(sumstats[[index]], "nmiss")
    nobs <- nobs + attr(sumstats[[index]], "nobs")
    colsum <- colsum + attr(sumstats[[index]], "colsum")  
    ysum <- ysum + attr(sumstats[[index]], "ysum")
    yssq <- yssq + attr(sumstats[[index]], "yssq")
  }
  
  ## center and scale xx
  xx <- xx - colsum %o% (colsum/nobs)
  ## scale xx matrix (x'x) so that diag = 1 and off-diag serve as correlations of cols of x's
  sdx <- sqrt(diag(xx)/nobs)
  xx <- xx / (sdx %o% sdx)
  ## divide by n because loss has 1/n
  xx <- xx / nobs
  
  ## center and scale xy
  yssq <- yssq - ysum^2/nobs
  yvar <- yssq / nobs
  sdy <- sqrt(yvar)
  xy <- xy - colsum * (ysum/nobs)
  xy <- xy/ (sdx * sdy)
  ## divide by n because loss has 1/n
  xy <- xy / nobs
  
  ## after standardize y
  yvar <- yvar/yvar
  
  ## to put beta's on original scale:
  ## if   y = x * b
  ## then y/sdy = x/sdx * a
  ## and b = a * (sdy/sdx)
  
  beta_multiplier <- sdy / sdx
  
  ss <- new_sumstats(xx, xy, nsubj, nmiss, nobs, colsum, ysum, yssq)
  validate_sumstats(ss)
  attr(ss, "yvar") <- yvar
  attr(ss, "beta_multiplier") <- beta_multiplier
  
  return(ss)
}


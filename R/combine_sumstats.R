#' Combine summary statistics
#'
#' Combines summary statistics, centers and scales
#'
#' Summary statistics in the input list are summed. Columns that are present
#' in one list element but not others are set to zero when computing the sum.
#' The results are centered and scaled and divdided by the number of observations.
#'
#' Use this function with the results of \code{\link{make_sumstats}}.
#'
#' @param sumstats list of sumstats objects
#' @return list of 1. sumstats object, 2. yvar (which should be 1),
#' 3. beta_multiplier, 4. list of columns with incomplete data (missing in at least
#' one list element)
#' @export
combine_sumstats <- function(sumstats){
  lapply(sumstats, validate_sumstats)
  matched_sumstats <- match_sumstats(sumstats)
  sumstats <- matched_sumstats$sumstats

  centered <- attr(sumstats[[1]], "centered")
  for (ss in sumstats) {
    if (attr(ss, "centered") != centered){
      stop("All sumstats must have the same value of 'centered'")
    }
  }

  ngroups <- length(sumstats)

  xx <- sumstats[[1]]$xx
  xy <- sumstats[[1]]$xy

  nsubj <- attr(sumstats[[1]], "nsubj")
  nmiss <- attr(sumstats[[1]], "nmiss")
  nobs <- attr(sumstats[[1]], "nobs")
  colsum <- attr(sumstats[[1]], "colsum")
  ysum <- attr(sumstats[[1]], "ysum")
  yssq <- attr(sumstats[[1]], "yssq")

  if (ngroups > 1) {
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
  }

  if (centered) {
    xx <- xx / nobs
    xy <- xy / nobs

    ## scale xx matrix (x'x) so that diag = 1 and off-diag serve as correlations of cols of x's
    sdx <- sqrt(diag(xx))
    xx <- xx / (sdx %o% sdx)

    ## center and scale xy
    yvar <- yssq / nobs
    sdy <- sqrt(yvar)
    xy <- xy/ (sdx * sdy)
  } else {
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
  }

  ## after standardize y
  yvar <- yvar/yvar

  ## to put beta's on original scale:
  ## if   y = x * b
  ## then y/sdy = x/sdx * a
  ## and b = a * (sdy/sdx)

  beta_multiplier <- sdy / sdx

  ss <- new_sumstats(xx, xy, nsubj, nmiss, nobs, colsum, ysum, yssq, centered=TRUE)
  validate_sumstats(ss)

  return(list(sumstats=ss, yvar=yvar, beta_multiplier=beta_multiplier,
              incomplete_cols=matched_sumstats$incomplete_cols))
}


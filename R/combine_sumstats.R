#' Combine summary statistics
#'
#' Combines summary statistics, centers and scales
#'
#' Summary statistics in the input list are summed. Columns that are present
#' in one list element but not others are set to zero when computing the sum.
#' The results are centered and scaled and divided by the number of observations.
#' If the sumstats objects provided are not already centered, centering is done
#' in this function.
#' 
#' The function checks for values of 0 on the diagonal of the X'X matrix after summing.
#' If any are found, those columns are removed from the sumstats object.
#'
#' Use this function with the results of \code{\link{make_sumstats}}.
#'
#' @param sumstats list of sumstats objects
#' @param scale boolean for whether to scale combined sumstats
#' @param no_drop columns that should not be dropped even if their variance is small (e.g. covariates in the model). if variance is zero, they will still be dropped.
#' @return list of 1. sumstats object, 2. yvar (which should be 1),
#' 3. beta_multiplier (Multiplier for converting standardized-scale
#'       coefficients back to the original predictor scale), 4. list of columns with incomplete data (missing in at least
#' one list element), 5. list of dropped columns with variance <= 1e-2 in X'X matrix (NULL if scale=FALSE)
#' @export
combine_sumstats <- function(sumstats, scale=TRUE, no_drop=character()){
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
  
  if (scale) {

  sdx <- sqrt(diag(xx)/nobs)
  
  ## eliminate variables with nearly 0 variance; small var creates very large
  ## beta_multiplier that might not be robust
  
  keep <- (sdx > 1e-2) | (colnames(xx) %in% no_drop & sdx > 0)
  near_zero_var <- colnames(xx)[!keep]
  if (length(near_zero_var) > 0) {
      xx  <- xx[keep, keep]
      xy  <- xy[keep,, drop=FALSE]
      colsum <- colsum[keep]
      sdx <- sdx[keep]
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
  } else {
      beta_multiplier <- 1
      yvar <- yssq / nobs
      near_zero_var <- NULL
  }

  ss <- new_sumstats(xx, xy, nsubj, nmiss, nobs, colsum, ysum, yssq, centered=TRUE)
  validate_sumstats(ss)

  return(list(sumstats=ss, yvar=yvar, beta_multiplier=beta_multiplier,
              incomplete_cols=matched_sumstats$incomplete_cols,
              near_zero_var=near_zero_var))
}




#' Compute weighted-average summary statistics across clusters
#'
#' Combines cluster-specific summary statistics by taking a weighted average of
#' per-cluster cross-product quantities. The resulting summary statistics are
#' centered and scaled so that the diagonal of `xx` is 1 and `xy` is expressed on
#' the standardized scale.
#'
#' Variables with near-zero marginal standard deviation are removed before
#' scaling, using `sdx > 1e-2`. This avoids numerical instability.
#'
#' @param sumstats_clusters A list of cluster-level summary-statistic objects.
#'   Each element must contain `xx` and `xy`, and must have attributes `nobs`,
#'   `nsubj`, `nmiss`, `colsum`, `ysum`, and `yssq`.
#' @param wt Numeric vector of cluster weights. Must have length equal to
#'   `length(sumstats_clusters)` and sum to 1.
#' @param no_drop columns that should not be dropped even if their variance is small (e.g. covariates in the model). if variance is zero, they will still be dropped.
#'
#' @return list of 1. sumstats object, 2. yvar (which should be 1),
#' 3. beta_multiplier (Multiplier for converting standardized-scale
#'       coefficients back to the original predictor scale), 4. list of columns with incomplete data (missing in at least
#' one list element), 5. list of dropped columns with variance <= 1e-2 in X'X matrix (NULL if scale=FALSE)
#'
#' @details
#' For each cluster, `xx`, `xy`, and `yssq` are first divided by that cluster's
#' observed sample size. These per-observation quantities are then combined using
#' `wt`. Count and sum attributes are accumulated across clusters without
#' weighting.
#'
#' The combined `xx` matrix is scaled to have unit diagonal:
#' \deqn{xx_{ij} = xx_{ij} / (sdx_i sdx_j)}
#'
#' The combined `xy` vector is scaled by predictor and outcome standard
#' deviations:
#' \deqn{xy_i = xy_i / (sdx_i sdy)}
#'
#' @examples
#' \dontrun{
#' sumstats_wt <- sumstats_weighted_ave(sumstats_clusters, wt)
#' }
#'
#' @export
sumstats_weighted_ave <- function(sumstats_clusters, wt, no_drop=character()){
    lapply(sumstats_clusters, validate_sumstats)
    matched_sumstats <- match_sumstats(sumstats_clusters)
    sumstats_clusters <- matched_sumstats$sumstats
    ncluster <- length(sumstats_clusters)
    
    ## use first cluster to setup template for 0's to later
    ## hold summations.
    xx <- sumstats_clusters[[1]]$xx * 0
    xy <- sumstats_clusters[[1]]$xy * 0
    nsubj <- 0
    nmiss <- 0
    nobs <- 0
    colsum <- 0
    ysum <- 0
    yssq <- 0
    yvar <- 0
    
    
    for(index in 1:ncluster){
        
        nobs_c <- attr(sumstats_clusters[[index]], "nobs")
        ## compute ave for each cluster
        xx_c <- sumstats_clusters[[index]]$xx/nobs_c
        xy_c <- sumstats_clusters[[index]]$xy/nobs_c
        yvar_c <- attr(sumstats_clusters[[index]], "yssq")/nobs_c
        
        ## combine by wt'd sum
        xx   <-   xx + wt[index]  * xx_c
        xy   <-   xy + wt[index]  * xy_c
        yvar <- yvar + wt[index]  * yvar_c
        
        ## totals
        nsubj <- nsubj + attr(sumstats_clusters[[index]], "nsubj")
        nmiss <- nmiss + attr(sumstats_clusters[[index]], "nmiss")
        nobs <-  nobs + attr(sumstats_clusters[[index]], "nobs")
        colsum <- colsum + attr(sumstats_clusters[[index]], "colsum")
        ysum <- ysum + attr(sumstats_clusters[[index]], "ysum")
        yssq <- yssq + attr(sumstats_clusters[[index]], "yssq")
        
    }
    
    
    sdx <- sqrt(diag(xx))
    
    ## eliminate variables with nearly 0 variance; small var creates very large
    ## beta_multiplier that might not be robust
    
    keep <- (sdx > 1e-2) | (colnames(xx) %in% no_drop & sdx > 0)
    near_zero_var <- colnames(xx)[!keep]
    if (length(near_zero_var) > 0) {
        xx  <- xx[keep, keep]
        xy  <- xy[keep,, drop=FALSE]
        colsum <- colsum[keep]
        sdx <- sdx[keep]
    }
    
    #xx_orig <- xx
    #xy_orig <- xy
    
    ## scale xx matrix (x'x) so that diag = 1 and off-diag serve as correlations of cols of x's
    
    xx <- xx / (sdx %o% sdx)
    
    sdy <- sqrt(yvar)
    xy  <- xy/ (sdx * sdy)
    
    sumstats_wt <- new_sumstats(xx, xy, nsubj, nmiss, nobs, colsum, ysum, yssq, centered=TRUE)
    validate_sumstats(sumstats_wt)
    #sumstats_wt$beta_multiplier <- sdy / sdx
    #sumstats_wt$yvar <- yvar
    #sumstats_wt$sdx <- sdx
    #sumstats_wt$sdy <- sdy
    #sumstats_wt$xx_orig <- xx_orig
    #sumstats_wt$xy_orig <- xy_orig
    
    beta_multiplier <- sdy / sdx
    return(list(sumstats=sumstats_wt, yvar=yvar, beta_multiplier=beta_multiplier,
                incomplete_cols=matched_sumstats$incomplete_cols,
                near_zero_var=near_zero_var))
    return(sumstats_wt)
}


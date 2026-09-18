#' Fit an Ensemble Polygenic Score from Summary Statistics
#'
#' Fits a regression model for an ensemble polygenic score (PGS) together with
#' adjustment covariates using summary-statistic cross-products. Predictors are
#' classified from the column names of `sumstats$xx`: columns whose names contain
#' `"PGS"` are treated as PGS predictors, and the remaining columns are treated
#' as covariates. Only predictors with `abs(beta) > 1e-6` are included.
#'
#' @param sumstats A list-like object containing at least `xx`, a numeric square
#'   predictor cross-product/correlation matrix, and `xy`, a numeric vector with
#'   one entry per column of `xx`. The function also uses
#'   `attr(sumstats, "nobs")` for the sample size and
#'   `attr(sumstats, "yssq")` for the outcome sum of squares. For a binary trait,
#'   `attr(sumstats, "ysum")` must contain the number of cases. Column names of
#'   `sumstats$xx` are used to identify PGS columns via the substring `"PGS"`.
#' @param beta Numeric vector with one entry per column of `sumstats$xx`.
#'   Non-negligible entries (`abs(beta) > 1e-6`) determine which predictors are
#'   included. For columns identified as PGS predictors, the corresponding
#'   values are also used as the fixed weights defining the ensemble PGS.
#' @param beta_multiplier to recover predictor scales
#' @param trait_type Character string specifying the trait type. The current
#'   implementation expects \code{"binary"}; quantities used later in the
#'   function are initialized only in the binary-trait branch.
#' @param tol Numeric tolerance passed to [qr.solve()] when solving the summary-
#'   statistic normal equations. It is also used in the symmetry check for
#'   `sumstats$xx`.
#'
#' @details
#' Let the selected PGS columns be indexed by \eqn{P} and selected covariates by
#' \eqn{C}. The selected PGS weights, `beta[P]`, are combined into one ensemble
#' score. Internally, the function constructs a summary-statistic design matrix
#' for the standardized ensemble PGS and the scaled covariates, solves for their
#' coefficients, and computes model \eqn{R^2} and a residual mean-squared-error
#' quantity from `attr(sumstats, "yssq")`.
#'
#' PGS predictors are identified automatically as columns for which
#' `grepl("PGS", colnames(sumstats$xx))` is `TRUE`; all other columns are treated
#' as covariates. Predictors whose supplied `beta` is effectively zero are
#' omitted. Consequently, `sumstats$xx` must have informative column names.
#'
#' For a binary trait with case fraction \eqn{p}, the function converts model
#' \eqn{R^2} values to approximate AUC values through a standardized case-control
#' mean-difference approximation. It also reports an approximate log odds ratio
#' for each fitted coefficient as \eqn{\alpha/[p(1-p)]}, with the corresponding
#' odds ratio obtained by exponentiation. AUC variances are calculated with
#' [hanley_mcneil()]. These calculations treat the supplied PGS weights as fixed.
#'
#' @return A list containing:
#' \describe{
#'   \item{`coefficients`}{Estimated coefficient for the ensemble PGS followed
#'     by coefficients for the selected covariates.}
#'   \item{`varcov`}{Estimated variance-covariance matrix of the fitted
#'     coefficients.}
#'   \item{`sigma2`}{Residual mean-squared-error quantity used in the covariance
#'     calculation.}
#'   \item{`df`}{Residual degrees of freedom, equal to the sample size minus the
#'     number of fitted coefficients.}
#'   \item{`R2_full_model`}{Model \eqn{R^2} for the ensemble PGS plus selected
#'     covariates.}
#'   \item{`R2_covar_model`}{Model \eqn{R^2} for the covariate-only component;
#'     `NA` when no covariates are fitted.}
#'   \item{`R2_partial`}{Partial \eqn{R^2}, computed as
#'     \eqn{(R^2_{full}-R^2_{covar})/(1-R^2_{covar})}.}
#'   \item{`log_or`}{Approximate log odds ratios derived from the fitted
#'     coefficients for a binary trait.}
#'   \item{`or`}{Approximate odds ratios, `exp(log_or)`.}
#'   \item{`auc`}{Named vector of approximate AUCs for the full model, the
#'     covariate-only model, and the ensemble-PGS-only model.}
#'   \item{`auc_var`}{Hanley-McNeil variance estimates corresponding to `auc`.}
#'   \item{`ncase`}{Number of cases from `attr(sumstats, "ysum")`.}
#'   \item{`ncont`}{Number of controls, `n - ncase`.}
#'   \item{`n`}{Total sample size from `attr(sumstats, "nobs")`.}
#' }
#'
#' @examples
#' # `sumstats` is expected to contain cross-products and scaling information.
#' # PGS columns are identified by names containing "PGS".
#' # fit <- pgs_ensemble_sumstats(sumstats, beta, trait_type = "binary")
#'
#' @export
pgs_ensemble_sumstats <- function(
    sumstats,
    beta,
    beta_multiplier = 1,
    trait_type = "binary",
    tol = 1e-10
) {
  
  
  is_pgs <- grepl("PGS", colnames(sumstats$xx)) 
  is_beta <- abs(beta) > 1e-6
  is_covar <- !is_pgs
  index_pgs <- (1:ncol(sumstats$xx))[is_pgs & is_beta]
  index_covar <- (1:ncol(sumstats$xx))[is_covar & is_beta]
  
  xx <- sumstats$xx
  xy <- sumstats$xy
  n <- attr(sumstats, "nobs")
  
  if (!is.matrix(xx) || !is.numeric(xx)) {
    stop("`sumstats$xx` must be a numeric matrix.", call. = FALSE)
  }
  
  if (nrow(xx) != ncol(xx)) {
    stop("`sumstats$xx` must be square.", call. = FALSE)
  }
  
  if (!is.numeric(xy) || length(xy) != ncol(xx)) {
    stop(
      "`sumstats$xy` must be numeric and have one entry per column of `xx`.",
      call. = FALSE
    )
  }
  
  if (!is.numeric(beta) || length(beta) != ncol(xx)) {
    stop(
      "`beta` must be numeric and have one entry per column of `xx`.",
      call. = FALSE
    )
  }
  
  if (length(n) != 1L || !is.finite(n) || n <= 0 || n != as.integer(n)) {
    stop(
      "`attr(sumstats, \"nobs\")` must be a positive integer.",
      call. = FALSE
    )
  }
  
  if (!is.numeric(index_pgs) || anyNA(index_pgs)) {
    stop("`index_pgs` must contain integer indices.", call. = FALSE)
  }
  
  if (!is.numeric(index_covar) || anyNA(index_covar)) {
    stop("`index_covar` must contain integer indices.", call. = FALSE)
  }
  
  index_pgs <- as.integer(index_pgs)
  index_covar <- as.integer(index_covar)
  
  if (length(index_pgs) == 0L) {
    stop("`index_pgs` must select at least one predictor.", call. = FALSE)
  }
  
  all_index <- c(index_pgs, index_covar)
  
  if (any(all_index < 1L | all_index > ncol(xx))) {
    stop("Predictor indices are outside the dimensions of `xx`.", call. = FALSE)
  }
  
  if (anyDuplicated(index_pgs) || anyDuplicated(index_covar)) {
    stop("Predictor indices must not be duplicated.", call. = FALSE)
  }
  
  if (length(intersect(index_pgs, index_covar)) > 0L) {
    stop(
      "`index_pgs` and `index_covar` must not overlap.",
      call. = FALSE
    )
  }
  
  if (any(!is.finite(xx)) ||
      any(!is.finite(xy)) ||
      any(!is.finite(beta[index_pgs]))) {
    stop("Inputs used in the model must all be finite.", call. = FALSE)
  }
  
  if (!isTRUE(all.equal(xx, t(xx), tolerance = sqrt(tol)))) {
    stop("`sumstats$xx` must be symmetric.", call. = FALSE)
  }
  
  
  sdy <- sqrt(attr(sumstats, "yssq")/n)
  sdx <- sdy/beta_multiplier
  sdx_pgs <- sdx[index_pgs]
  sdx_covar <- sdx[index_covar]
  Dsx_covar <- diag(sdx_covar)
  
  beta_pgs <- beta[index_pgs]
  beta_covar <- beta[index_covar]
  
  xx_pgs <- xx[index_pgs, index_pgs, drop = FALSE]
  xx_covar <- xx[index_covar, index_covar, drop = FALSE]
  xx_pgs_covar <- xx[index_pgs, index_covar, drop = FALSE]
  
  xy_pgs <- xy[index_pgs]
  xy_covar <- xy[index_covar]
  
  q <- drop(beta_pgs %*% xx_pgs %*% beta_pgs)
  
  Umat <- rbind (cbind(1, beta_pgs %*% xx_pgs_covar %*%  Dsx_covar/sqrt(q) ),
                 cbind(Dsx_covar %*% t(xx_pgs_covar) %*% beta_pgs/sqrt(q), Dsx_covar %*% xx_covar %*% Dsx_covar ))
  uvec <- c( (sdy * t(beta_pgs) %*% xy_pgs)/sqrt(q) ,sdy * Dsx_covar %*% xy_covar )
  alpha  <- qr.solve(Umat, uvec, tol = tol)
  R2 <- drop((alpha %*% Umat %*% alpha)/sdy^2)
  mse <- sdy^2*(1-R2)
  mse <- max(mse, 0)
  
  covar_names <- colnames(xx)[index_covar]
  
  if (length(index_covar) > 0L) {
    if (is.null(covar_names)) {
      covar_names <- paste0("covariate_", seq_along(index_covar))
    }
    names(uvec) <- c("PGS_ensemble", covar_names)
  } else{
    names(uvec) <- c("PGS_ensemble")
  }
  
 
  names(alpha) <- names(uvec)
 
  
  n_parameters <- length(alpha)
  df <- n - n_parameters
  var_alpha <- mse* solve(Umat)/df
  
  
  # Remove minor numerical asymmetry.
  var_alpha <- (var_alpha + t(var_alpha)) / 2
  
  dimnames(var_alpha) <- list(names(alpha), names(alpha))
   
  n <- attr(sumstats, "nobs")
  
  ncase <- NA
  case_freq <- NA
  auc <- NA
  auc_var <- NA
  
  
  ## For binary trait approximate AUC based on sumstats
  
  if(trait_type == "binary"){
    ncase <- attr(sumstats, "ysum")
    p <- ncase/n
    
    R2_full <- drop((alpha %*% Umat %*% alpha)/sdy^2)
    D2 <- R2_full/( p*(1-p)*(1-R2_full) )
    auc_full <- pnorm(sqrt(D2/2))
    
    if(length(alpha) > 1){
      R2_cov <- drop((alpha[-1] %*% Umat[-1,-1,drop=FALSE] %*% alpha[-1])/sdy^2)
      D2 <- R2_cov/( p*(1-p)*(1-R2_cov) )
      auc_cov <- pnorm(sqrt(D2/2))
    } else{
      R2_cov <- NA
      auc_cov <- NA
    }
    
    R2_pgs <- drop((alpha[1] %*% Umat[1,1,drop=FALSE] %*% alpha[1])/sdy^2)
    D2 <- R2_pgs/( p*(1-p)*(1-R2_pgs) )
    auc_pgs <- pnorm(sqrt(D2/2))
    
    auc <- c(auc_full, auc_cov, auc_pgs)
    aucname <- c("Full", "Covar", "PGS_Ensemble")
    names(auc) <- aucname
    
    auc_var <- c(
      hanley_mcneil(auc_full, n.case = ncase, n.control = n - ncase)$variance,
      if (is.na(auc_cov)) NA_real_ else
        hanley_mcneil(auc_cov, n.case = ncase, n.control = n - ncase)$variance,
      hanley_mcneil(auc_pgs, n.case = ncase, n.control = n - ncase)$variance
    )
    
    names(auc_var) <- aucname
  }
  
  ## model R2
  ## full model
  
  R2_partial <- (R2_full - R2_cov) / (1 - R2_cov)
  
  ## approximate odds ratio
  log_or <- alpha/(p*(1-p))
  or <- exp(log_or)
  list(
    coefficients = alpha,
    varcov = var_alpha,
    sigma2 = mse,
    df = df,
    R2_full_model = R2_full,
    R2_covar_model = R2_cov,
    R2_partial = R2_partial,
    log_or = log_or,
    or = or,
    auc=auc,
    auc_var = auc_var,
    ncase=ncase,
    ncont=n-ncase,
    n = n
  )
}


#' Fit Marginal Polygenic Score Models from Summary Statistics
#'
#' Fits one summary-statistic regression model for each selected PGS predictor,
#' adjusting each model for the same selected covariates. PGS predictors are
#' identified from column names containing `"PGS"`; covariates are all remaining
#' columns. Only predictors with `abs(beta) > 1e-6` are selected.
#'
#' @param sumstats A list-like object containing `xx`, a square predictor
#'   cross-product/correlation matrix, and `xy`, a numeric vector with one entry
#'   per column of `xx`. The function uses `attr(sumstats, "nobs")` for sample
#'   size and `attr(sumstats, "yssq")` for the outcome sum of squares. For binary
#'   traits, `attr(sumstats, "ysum")` must contain the number of cases. Column
#'   names of `sumstats$xx` are used to identify PGS predictors.
#' @param beta Numeric vector with one entry per column of `sumstats$xx`. In this
#'   function, its values are used to select predictors through
#'   `abs(beta) > 1e-6`; the magnitudes of the selected weights are not otherwise
#'   used in the marginal regression calculations.
#' @param trait_type Character string specifying the trait type. When
#'   `trait_type = "binary"`, approximate AUCs, log odds ratios, and odds ratios
#'   are calculated.
#' @param tol Numeric tolerance argument retained for interface consistency.
#'   The current implementation does not use `tol` in its calculations.
#'
#' @details
#' For each selected PGS column in turn, the function forms a model containing
#' that PGS and all selected covariates. If \eqn{R} denotes the corresponding
#' predictor matrix from `sumstats$xx` and \eqn{r} the corresponding entries of
#' `sumstats$xy`, the coefficient vector is obtained from
#' \deqn{\alpha = R^{-1}r.}
#' The reported PGS coefficient is multiplied by the outcome standard deviation
#' derived from `attr(sumstats, "yssq")`. The full-model \eqn{R^2} is
#' \deqn{R^2 = r^T R^{-1} r.}
#'
#' For binary traits, the function uses the case fraction to obtain approximate
#' AUC values for the full model, the covariate-only model, and the PGS-only
#' model. Approximate log odds ratios are computed from the marginal PGS
#' coefficient as \eqn{\alpha/[p(1-p)]}.
#'
#' @return A data frame with one row per selected PGS predictor and columns:
#' \describe{
#'   \item{`coef_pgs`}{Estimated marginal PGS coefficient after adjustment for
#'     selected covariates.}
#'   \item{`se`}{Standard error of `coef_pgs`.}
#'   \item{`R2_full`}{\eqn{R^2} for the PGS-plus-covariate model.}
#'   \item{`log_or`}{Approximate binary-trait log odds ratio.}
#'   \item{`or`}{Approximate binary-trait odds ratio.}
#'   \item{`auc_full`}{Approximate AUC for the full model.}
#'   \item{`auc_covar`}{Approximate AUC for the covariate-only model, or `NA`
#'     when no covariates are included.}
#'   \item{`auc_pgs`}{Approximate AUC for the PGS-only model.}
#' }
#' Row names are the selected PGS column names from `sumstats$xx`.
#'
#' @examples
#' # res <- pgs_marginal_sumstats(sumstats, beta, trait_type = "binary")
#'
#' @export
pgs_marginal_sumstats <- function(
    sumstats,
    beta,
    trait_type = "binary",
    tol = 1e-10
){
 
  
  is_pgs <- grepl("PGS", colnames(sumstats$xx)) 
  is_beta <- abs(beta) > 1e-6
  is_covar <- !is_pgs
  index_pgs <- (1:ncol(sumstats$xx))[is_pgs & is_beta]
  index_covar <- (1:ncol(sumstats$xx))[is_covar & is_beta]
  
  xx <- sumstats$xx
  xy <- sumstats$xy
  n <- attr(sumstats, "nobs")
  sdy <- sqrt(attr(sumstats, "yssq")/n)
  
  xx_pgs <- xx[index_pgs, index_pgs, drop = FALSE]
  xx_covar <- xx[index_covar, index_covar, drop = FALSE]
  xx_pgs_covar <- xx[index_pgs, index_covar, drop = FALSE]
  
  xy_pgs <- xy[index_pgs]
  xy_covar <- xy[index_covar]
  
  pgs_name <- colnames(sumstats$xx)[index_pgs]
  
  alpha_pgs <- rep(NA,  length(index_pgs))
  var_alpha_pgs <- rep(NA,  length(index_pgs))
  R2_full  <- rep(NA,  length(index_pgs))
  auc_full <- rep(NA, length(index_pgs))
  auc_covar <- rep(NA, length(index_pgs))
  auc_pgs <- rep(NA, length(index_pgs))  
  log_or <- rep(NA, length(index_pgs))
  or <- rep(NA, length(index_pgs))

 
  for(i in 1:length(index_pgs)){
    index <- c(index_pgs[i], index_covar)
    Rmat <- xx[index, index, drop=FALSE]
    rvec <- xy[index]
    q <- length(rvec)
    Rinv <- solve(Rmat)
    alpha <- drop(Rinv %*% rvec)
    alpha_pgs[i] <- sdy*alpha[1]
    R2_full[i] <- R2 <- drop(rvec %*% Rinv %*% rvec)
    var_alpha <- (1-R2)* Rinv/(n-q)
    var_alpha_pgs[i] <- sdy^2*var_alpha[1,1]
    if(length(index_covar > 0)){
      Rmat_covar <- xx[index_covar, index_covar, drop=FALSE]
      Rcovar_inv <- solve(Rmat_covar)
      rvec_covar <- xy[index_covar]
      R2_covar <-  drop(rvec_covar %*% Rcovar_inv %*% rvec_covar)
    } else{
      R2_covar <- NA
    }
    ## For binary trait approximate AUC based on sumstats
    if(trait_type == "binary"){
      ncase <- attr(sumstats, "ysum")
      p <- ncase/n
      
      ## standardized case-control mean difference for full model
      D2 <- R2/( p*(1-p)*(1-R2) )
      auc_full[i] <- pnorm(sqrt(D2/2))
      
      # standardized case-control mean difference for covar only model
      if(length(index_covar) > 0){
        D2_covar <- R2_covar / ( p*(1-p)*(1-R2_covar) )
        auc_covar[i] <- pnorm(sqrt(D2_covar/2))
      } else{
        auc_covar <- NA
      }
      ## standardized case-control mean difference for pgs only model
      r2 <- rvec[1]^2
      d2 <-  r2 / ( p*(1-p)*(1-r2) )
      auc_pgs[i] <- pnorm(sqrt(d2/2))
      
      log_or[i] <- alpha_pgs[i]/(p*(1-p))
      or[i] <- exp(log_or[i])
    }
    
  }
  
    df <- data.frame(coef_pgs=alpha_pgs, se=sqrt(var_alpha_pgs),
                     R2_full=R2_full,log_or = log_or, or = or,  auc_full=auc_full, auc_covar = auc_covar, auc_pgs=auc_pgs)
    rownames(df) <- pgs_name
    return(df)
      
}

#' Hanley-McNeil Variance and Confidence Interval for an AUC
#'
#' Computes the Hanley-McNeil large-sample variance approximation for an area
#' under the ROC curve (AUC), together with its standard error and an untruncated
#' normal-approximation 95 percent confidence interval.
#'
#' @param auc Numeric scalar giving the AUC.
#' @param n.case Number of cases.
#' @param n.control Number of controls.
#'
#' @details
#' For a non-missing AUC \eqn{A}, the implementation defines
#' \deqn{Q_1 = A/(2-A), \qquad Q_2 = 2A^2/(1+A)}
#' and estimates the variance as
#' \deqn{\frac{A(1-A) + (n_1-1)(Q_1-A^2) + (n_0-1)(Q_2-A^2)}{n_1 n_0},}
#' where \eqn{n_1} and \eqn{n_0} are the numbers of cases and controls. The
#' confidence interval is `auc + c(-1, 1) * 1.96 * se` and is not constrained to
#' the interval from 0 to 1.
#'
#' @return A list with components `variance`, `se`, `lower`, and `upper`.
#'
#' @examples
#' \dontrun{hanley_mcneil(auc = 0.75, n.case = 100, n.control = 200)}
#'
#' @keywords internal
hanley_mcneil <- function(auc, n.case, n.control){
  if(is.na(auc)){ 
    var = NA
    se = NA
    lower = NA
    upper = NA
  } else{
    
    Q1 <- auc/(2-auc)
    Q2 <- 2*auc^2/(1+auc)
    
    var <- (auc*(1-auc) +
              (n.case-1)*(Q1-auc^2) +
              (n.control-1)*(Q2-auc^2)) /
      (n.case*n.control)
    
    se <- sqrt(var)
    
    ci <- auc + c(-1,1)*1.96*se
  }
  
  list(
    variance = var,
    se = se,
    lower = ci[1],
    upper = ci[2]
  )
}


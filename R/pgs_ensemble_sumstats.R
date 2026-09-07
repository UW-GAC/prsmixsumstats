#' Estimate the Effect of a Fixed-Weight Polygenic Score (Ensemble PGS) from Summary Statistics
#'
#' Estimates the association between a fixed-weight polygenic score (PGS) and
#' an outcome while adjusting for covariates, using predictor cross-products
#' and predictor-outcome cross-products. The function also computes model
#' R-squared summaries from the supplied fixed predictor weights and, for
#' binary traits, approximate AUC summaries.
#'
#' The Ensemble PGS is defined as
#'
#' \deqn{S = X_{\mathrm{PGS}} \beta_{\mathrm{PGS}},}
#'
#' where the weights in \code{beta} are treated as fixed rather than estimated
#' in this model. The fitted summary-statistic regression is
#'
#' \deqn{y = \alpha_{\mathrm{PGS}} S +
#'       X_{\mathrm{covar}} \alpha_{\mathrm{covar}} + \epsilon.}
#'
#' The function assumes that the supplied summary statistics satisfy
#'
#' \deqn{xx = X^\top X / n}
#'
#' and
#'
#' \deqn{xy = X^\top y / n.}
#'
#' It additionally assumes that the outcome is centered and normalized such
#' that
#'
#' \deqn{y^\top y / n = 1.}
#'
#' No intercept is fitted. Centering the outcome and all predictors is therefore
#' required if an intercept would otherwise be needed.
#'
#' @param sumstats A list-like object containing:
#'   \describe{
#'     \item{\code{xx}}{A numeric square matrix containing
#'       \eqn{X^\top X / n}.}
#'     \item{\code{xy}}{A numeric vector containing
#'       \eqn{X^\top y / n}.}
#'   }
#'   The sample size must be stored in \code{attr(sumstats, "nobs")}.
#'   When \code{trait_type = "binary"}, the number of cases must additionally
#'   be stored in \code{attr(sumstats, "ysum")} for the AUC calculations.
#'
#' @param beta Numeric vector of fixed predictor weights with one entry per
#'   column of \code{xx}. Entries selected by \code{index_pgs} are used to
#'   construct the PGS. Entries selected by \code{index_covar}, as well as the
#'   full vector, are also used in the approximate AUC and R-squared summaries.
#'
#' @param trait_type Character string specifying the outcome type. The default,
#'   \code{"binary"}, enables approximate AUC calculations using the number of
#'   cases stored in \code{attr(sumstats, "ysum")}. For non-binary traits,
#'   AUC-related quantities are not calculated and are returned as \code{NA}.
#'
#' @param index_pgs Integer indices identifying the columns of \code{xx} and
#'   entries of \code{xy} and \code{beta} used to construct the PGS.
#'
#' @param index_covar Integer indices identifying adjustment covariates. May be
#'   empty (the default), in which case the fitted model contains only the PGS.
#'
#' @param tol Numeric tolerance used when solving the normal equations and
#'   assessing a slightly negative residual quantity caused by floating-point
#'   error.
#'
#' @return A list with components:
#'   \describe{
#'     \item{\code{coefficients}}{Estimated coefficient for the Ensemble PGS
#'       followed by coefficients for the adjustment covariates.}
#'     \item{\code{varcov}}{Estimated variance-covariance matrix of the
#'       coefficients, using the normalization of the supplied cross-products.}
#'     \item{\code{sigma2}}{Residual variance quantity computed as
#'       \code{(1 - alpha^T B) / df}. Because \code{xx} and \code{xy} are
#'       normalized by \code{n}, this is scaled by \code{1/n} relative to the
#'       conventional residual variance based on residual sum of squares.}
#'     \item{\code{df}}{Residual degrees of freedom, equal to the sample size
#'       minus the number of fitted coefficients.}
#'     \item{\code{r2_full_model}}{R-squared summary computed from the full
#'       supplied \code{beta}, \code{xx}, and \code{xy}.}
#'     \item{\code{r2_covar_model}}{R-squared summary computed from the
#'       covariate subset of the supplied weights and summary statistics.}
#'     \item{\code{r2_partial}}{Partial R-squared calculated from the full and
#'       covariate-model R-squared summaries.}
#'     \item{\code{auc}}{For binary traits, a named numeric vector containing
#'       approximate AUCs for the full fixed-weight score, the covariate-only
#'       score, and the PGS-only score. For non-binary traits, \code{NA}.}
#'     \item{\code{auc_var}}{For binary traits, Hanley-McNeil variance estimates
#'       corresponding to the entries of \code{auc}. For non-binary traits,
#'       \code{NA}.}
#'     \item{\code{ncase}}{For binary traits, the number of cases from
#'       \code{attr(sumstats, "ysum")}; otherwise \code{NA}.}
#'     \item{\code{ncont}}{For binary traits, the number of controls, computed
#'       as \code{attr(sumstats, "nobs") - attr(sumstats, "ysum")}; otherwise
#'       \code{NA}.}
#'     \item{\code{ntot}}{Total sample size from
#'       \code{attr(sumstats, "nobs")}.}
#'   }
#'
#' @details
#' Let \eqn{Z} contain the constructed PGS as its first column and the
#' adjustment covariates as its remaining columns. The function constructs
#'
#' \deqn{A = Z^\top Z / n}
#'
#' and
#'
#' \deqn{B = Z^\top y / n,}
#'
#' then solves \eqn{A\alpha = B}.
#'
#' Under the assumption \eqn{y^\top y/n = 1}, the code computes the normalized
#' residual quantity
#'
#' \deqn{s = 1 - \alpha^\top B,}
#'
#' and sets \eqn{\widehat{\sigma}^2 = s / df}. The coefficient covariance
#' matrix returned by the function is
#'
#' \deqn{\widehat{\mathrm{Var}}(\alpha)
#'       = \widehat{\sigma}^2 A^{-1}.}
#'
#' With \code{xx} and \code{xy} normalized by \code{n}, this produces the
#' same overall covariance scaling as using the conventional residual sum of
#' squares \eqn{n s} together with an additional factor of \eqn{1/n}.
#'
#' The uncertainty in the supplied PGS and other fixed predictor weights is not
#' incorporated. Thus, the variance estimates are conditional on \code{beta}.
#'
#' When \code{trait_type = "binary"}, approximate AUCs are computed separately
#' for the full fixed-weight score, the covariate-only score, and the PGS-only
#' score. These calculations use the case fraction derived from
#' \code{attr(sumstats, "ysum")} and \code{attr(sumstats, "nobs")}. Their
#' variances are estimated with the Hanley-McNeil approximation implemented by
#' \code{hanley_mcneil()}. AUC-related quantities are not calculated for
#' non-binary traits.
#'
#' Model R-squared summaries are calculated directly from the supplied fixed
#' weights as \eqn{2\beta^\top xy - \beta^\top xx\beta}, with analogous
#' calculations for the covariate-only subset. The returned partial R-squared is
#' \eqn{(R^2_{full} - R^2_{covar}) / (1 - R^2_{covar})}.
#'
#' @examples
#' set.seed(1)
#' n <- 100L
#' X <- scale(matrix(rnorm(500), nrow = n), center = TRUE, scale = FALSE)
#' y <- rep(c(0, 1), each = n / 2)
#' y <- as.numeric(scale(y, center = TRUE, scale = FALSE))
#' y <- y / sqrt(drop(crossprod(y)) / n)
#'
#' xx <- crossprod(X) / n
#' xy <- drop(crossprod(X, y)) / n
#'
#' sumstats <- list(xx = xx, xy = xy)
#' attr(sumstats, "nobs") <- n
#' attr(sumstats, "ysum") <- 50L
#'
#' fit <- pgs_ensemble_sumstats(
#'   sumstats = sumstats,
#'   beta = rep(0.1, ncol(xx)),
#'   trait_type = "binary",
#'   index_pgs = 1:3,
#'   index_covar = 4:5
#' )
#'
#' @export
pgs_ensemble_sumstats <- function(
    sumstats,
    beta,
    trait_type = "binary",
    index_pgs,
    index_covar = integer(0),
    tol = 1e-10
) {
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
  
  beta_pgs <- beta[index_pgs]
  beta_covar <- beta[index_covar]
  
  xx_pgs <- xx[index_pgs, index_pgs, drop = FALSE]
  xx_covar <- xx[index_covar, index_covar, drop = FALSE]
  xx_pgs_covar <- xx[index_pgs, index_covar, drop = FALSE]
  
  xy_pgs <- xy[index_pgs]
  xy_covar <- xy[index_covar]
  
  # Cross-product of the constructed PGS with itself.
  A11 <- drop(crossprod(beta_pgs, xx_pgs %*% beta_pgs))
  
  if (length(index_covar) > 0L) {
    # Cross-products of the constructed PGS with the covariates.
    A12 <- matrix(
      drop(crossprod(beta_pgs, xx_pgs_covar)),
      nrow = 1L
    )

    Amat <- rbind(
      cbind(A11, A12),
      cbind(t(A12), xx_covar)
    )

    Bvec <- c(
      PGS_ensemble = drop(crossprod(beta_pgs, xy_pgs)),
      xy_covar
    )
  } else {
    # PGS-only model. Keep Amat as a 1 x 1 matrix for qr.solve().
    Amat <- matrix(A11, nrow = 1L, ncol = 1L)
    Bvec <- c(PGS_ensemble = drop(crossprod(beta_pgs, xy_pgs)))
  }
  
  covar_names <- colnames(xx)[index_covar]
  
  if (length(index_covar) > 0L) {
    if (is.null(covar_names)) {
      covar_names <- paste0("covariate_", seq_along(index_covar))
    }
    
    names(Bvec) <- c("PGS_ensemble", covar_names)
  }
  
  alpha <- tryCatch(
    qr.solve(Amat, Bvec, tol = tol),
    error = function(e) {
      stop(
        "The model cross-product matrix is singular or ill-conditioned: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  names(alpha) <- names(Bvec)
  
  # Valid when y'y / n = 1.
  sse <-  (1 - drop(crossprod(alpha, Bvec)))
  
  # Allow only negligible negative values from floating-point error.
  sse_tolerance <- tol * n
  
  if (sse < -sse_tolerance) {
    stop(
      paste0(
        "The calculated SSE is negative. Check whether `xx` and `xy` ",
        "were divided by n and whether y'y / n equals 1."
      ),
      call. = FALSE
    )
  }
  
  sse <- max(sse, 0)
  
  n_parameters <- length(alpha)
  df <- n - n_parameters
  
  if (df <= 0L) {
    stop(
      "The sample size must exceed the number of fitted coefficients.",
      call. = FALSE
    )
  }
  
  sigma2 <- sse / df
  
  Amat_inv <- tryCatch(
    qr.solve(Amat, diag(nrow(Amat)), tol = tol),
    error = function(e) {
      stop(
        "Could not invert the model cross-product matrix: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  var_alpha <-  sigma2 * Amat_inv
  
  # Remove minor numerical asymmetry.
  var_alpha <- (var_alpha + t(var_alpha)) / 2
  
  dimnames(var_alpha) <- list(names(alpha), names(alpha))
  
 
   
  ntot <- attr(sumstats, "nobs")
  
  ncase <- NA
  case_freq <- NA
  auc <- NA
  auc_var <- NA
  
  
  ## For binary trait approximate AUC based on sumstats
  
  if(trait_type == "binary"){
    ncase <- attr(sumstats, "ysum")
    case_freq <- ncase/ntot
    
    auc_full <- auc_estimate(xx, xy, beta, case_freq)
    auc_covar <- if (length(index_covar) > 0L) {
      auc_estimate(xx_covar, xy_covar, beta_covar, case_freq)
    } else {
      NA_real_
    }
    
    auc_pgs <- auc_estimate(xx_pgs, xy_pgs, beta_pgs, case_freq)
    auc <- c(auc_full, auc_covar, auc_pgs)
    aucname <- c("Full", "Covar", "PGS_Ensemble")
    names(auc) <- aucname
    
    auc_var <- c(
      hanley_mcneil(auc_full, n.case = ncase, n.control = ntot - ncase)$variance,
      if (is.na(auc_covar)) NA_real_ else
        hanley_mcneil(auc_covar, n.case = ncase, n.control = ntot - ncase)$variance,
      hanley_mcneil(auc_pgs, n.case = ncase, n.control = ntot - ncase)$variance
    )
    
    names(auc_var) <- aucname
  }
  
  ## model R2
  ## full model
  r2_full_model <- drop(2*beta %*% xy  - beta %*% xx %*% beta)
  r2_reduced_model <- if (length(index_covar) > 0L) {
    drop(2 * beta_covar %*% xy_covar - beta_covar %*% xx_covar %*% beta_covar)
  } else {
    0
  }
  r2_partial <- (r2_full_model - r2_reduced_model) / (1 - r2_reduced_model)
  
  list(
    coefficients = alpha,
    varcov = var_alpha,
    sigma2 = sigma2,
    df = df,
    r2_full_model = r2_full_model,
    r2_covar_model = r2_reduced_model,
    r2_partial = r2_partial,
    auc=auc,
    auc_var = auc_var,
    ncase=ncase,
    ncont=ntot-ncase,
    ntot = ntot
  )
}


auc_estimate <- function(xx, xy, beta, case_freq){
    auc_num <- drop(beta %*% xy)
    pgs_within_group_var <- (beta %*% xx %*% beta -  (beta%*%xy)^2)
    auc_den <- drop(sqrt( 2*case_freq*(1-case_freq) * pgs_within_group_var))
    z <- auc_num/auc_den
    auc_approx <- pnorm(z)
    return(auc_approx)
}
  
hanley_mcneil <- function(auc, n.case, n.control){
  
  Q1 <- auc/(2-auc)
  Q2 <- 2*auc^2/(1+auc)
  
  var <- (auc*(1-auc) +
            (n.case-1)*(Q1-auc^2) +
            (n.control-1)*(Q2-auc^2)) /
    (n.case*n.control)
  
  se <- sqrt(var)
  
  ci <- auc + c(-1,1)*1.96*se
  
  list(
    variance = var,
    se = se,
    lower = ci[1],
    upper = ci[2]
  )
}


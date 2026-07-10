#' Rescale response-dependent summary statistics
#'
#' Rescales the response-dependent components of a summary statistics object
#' based on response vector \eqn{y}  multiplied by a scalar. If the
#' transformed response is \eqn{y^* = a y}, then the function updates:
#'
#' \itemize{
#'   \item \eqn{X^\top y \rightarrow a X^\top y}
#'   \item \eqn{\sum y \rightarrow a \sum y}
#'   \item \eqn{y^\top y \rightarrow a^2 y^\top y}
#' }
#'
#' The predictor-dependent quantities (e.g., \eqn{X^\top X}) are left unchanged.
#'
#' @param sumstats A summary statistics object containing an `xy` component and
#'   attributes `"ysum"` and `"yssq"`.
#' @param scale_multiplier Numeric scalar by which the original response vector
#'   is to be multiplied.
#'
#' @return A summary statistics object of the same structure as `sumstats`,
#'   with the response-dependent quantities appropriately rescaled.
#'
#' @examples
#' dat <- sim_test_dat(10, nprs=10)
#' ss <- make_sumstats(dat$x, dat$y)
#' ss_scaled <- rescale_sumstats(ss, scale_multiplier = 2)
#'
#' @export
rescale_sumstats <- function(sumstats, scale_multiplier) {
  ss <- sumstats
  ss$xy <- scale_multiplier * sumstats$xy
  attr(ss, "ysum") <- scale_multiplier * attr(sumstats, "ysum")
  attr(ss, "yssq") <- scale_multiplier^2 * attr(sumstats, "yssq")
  return(ss)
}
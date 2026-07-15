#' Rescale Summary Statistics
#'
#' Rescales the summary statistics corresponding to a transformed design matrix
#' and response vector. If the transformed variables are
#'
#' \deqn{X^* = X \mathrm{diag}(b), \qquad y^* = a y,}
#'
#' where `x_multiplier = b` and `y_multiplier = a`, then the function updates
#' the stored summary statistics as follows:
#'
#' \itemize{
#'   \item \eqn{X^\top X \rightarrow \mathrm{diag}(b) X^\top X \mathrm{diag}(b)}
#'   \item \eqn{X^\top y \rightarrow a\,\mathrm{diag}(b) X^\top y}
#'   \item Column sums of \eqn{X}: \eqn{\sum X_j \rightarrow b_j \sum X_j}
#'   \item \eqn{\sum y \rightarrow a \sum y}
#'   \item \eqn{y^\top y \rightarrow a^2 y^\top y}
#' }
#'
#' @param sumstats A summary statistics object containing components `xx` and
#'   `xy`, and attributes `"colsum"`, `"ysum"`, and `"yssq"`.
#' @param y_multiplier Numeric scalar by which the response vector is multiplied.
#' @param x_multiplier Numeric vector of column-wise multipliers for the design
#'   matrix. Its length must equal the number of predictors.
#'
#' @return A summary statistics object with the same structure as `sumstats`,
#'   with all predictor- and response-dependent quantities rescaled to
#'   correspond to the transformed variables.
#'
#' @examples
#' dat <- sim_test_dat(10, nprs=10)
#' ss <- make_sumstats(dat$x[,1:3], dat$y)
#' ss_scaled <- rescale_sumstats(
#'   ss,
#'   y_multiplier = 2,
#'   x_multiplier = c(1, 0.5, 2)
#' )
#'
#' @export
rescale_sumstats <- function(sumstats, y_multiplier=1, x_multiplier=rep(1, ncol(sumstats$xx))) {
  if(ncol(sumstats$xx) != length(x_multiplier)){
    stop("ncol of xx != length of x_multiplier")
  }
  if(length(y_multiplier) != 1){
    stop("length(y_multiplier) != 1")
  }
  
  ynames <- rownames(sumstats$xy)
  sumstats$xy <- diag(x_multiplier) %*% sumstats$xy * y_multiplier
  attr(sumstats, "ysum") <- y_multiplier * attr(sumstats, "ysum")
  attr(sumstats, "yssq") <- y_multiplier^2 * attr(sumstats, "yssq")
  rownames(sumstats$xy) <- ynames
  
  xnames <- rownames(sumstats$xx)
  sumstats$xx <-  diag(x_multiplier) %*% sumstats$xx %*% diag(x_multiplier)
  attr(sumstats, "colsum") <-   attr(sumstats, "colsum") * x_multiplier
  dimnames(sumstats$xx) <- list(xnames, xnames)
  
  return(sumstats)
}



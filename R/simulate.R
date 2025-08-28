
## simulate y and X matrix from covariance
## matrix:
##        | var(y),  cov(y,x) |
## vmat = | cov(x,y) cov(x,x) |
## and return matrix [Y|X]
simulate_yx <- function(n, vmat) {
    p <- ncol(vmat)
    svd_decomp <- svd(vmat)
    U <- svd_decomp$u
    D <- diag(sqrt(pmax(svd_decomp$d, 0)))
    Z <- matrix(rnorm(n * p), nrow = n)
    return(Z %*% U %*% D)
}

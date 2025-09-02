new_sumstats <- function(xx, xy, nsubj, nmiss, nobs, colsum, ysum, yssq) {
    ss <- list(xx=xx, xy=xy)
    attr(ss, "nsubj") <- nsubj
    attr(ss, "nmiss") <- nmiss
    attr(ss, "nobs") <-  nobs
    attr(ss, "colsum") <- colsum
    attr(ss, "ysum") <-  ysum
    attr(ss, "yssq") <-  yssq
    return(ss)
}


validate_sumstats <- function(ss) {
    stopifnot(nrow(ss$xx) == ncol(ss$xx))
    stopifnot(nrow(ss$xx) == nrow(ss$xy))
    stopifnot(ncol(ss$xy) == 1)
    stopifnot(attr(ss, "nsubj") == attr(ss, "nobs") + attr(ss, "nmiss"))
    stopifnot(length(attr(ss, "colsum")) == nrow(ss$xx))
}

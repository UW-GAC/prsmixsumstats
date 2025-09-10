match_sumstats <- function(sumstats) {
  if (length(sumstats) == 1) {
      return(list(sumstats=sumstats, incomplete_cols=character()))
  }
  cols <- lapply(sumstats, function(x) colnames(x$xx))
  cols_intersect <- cols[[1]]
  cols_union <- cols[[1]]
  for (i in 2:length(cols)) {
    cols_intersect <- intersect(cols_intersect, cols[[i]])
    cols_union <- union(cols_union, cols[[i]])
  }
  incomplete_cols <- setdiff(cols_union, cols_intersect)
  tmp <- list()
  for (i in 1:length(sumstats)) {
    missing_cols <- setdiff(cols_union, cols[[i]])
    tmp[[i]] <- add_cols_sumstats(sumstats[[i]], missing_cols)
    tmp[[i]] <- match_cols_sumstats(tmp[[i]], cols_union)
  }
  lapply(tmp, validate_sumstats)
  return(list(sumstats=tmp, incomplete_cols=incomplete_cols))
}


match_cols_sumstats <- function(ss, col_names) {
  tmp <- ss
  tmp$xx <- ss$xx[col_names, col_names]
  tmp$xy <- ss$xy[col_names,,drop=FALSE]
  attr(tmp, "colsum") <- attr(ss, "colsum")[col_names]
  return(tmp)
}


#' @importFrom stats setNames
add_cols_sumstats <- function(ss, col_names) {
  tmp <- ss
  tmp$xx <- add_cols_square_matrix(ss$xx, col_names)
  tmp$xy <- add_rows_matrix(ss$xy, col_names)
  xzeros <- setNames(rep(0, length(col_names)), col_names)
  attr(tmp, "colsum") <- c(attr(ss, "colsum"), xzeros)
  return(tmp)
}


add_cols_square_matrix <- function(x, col_names) {
  xc <- matrix(0, ncol=length(col_names), nrow=nrow(x), dimnames=list(NULL, col_names))
  x <- cbind(x, xc)
  xr <- matrix(0, ncol=ncol(x), nrow=length(col_names), dimnames=list(col_names, NULL))
  x <- rbind(x, xr)
  return(x)
}


add_rows_matrix <- function(x, col_names) {
    xr <- matrix(0, ncol=ncol(x), nrow=length(col_names), dimnames=list(col_names, NULL))
    x <- rbind(x, xr)
    return(x)
}

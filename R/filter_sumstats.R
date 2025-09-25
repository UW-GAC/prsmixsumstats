#' Filter a sumstats object on a column in another table
#' 
#' Filter a sumstats object on a column in another table
#' 
#' The intended use of this function is to filter scores that fall below a threshold of
#' overlapping variants. name_col should be the score identifier, filter_col should be
#' the column with values to filter on (overlap or beta_fraction), and filter_threshold
#' is the minimum allowed value of filter_col.
#' @param sumstats sumstats object
#' @param filter_tbl data.frame with filter information
#' @param name_col column of filter_tbl with column names to be removed
#' @param filter_col column of filter_tbl with values to filter on
#' @param filter_threshold remove from sumstats any values of name_col where filter_col < filter_threshold
#' @return sumstats object without columns that fall below the threshold
#' @export
filter_sumstats <- function(sumstats, filter_tbl, name_col, filter_col, filter_threshold) {
    filtered_ids <- filter_tbl[[name_col]][filter_tbl[[filter_col]] < filter_threshold]
    drop_cols_sumstats(sumstats, filtered_ids)
}


#' Drop columns from a sumstats object
#' 
#' Drop columns from a sumstats object
#' 
#' The intended use of this function is to drop scores from a sumstats object
#' @param sumstats sumstats object
#' @param drop_cols column names to drop
#' @return sumstats object without drop_cols
#' @export
drop_cols_sumstats <- function(sumstats, drop_cols) {
    keep_cols <- setdiff(colnames(sumstats$xx), drop_cols)
    sumstats_filt <- match_cols_sumstats(sumstats, keep_cols)
    validate_sumstats(sumstats_filt)
    return(sumstats_filt)
}

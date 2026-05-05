#' Convert sumstats to text files
#'
#' Convert sumstats to text files and vice versa
#' 
#' sumstats_to_text writes a sumstats object to four text files ending in _xx.txt, _xy.txt,
#' _colsum.txt and _attr.txt. sumstats_from_text takes these four text files as input
#' and returns a sumstats object.
#'
#' @rdname sumstats_to_text
#' @param sumstats_file a RDS file ending in .rds that contains a sumstats object
#' @returns sumstats_to_text: vector of filenames
#' @examples
#' dat <- sim_test_dat(100, 1000, prev=.1, beta.sd=2)
#' ss1 <- make_sumstats(dat$x, dat$y, center=FALSE)  
#' ssfile <- paste0(tempfile(), ".rds")
#' saveRDS(ss1, ssfile)
#' files <- sumstats_to_text(ssfile)
#' ss2 <- sumstats_from_text(files)
#' @importFrom methods is
#' @importFrom utils write.table
#' @export
sumstats_to_text <- function(sumstats_file) {
    sumstats <- readRDS(sumstats_file)
    stopifnot(is(sumstats, "sumstats"))
    validate_sumstats(sumstats)
    
    prefix <- sub(".rds$", "", basename(sumstats_file))
    xx_file <- paste0(prefix, "_xx.txt")
    xy_file <- paste0(prefix, "_xy.txt")
    colsum_file <- paste0(prefix, "_colsum.txt")
    attr_file <- paste0(prefix, "_attr.txt")
    write.table(sumstats$xx, xx_file, sep="\t")
    write.table(sumstats$xy, xy_file, sep="\t")
    colsum <- as.data.frame(attr(sumstats, "colsum"))
    write.table(colsum, colsum_file, sep="\t", col.names=FALSE)
    attr_tbl <- data.frame(
        nsubj = attr(sumstats, "nsubj"),
        nmiss = attr(sumstats, "nmiss"),
        nobs = attr(sumstats, "nobs"),
        ysum = attr(sumstats, "ysum"),
        yssq = attr(sumstats, "yssq"),
        centered = attr(sumstats, "centered")
    )
    write.table(attr_tbl, attr_file, sep="\t")
    return(c(xx_file, xy_file, colsum_file, attr_file))
}

#' @rdname sumstats_to_text
#' @param sumst_text_files vector of files produced by sumstats_to_text
#' @returns sumstats_from_text: sumstats object
#' @importFrom utils read.table
#' @export
sumstats_from_text <- function(sumst_text_files) {
    xx_file <- sumst_text_files[grepl("_xx.txt", sumst_text_files)]
    xy_file <- sumst_text_files[grepl("_xy.txt", sumst_text_files)]
    colsum_file <- sumst_text_files[grepl("_colsum.txt", sumst_text_files)]
    attr_file <- sumst_text_files[grepl("_attr.txt", sumst_text_files)]
    xx <- as.matrix(read.table(xx_file, header=TRUE, sep="\t"))
    xy <- as.matrix(read.table(xy_file, header=TRUE, sep="\t"))
    colnames(xy) <- NULL
    colsum <- read.table(colsum_file)
    colsum <- setNames(colsum[[2]], colsum[[1]])
    attr_tbl <- read.table(attr_file, header=TRUE, sep="\t")
    new_sumstats(xx, xy, attr_tbl$nsubj, attr_tbl$nmiss, attr_tbl$nobs, 
                 colsum, attr_tbl$ysum, attr_tbl$yssq, attr_tbl$centered)
}

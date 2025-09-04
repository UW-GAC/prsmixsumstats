test_that("make_sumstats", {
    dat <- .example_data(n1=100, n2=50, n3=30, nprs=1000)
    ss1 <- make_sumstats(dat[[1]]$x, dat[[1]]$y)
    validate_sumstats(ss1)
    expect_equal(dim(ss1$xx), c(1004,1004))
    expect_equal(dim(ss1$xy), c(1004,1))
    expect_equal(attr(ss1, "nsubj"), 100)
    expect_equal(attr(ss1, "nmiss"), 18)
})


test_that("make_sumstats_center", {
    dat <- .example_data(n1=100, n2=50, n3=30, nprs=1000)
    ss1 <- make_sumstats_center(dat[[1]]$x, dat[[1]]$y)
    validate_sumstats(ss1)
    expect_equal(dim(ss1$xx), c(1004,1004))
    expect_equal(dim(ss1$xy), c(1004,1))
    expect_equal(attr(ss1, "nsubj"), 100)
    expect_equal(attr(ss1, "nmiss"), 18)
})


test_that("combine_sumstats", {
    dat <- .example_data(n1=100, n2=50, n3=30, nprs=1000)
    ss1 <- make_sumstats(dat[[1]]$x, dat[[1]]$y)
    ss2 <- make_sumstats(dat[[2]]$x, dat[[2]]$y)
    ss3 <- make_sumstats(dat[[3]]$x, dat[[3]]$y)
    chk <- combine_sumstats(list(ss1, ss2, ss3))
    ss <- chk$sumstats
    expect_equal(dim(ss1$xx), c(1004,1004))
    expect_equal(dim(ss1$xy), c(1004,1))
    expect_equal(attr(ss, "nsubj"), 180)
})


test_that("make_sumstats_clusters", {
    dat <- .example_cluster_data(n=100, nprs=1000)
    make_sumstats_clusters(dat$trait, dat$covariates, dat$scores, dat$clusters, "pheno", "cohort")
    all <- readRDS("pheno_cohort_sumstats.rds")
    c1 <- readRDS("pheno_cohort_cluster1_sumstats.rds")
    c2 <- readRDS("pheno_cohort_cluster2_sumstats.rds")
    expect_equal(attr(all, "nsubj"), 80)
    expect_true(all(abs(all$xy - (c1$xy + c2$xy)) < 1e-7))
    file.remove(paste0("pheno_cohort", c("", "_cluster1", "_cluster2"), "_sumstats.rds"))
})


test_that("only one cluster", {
    dat <- .example_cluster_data(n=100, nprs=1000)
    make_sumstats_clusters(dat$trait, dat$covariates, dat$scores, dat$clusters, "pheno", "cohort",
                           min_cluster_size = 40)
    expect_true(file.exists("pheno_cohort_cluster1_sumstats.rds"))
    expect_false(file.exists("pheno_cohort_cluster2_sumstats.rds"))
    file.remove(paste0("pheno_cohort", c("", "_cluster1"), "_sumstats.rds"))
})


test_that("add_cols_square_matrix", {
    x <- matrix(1, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    col_names <- c("d", "e")
    chk <- add_cols_square_matrix(x, col_names)
    exp <- matrix(c(rep(c(rep(1, 3), rep(0, 2)), 3), rep(rep(0, 5), 2)), byrow=TRUE, nrow=5, ncol=5, 
                  dimnames=list(letters[1:5], letters[1:5]))
    expect_equal(chk, exp)
})


test_that("add_rows_matrix", {
    x <- matrix(1, nrow=3, ncol=1, dimnames=list(letters[1:3], NULL))
    col_names <- c("d", "e")
    chk <- add_rows_matrix(x, col_names)
    exp <- matrix(c(rep(1, 3), rep(0, 2)), byrow=FALSE, nrow=5, ncol=1,
                  dimnames=list(letters[1:5], NULL))
    expect_equal(chk, exp)
})


test_that("add_cols_sumstats", {
    xx <- matrix(1, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    xy <- matrix(1:3, nrow=3, ncol=1, dimnames=list(letters[1:3], NULL))
    col_names <- c("d", "e")
    ss <- structure(list(xx=xx, xy=xy), "colsum"=colSums(xx))
    chk <- add_cols_sumstats(ss, col_names)
    exp1 <- matrix(c(rep(c(rep(1, 3), rep(0, 2)), 3), rep(rep(0, 5), 2)), byrow=TRUE, nrow=5, ncol=5,
                  dimnames=list(letters[1:5], letters[1:5]))
    exp2 <- matrix(c(1:3, rep(0, 2)), nrow=5, ncol=1, dimnames=list(letters[1:5], NULL))
    exps <- setNames(c(colSums(xx), rep(0, 2)), letters[1:5])
    expect_equal(chk, structure(list(xx=exp1, xy=exp2), colsum=exps))
})


test_that("match_cols_sumstats", {
    xx <- matrix(rep(1:3, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    xy <- matrix(1:3, nrow=3, ncol=1, dimnames=list(letters[1:3], NULL))
    ss <- structure(list(xx=xx, xy=xy), "colsum"=colSums(xx))
    chk <- match_cols_sumstats(ss, letters[3:1])
    exp1 <- matrix(rep(3:1, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[3:1], letters[3:1]))
    exp2 <- matrix(3:1, nrow=3, ncol=1, dimnames=list(letters[3:1], NULL))
    exps <- setNames(rev(colSums(xx)), letters[3:1])
    expect_equal(chk, structure(list(xx=exp1, xy=exp2), colsum=exps))
})


test_that("match_sumstats", {
    xx1 <- matrix(rep(1:3, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    xy1 <- matrix(1:3, nrow=3, ncol=1, dimnames=list(letters[1:3], NULL))
    xx2 <- matrix(rep(3:5, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[3:5], letters[3:5]))
    xy2 <- matrix(3:5, nrow=3, ncol=1, dimnames=list(letters[3:5], NULL))
    expx1 <- matrix(c(rep(c(1:3, rep(0, 2)), 3), rep(rep(0, 5), 2)), byrow=TRUE, nrow=5, ncol=5,
                    dimnames=list(letters[1:5], letters[1:5]))
    expy1 <- matrix(c(1:3, rep(0, 2)), nrow=5, ncol=1, dimnames=list(letters[1:5], NULL))
    exps1 <- setNames(c(3,6,9,0,0), letters[1:5])
    expx2 <- matrix(c(rep(rep(0, 5), 2), rep(c(rep(0, 2), 3:5), 3)), byrow=TRUE, nrow=5, ncol=5,
                    dimnames=list(letters[1:5], letters[1:5]))
    expy2 <- matrix(c(rep(0, 2), 3:5), nrow=5, ncol=1, dimnames=list(letters[1:5], NULL))
    exps2 <- setNames(c(0,0,9,12,15), letters[1:5])
    ss1 <- structure(list(xx=xx1, xy=xy1), colsum=colSums(xx1))
    ss2 <- structure(list(xx=xx2, xy=xy2), colsum=colSums(xx2))
    chk <- match_sumstats(list(ss1, ss2))
    expect_equal(chk$sumstats, list(
        structure(list(xx=expx1, xy=expy1), colsum=exps1), 
        structure(list(xx=expx2, xy=expy2), colsum=exps2)
        ))
})


test_that("match_sumstats identical", {
    xx1 <- matrix(rep(1:3, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    xy1 <- matrix(1:3, byrow=TRUE, nrow=3, ncol=1, dimnames=list(letters[1:3], NULL))
    ss1 <- structure(list(xx=xx1, xy=xy1), colsum=colSums(x1))
    ss <- list(ss1, ss1)
    chk <- match_sumstats(ss)
    expect_equal(chk$sumstats, ss)
    expect_equal(chk$incomplete_cols, character())
})


test_that("match_sumstats big", {
    dat <- .example_data(n1=100, n2=50, n3=30, nprs=1000)
    ss1 <- make_sumstats(dat[[1]]$x, dat[[1]]$y)
    ss2 <- make_sumstats(dat[[2]]$x, dat[[2]]$y)
    ss3 <- make_sumstats(dat[[3]]$x, dat[[3]]$y)
    ss <- list(ss1, ss2, ss3)
    chk <- match_sumstats(ss)
    expect_equal(chk$sumstats, ss)
    expect_equal(chk$incomplete_cols, character())
})


test_that("combine_matched_sumstats", {
    dat <- .example_data_diffprs(n1=100, n2=50, nprs1=1000, nprs2=1050)
    ss1 <- make_sumstats(dat[[1]]$x, dat[[1]]$y)
    ss2 <- make_sumstats(dat[[2]]$x, dat[[2]]$y)
    ss <- list(ss1, ss2)
    chk <- combine_sumstats(ss)
    expect_equal(ncol(chk$sumstats$xx), 1054)
    expect_equal(length(chk$incomplete_cols), 50)
    expect_equal(attr(chk$sumstats, "nsubj"), 150)
    expect_equal(attr(chk$sumstats, "nmiss"), attr(ss1, "nmiss") + attr(ss2, "nmiss"))
    expect_equal(attr(chk$sumstats, "nobs"), attr(ss1, "nobs") + attr(ss2, "nobs"))
    expect_equal(attr(chk$sumstats, "colsum")[1:1004], attr(ss1, "colsum")[1:1004] + attr(ss2, "colsum")[1:1004])
    expect_equal(attr(chk$sumstats, "colsum")[1005:1054], attr(ss2, "colsum")[1005:1054])
    expect_equal(attr(chk$sumstats, "ysum"), attr(ss1, "ysum") + attr(ss2, "ysum"))
    # not this is not true because yssq was set to yssq - ysum^2/nobs
    #expect_equal(attr(chk$sumstats, "yssq"), attr(ss1, "yssq") + attr(ss2, "yssq"))
})


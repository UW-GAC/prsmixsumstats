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
    ss <- combine_sumstats(list(ss1, ss2, ss3))
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
})


test_that("add_cols_square_matrix", {
  x <- matrix(1, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
  col_names <- c("d", "e")
  chk <- add_cols_square_matrix(x, col_names)
  exp <- matrix(c(rep(c(rep(1, 3), rep(0, 2)), 3), rep(rep(0, 5), 2)), byrow=TRUE, nrow=5, ncol=5,
                 dimnames=list(letters[1:5], letters[1:5]))
  expect_equal(chk, exp)
})


test_that("add_cols_sumstats", {
    x <- matrix(1, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    col_names <- c("d", "e")
    ss <- list(xx=x, xy=setNames(1:3, letters[1:3]))
    chk <- add_cols_sumstats(ss, col_names)
    exp1 <- matrix(c(rep(c(rep(1, 3), rep(0, 2)), 3), rep(rep(0, 5), 2)), byrow=TRUE, nrow=5, ncol=5,
                  dimnames=list(letters[1:5], letters[1:5]))
    exp2 <- setNames(c(1:3, rep(0, 2)), letters[1:5])
    expect_equal(chk, list(xx=exp1, xy=exp2))
    match_cols_sumstats(ss, c("c", "b", "a"))
})


test_that("match_cols_sumstats", {
    x <- matrix(rep(1:3, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    y <- setNames(1:3, letters[1:3])
    ss <- list(xx=x, xy=y)
    chk <- match_cols_sumstats(ss, letters[3:1])
    exp1 <- matrix(rep(3:1, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[3:1], letters[3:1]))
    exp2 <- setNames(3:1, letters[3:1])
    expect_equal(chk, list(xx=exp1, xy=exp2))
})


test_that("match_sumstats", {
    x1 <- matrix(rep(1:3, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
    y1 <- setNames(1:3, letters[1:3])
    x2 <- matrix(rep(3:5, 3), byrow=TRUE, nrow=3, ncol=3, dimnames=list(letters[3:5], letters[3:5]))
    y2 <- setNames(3:5, letters[3:5])
    expx1 <- matrix(c(rep(c(1:3, rep(0, 2)), 3), rep(rep(0, 5), 2)), byrow=TRUE, nrow=5, ncol=5,
                    dimnames=list(letters[1:5], letters[1:5]))
    expy1 <- setNames(c(1:3, rep(0, 2)), letters[1:5])
    expx2 <- matrix(c(rep(rep(0, 5), 2), rep(c(rep(0, 2), 3:5), 3)), byrow=TRUE, nrow=5, ncol=5,
                    dimnames=list(letters[1:5], letters[1:5]))
    expy2 <- setNames(c(rep(0, 2), 3:5), letters[1:5])
    chk <- match_sumstats(list(list(xx=x1, xy=y1), list(xx=x2, xy=y2)))
    expect_equal(chk, list(list(xx=expx1, xy=expy1), list(xx=expx2, xy=expy2)))
})




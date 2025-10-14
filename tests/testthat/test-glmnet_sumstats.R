test_that("glmnet_sumstats", {
    dat <- .example_data(n1=100, n2=50, n3=30, nprs=1000)
    ss <- make_sumstats(dat[[1]]$x, dat[[1]]$y)
    fit_sumstats <-  glmnet_sumstats(ss, alpha=0.5, lambda=0.5, 
                                     maxiter=10, tol=1e-7, verbose=FALSE)
    expect_equal(length(fit_sumstats$beta), 1004)
    expect_equal(names(fit_sumstats$beta), colnames(ss$xx))
})

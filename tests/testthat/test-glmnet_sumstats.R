test_that("glmnet_sumstats", {
    dat <- .example_data(n1=100, n2=50, n3=30, nprs=1000)
    ss <- make_sumstats(dat[[1]]$x, dat[[1]]$y)
    fit_sumstats <-  glmnet_sumstats(ss, alpha=0.5, lambda=0.5, 
                                     maxiter=10, tol=1e-7, 
                                     beta_threshold=1e-4, verbose=FALSE)
    expect_equal(length(fit_sumstats$beta), 1004)
    expect_equal(names(fit_sumstats$beta), colnames(ss$xx))
    chk <- fit_sumstats$beta[abs(fit_sumstats$beta) > 0]
    expect_true(all(abs(chk) >= 1e-4))
})


test_that("ensemble", {
    dat <- .example_data(n1=100, n2=50, n3=30, nprs=1000)
    ss <- make_sumstats(dat[[1]]$x, dat[[1]]$y)
    ssc <- combine_sumstats(list(ss), scale=TRUE)
    fit_sumstats <-  glmnet_sumstats(ssc$sumstats, alpha=0.5, lambda=0.5, 
                                     maxiter=10, tol=1e-7, 
                                     beta_threshold=1e-4, verbose=FALSE)
    is_pgs <- grepl("^PRS", names(fit_sumstats$beta))
    fit_effects <- pgs_ensemble_sumstats(ssc$sumstats, beta = fit_sumstats$beta,  
                                         trait_type = "binary", 
                                         index_pgs = which(is_pgs), 
                                         index_covar = which(!is_pgs))
    expect_equal(names(fit_effects$coefficients),
                 c("PGS_ensemble", "age", "sex", "cov1", "cov2"))
})

## create test data of 3 groups with PRS (nprs) and covariates (sex, age, cov1, cov2)

## simulate training data

.example_data <- function(n1=1000, n2=500, n3=50, nprs=4000) {
  dat1 <- sim_test_dat(n1, nprs, prev=.1, beta.sd=2)
  dat2 <- sim_test_dat(n2, nprs, prev=.1, beta.sd=2)
  dat3 <- sim_test_dat(n3, nprs, prev=.1, beta.sd=2)

  ## make some y's missing
  dat1$y[3:20] <- NA
  dat2$y[15:30] <- NA
  dat3$y[1:16] <- NA

  ## make some x's missing
  dat1$x[3:5, 4:5] <- NA
  dat2$x[20:30, 5:8] <- NA
  dat3$x[2:10, 6:9] <- NA

  return(list(dat1, dat2, dat3))
}


.example_cluster_data <- function(n=100, nprs=1000) {
    dat <- sim_test_dat(n, nprs)
    id <- as.character(1:n)
    trait <- data.frame(id, pheno=dat$y)
    covariates <- cbind(id, as.data.frame(dat$x[,1:4]))
    scores <- cbind(id, as.data.frame(dat$x[,5:(4+nprs)]))
    clusters <- data.frame(id, cluster=c(rep(1,ceiling(n/2)), rep(2,floor(n/2))))
    return(list(trait=trait[1:(n-10),], covariates=covariates[11:n,], scores=scores, clusters=clusters))
}

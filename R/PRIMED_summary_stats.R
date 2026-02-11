#' Make summary statistics
#'
#' From input design matrix x and outcome vector y, compute x'x and x'y
#'
#' important: x and y must have the same number of subjects
#' and ordered the same way according to subject ID, but ID
#' should not be a column of X
#'
#' returns a sumstats object, which contains two list elements:
#' 1. matrix of x'x where x is nxp design matrix
#' 2. vector of x'y where y is nx1 trait vector
#' attributes of the sumstats object: nsubj, nmiss, nobs, colsum, ysum, yssq
#'
#' @param x design matrix with rows as individual observations
#' @param y vector with outcomes, length should match nrow(x)
#' @param center boolean for whether to center and scale x and y
#' @return sumstats object with elements xx and xy
#' @export
make_sumstats <- function(x, y, center=TRUE){
  ## important: x and y must have the same number of subjects
  ## and ordered the same way according to subject ID, but ID
  ## should not be a column of X

  ## create sum stats xx and xy
  xcol.names <-  colnames(x)
  names.miss <- xcol.names == "" | is.null(xcol.names)
  if(any(names.miss)){
    stop("col names of x are missing")
  }

  nsubj <- nrow(x)
  is.miss.x <- rowSums(is.na(x)) > 0
  is.miss.y <-  is.na(y)
  is.miss <- is.miss.x | is.miss.y
  nmiss <- sum(is.miss)
  nobs <- nsubj - nmiss

  x <- x[!is.miss, , drop=FALSE]
  csum <- colSums(x)
  if (center) {
    x <- scale(x, scale=FALSE)
  }
  #xx <- (t(x) %*% x)
  xx <- crossprod(x)
  rownames(xx) <- xcol.names
  colnames(xx) <- xcol.names

  y <- y[!is.miss]
  ysum <- sum(y, na.rm=TRUE)
  if (center) {
    y <- scale(y, scale=FALSE)
  }
  #xy <- t(x) %*% y
  xy <- crossprod(x, y)
  #names(xy) <-  xcol.names

  yssq <- sum(y^2, na.rm=TRUE)

  ss <- new_sumstats(xx, xy, nsubj, nmiss, nobs, csum, ysum, yssq, centered=center)

  return(ss)
}




#' Compute elastic net from summary statistics
#' @description
#' Fit an elastic net model based on summary statistics for specified
#' penalty parameters alpha and lambda
#'
#' @param sumstats object with elements xx: matrix of x'x where x is nxp design matrix, xy: vector of x'y where y is nx1 trait vector
#' @param beta_init is px1 vector of starting values
#' @param alpha (range 0-1) is fraction of penalty for L1 penalty
#' and (1-alpha) is fraction for L2 penalty
#' @param lambda is penalty parameter
#' @param penalty_factor is px1 vector for weighting penalties of design matrix columns.
#' Typically used as value of 0 for terms not to be penalized and 1 for terms to penalize
#' @param maxiter is maximum number of cyclic coordinate descent iterations.
#' One iteration is over all p parameters.
#' @param tol is tolerance to check convergence
#' @param verbose prints summary results at each iteration if verbose=TRUE
#' @details
#' Cyclic coordinate descent is used to fit an elastic net model based on minimizing penalized least squared.
#' @returns list with fitted beta, number of iterations, convergence (true/false), and input penalty parameters
#' alpha and lambda
#' @author Dan Schaid (schaid@mayo.edu)
#' @export
#'
#'
glmnet_sumstats <- function(sumstats, beta_init = NULL, alpha, lambda, penalty_factor = NULL,
                               maxiter=500, tol=1e-5, verbose=FALSE){
  ## temp skip validate_sumstats(sumstats)
  xx <- sumstats$xx
  xy <- sumstats$xy


  compute_loss <- function(beta, xx, xy, alpha,lambda_pen){
    ## assume y'y = 1 and divide squared loss by 2
    loss <-  .5 + .5 * t(beta) %*% xx %*% beta - t(beta) %*% xy + alpha*sum(lambda_pen * abs(beta)) +
      ((1-alpha)/2)*sum(lambda_pen*beta^2)
    return(loss)
  }


  np <- ncol(xx)
  if(is.null(penalty_factor)) penalty_factor <- rep(1, np)
  lambda_pen <- lambda * penalty_factor
  if(is.null(beta_init)){
    beta_init <- rep(0, np)
  }

  stopifnot(length(beta_init) == ncol(xx))
  stopifnot(length(penalty_factor) == ncol(xx))

  converge <- FALSE
  beta_curr <- beta_init
  loss_curr <-  compute_loss(beta_curr, xx, xy, alpha,lambda_pen)


  ## iterate over active set
  for(iter in 1:maxiter){

    ## KKT test for zero entries

    grad <- abs(xy - xx %*% beta_curr)
    active <- grad > alpha * lambda_pen

    loss_old <- loss_curr
    beta_old <- beta_curr

    for(j in 1:np){
      if(!active[j]) next

      numer <- xy[j] - xx[j, ] %*% beta_curr + xx[j,j] * beta_curr[j]
      denom <- xx[j,j] + lambda_pen[j] * (1 - alpha)
      beta_curr[j] <- soft(numer, alpha * lambda_pen[j]) / denom

    }

    loss_curr <-  compute_loss(beta_curr, xx, xy, alpha,lambda_pen)

    loss_rel_change <- abs(loss_curr - loss_old) / (abs(loss_old) + tol)
    beta_change <- max(abs(beta_curr - beta_old))

    if (verbose) {
      cat("iter =", iter, ", loss_curr =", loss_curr, ", n active = ", sum(active), ", n non-zero beta = ", sum(abs(beta_curr) > 1e-6), ", beta range =", range(beta_curr), "\n")
    }
    if (loss_rel_change < tol || beta_change < tol) {
      converge <- TRUE
      break
    }
  }


  names(beta_curr) <- colnames(xx)
  # filter beta on threshold (default 1e-6) - set to zero, don't remove from vector
  return(list(beta=beta_curr, loss=loss_curr, iter=iter, converge=converge, alpha=alpha, lambda=lambda))
}


soft <- function(x, gamma){
  if(x > gamma){
    s <- x - gamma
  } else if (x < -gamma){
    s <- x + gamma
  }else{
    s <- 0
  }
  return(s)
}



#' calculate AUC from glmnet_sumstats results
#' @param beta beta from glmnet_sumstats
#' @param xx X'X matrix that went into glmnet_sumstats
#' @param vary variance of Y
#' @param ncase number of cases
#' @param ncont number of controls
#' @return list with AUC and R2
#' @importFrom stats pnorm
#' @export
auc_glmnet_sumstats <- function(beta, xx, vary, ncase, ncont){
  ssr <- t(beta) %*% xx %*% beta
  sst <- vary #* nsubj
  r2 <- ssr/sst
  a <- (ncase + ncont)^2/(ncase * ncont)
  d <- sqrt(a*r2)/sqrt(1-r2)
  auc <- pnorm(d/sqrt(2))
  return(list(auc=auc, r2=r2))
}


#' Simulate example data
#' @param nsubj number of subjects
#' @param nprs number of PRS
#' @param prev prev
#' @param beta.sd beta.sd
#' @param seed seed
#' @importFrom stats rbinom rnorm runif
#' @export
sim_test_dat <- function(nsubj, nprs, prev=.1, beta.sd=2, seed=42){
  set.seed(seed)
 ## large beta.sd allows larger beta's
  sex <-  rbinom(nsubj,size=1,prob=.5)
  age <- round(runif(nsubj, min=40, max=70),0)
  cov1 <- rnorm(nsubj)
  cov2 <- rnorm(nsubj)
  x <- cbind(age, sex, cov1, cov2, matrix(rnorm(nsubj*nprs), nrow=nsubj))
  beta <- rnorm(nprs+4, sd=beta.sd)
  xb <- x %*% beta + log(prev/(1-prev))
  p <- exp(xb) / (1 + exp(xb))
  y <- as.vector(1*(runif(nsubj) <= p))
  colnames(x) <- c("age","sex","cov1","cov2", paste0("PRS00", 1:nprs))
  return(list(y=y, x=x))
}


#' Fit glmnet over grid of alpha and lambda values from summary statistics
#' @param sumstats list with items xx, xy
#' @param alpha_grid vector of alpha values (0-1)
#' @param lambda_frac vector of lambda fractions (0-1)
#' @param penalty_factor vector of penalty factors
#' @param maxiter maximum number of iterations for glmnet_sumstats
#' @param tol tolerance for glmnet_sumstats
#' @param verbose print progress if TRUE
#' @return matrix (nalpha x nlambda) of glmnet_sumstats results (lists)
#' @export
glmnet_sumstats_grid <- function(sumstats, alpha_grid, lambda_frac, penalty_factor, maxiter=500, tol=1e-6, verbose=FALSE){


  nalpha <- length(alpha_grid)
  nlambda <- length(lambda_frac)
  fit_grid <- matrix(list(), nrow=nalpha, ncol=nlambda)

  beta_zero <- rep(0, ncol(sumstats$xx))
  max_xy <- max(abs(sumstats$xy))

  time_begin <-  proc.time()
  for(i in 1:nalpha){
    alpha <- alpha_grid[i]
    lambda_max <- max_xy/alpha

    for(j in 1:nlambda){

      lambda <- lambda_frac[j]*lambda_max

      if(verbose)  cat("================ alpha = ", alpha, ", lambda j = ",j, ", lambda = ", lambda, " ==================\n")

      if(i==1 & j==1){
        beta_init <- beta_zero
      } else if (i > 1 & j == 1){
        beta_init <- fit_grid[[i-1,j]]$beta   ## use warm start for next fit
      } else {
        beta_init <- fit_grid[[i,j-1]]$beta   ## use warm start for next fit
      }

      ptm <- proc.time()



      fit_grid[[i,j]] <-  glmnet_sumstats(sumstats, beta_init, alpha=alpha, lambda=lambda, penalty_factor,  maxiter=maxiter, tol=tol, verbose=verbose)
      if(verbose) print(proc.time() - ptm)
    }
  }

  if(verbose) print(proc.time() - time_begin)
  return(fit_grid)
}


#' calculate metrics for summary stats
#' assumes yvar = 1
#' @param sumstats list with items xx, xy
#' @param fit_grid output of glmnet_sumstats
#' @return list of auc, loss, nbeta
#' @export
metrics_sumstats <- function(sumstats, fit_grid){
  ncase <- attr(sumstats, "ysum")
  nobs <- attr(sumstats, "nobs")
  ncont <- nobs - ncase

  nalpha <- nrow(fit_grid)
  nlambda <- ncol(fit_grid)

  auc <- bic <- nbeta <- loss_ssq <- pen_func <- matrix(0, nalpha, nlambda)
  for(i in 1:nalpha){
    for(j in 1:nlambda){
      alpha <- fit_grid[[i,j]]$alpha
      lambda <- fit_grid[[i,j]]$lambda
      beta <- as.vector(fit_grid[[i,j]]$beta)
      tmp <- auc_glmnet_sumstats(beta, sumstats$xx, vary=1, ncase, ncont)
      auc[i,j] <- tmp$auc
      ## assume y'y = 1 and divide squared loss by 2
      loss_ssq[i,j] <-  .5 + .5 * t(beta) %*% sumstats$xx %*% beta - t(beta) %*% sumstats$xy
      nbeta[i,j] <- sum( abs(fit_grid[[i,j]]$beta) > 1e-6)
      bic[i,j] <- nobs * log(loss_ssq[i,j]) + nbeta[i,j]*log(nobs)
      pen_func[i,j] <- loss_ssq[i,j]  + alpha*sum(fit_grid$penalty_factor*lambda * abs(beta)) +
        ((1-alpha)/2)*sum(fit_grid$penalty_factor * lambda * beta^2)
    }
  }
  min_bic <- min(bic)
  bic_min_index <- which(bic == min_bic, arr.ind = TRUE)

  min_loss <- min(loss_ssq)
  loss_min_index <- which(loss_ssq ==  min_loss, arr.ind = TRUE)
  return(list(auc=auc, bic=bic, loss_ssq=loss_ssq, pen_func=pen_func,
              nbeta=nbeta, bic_min_index = bic_min_index, loss_min_index = loss_min_index))

}


#' convert matrix indices to vector index
#' @param row_i row index
#' @param col_j column index
#' @param nrow number of rows in matrix
#' @return vector index
#' @export
index_mat_to_vec <- function(row_i,col_j, nrow){
  k <- (col_j -1)* nrow + row_i
  return(k)
}

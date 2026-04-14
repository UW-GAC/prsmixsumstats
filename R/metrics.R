
#' calculate R2 from glmnet_sumstats results
#' @param beta beta from glmnet_sumstats
#' @param xx X'X matrix that went into glmnet_sumstats
#' @param xy X'y matrix that went into glmnet_sumstats
#' @param eps threshold to determine 0
#' @return r2
#' @export
r2_glmnet_sumstats <- function(beta, xx, xy, eps=1e-8){
  ## assume x and y are scaled, vary = 1
  
  r2_den <- t(beta) %*% xx %*% beta
  xy_beta <- t(beta) %*% xy
  r2_num <- (xy_beta)^2
  if(abs(r2_den) < eps){
    r2 <- 0
  } else {
    r2 <- r2_num / r2_den
  }
  
  return(r2)
}


#' calculate AUC from glmnet_sumstats results
#' @param r2 model R2
#' @param ncase number of cases
#' @param ncont number of controls
#' @return AUC
#' @importFrom stats pnorm
#' @export
auc_glmnet_sumstats <- function(r2, ncase, ncont){
  a <- (ncase + ncont)^2/(ncase * ncont)
  d <- sqrt(a*r2)/sqrt(1-r2)
  auc <- pnorm(d/sqrt(2))
  return(auc)
}

#' calculate metrics for summary stats
#' assumes yvar = 1
#' @param sumstats list with items xx, xy
#' @param fit_grid output glmnet_sumstats_grid
#' @param penalty_factor used in glmnet_sumstats_grid
#' @param trait_type "binary" for case/control, "quant" for quantitative
#' @return list of auc, loss, pen_func,  nbeta, bic_min_index
#' @export
metrics_sumstats <- function(sumstats, fit_grid, penalty_factor, trait_type){
  nobs <- attr(sumstats, "nobs")
  if(trait_type == "binary"){
    ncase <- attr(sumstats, "ysum")
    ncont <- nobs - ncase
  }
  
  nalpha <- nrow(fit_grid)
  nlambda <- ncol(fit_grid)
  
  r2 <- auc <- bic <- nbeta <- loss_ssq <- pen_func <- matrix(NA, nalpha, nlambda)
  for(i in 1:nalpha){
    for(j in 1:nlambda){
      alpha <- fit_grid[[i,j]]$alpha
      lambda <- fit_grid[[i,j]]$lambda
      beta <- as.vector(fit_grid[[i,j]]$beta)
      r2[i,j] <- r2_glmnet_sumstats(beta, sumstats$xx, sumstats$xy)
      if(trait_type == "binary"){
        auc [i,j] <- auc_glmnet_sumstats(r2[i,j], ncase, ncont)
      }
      
      ## assume y'y = 1 and divide squared loss by 2
      loss_ssq[i,j] <-  .5 + .5 * t(beta) %*% sumstats$xx %*% beta - t(beta) %*% sumstats$xy
      nbeta[i,j] <- sum( abs(fit_grid[[i,j]]$beta) > 1e-6)
      bic[i,j] <- nobs * log(loss_ssq[i,j]) + nbeta[i,j]*log(nobs)
      pen_func[i,j] <- loss_ssq[i,j]  + alpha*sum(penalty_factor*lambda * abs(beta)) +
        ((1-alpha)/2)*sum(penalty_factor * lambda * beta^2)
    }
  }
  min_bic <- min(bic)
  bic_min_index <- which(bic == min_bic, arr.ind = TRUE)
  
  min_loss <- min(loss_ssq)
  loss_min_index <- which(loss_ssq ==  min_loss, arr.ind = TRUE)
  
  result <- list(auc=auc, r2=r2, bic=bic, loss_ssq=loss_ssq, pen_func=pen_func,
                 nbeta=nbeta, bic_min_index = bic_min_index, loss_min_index = loss_min_index)
  
  return(result)
  
}

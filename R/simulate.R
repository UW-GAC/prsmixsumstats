
#' create variance matrix
#' @param sumstats list with items xx, xy, and vary
#' @export
make_var_matrix <- function(sumstats){
    ## create var mat for validation by simulations
    xx <- sumstats$xx
    xy <- sumstats$xy
    vary <- sumstats$vary
    v12 <- c(vary, xy)
    vmat <- rbind(v12, cbind(xy, xx))
    return(vmat)
}



#' simulate y and X matrix from covariance
#' @param vmat variance matrix
#' @param nsim number of rows
#' @export
#' @importFrom rWishart rSingularWishart
sim_sumstats <- function(vmat, nsim){
    svd_decomp <- svd(vmat)
    df  <- sum(svd_decomp$d > 1e-6)
    wishart_sim <- rSingularWishart(n=nsim, df=df, vmat)
    for(isim in 1:nsim){
        ## divide by df because simulated variance
        ## matrix has expectation df * vmat
        wishart_sim[,,isim] <- wishart_sim[,,isim]/df
    }
    return(wishart_sim)
}



#' simulate metrics
#' @param wishart_sim result of \code{\link{sim_sumstats}}
#' @param fit_grid 2-dimensional grid of fits. Each fit is a list 
#' and all fits arranged as matrix (of lists)
#' @param vary variance of y
#' @export
eval_sim <- function(wishart_sim, fit_grid, vary){
    ## compute loss metrics of fit_grid applied to 
    ## simulated var matrices
    nsim <- dim(wishart_sim)[3]
    penalty_dim <- dim(fit_grid)
    nalpha <- penalty_dim[1]
    nlambda <- penalty_dim[2]
    loss_tot <- loss_ssq <- matrix(0, nalpha, nlambda)
    
    for(isim in 1:nsim){
        vmat_sim <- wishart_sim[,,isim]
        yvar_sim <- vmat_sim[1,1]
        xy_sim <- vmat_sim[-1,1]
        xx_sim <- vmat_sim[-1,-1]
        sdx_sim <- sqrt(diag(xx_sim))
        xx_sim <- xx_sim / (sdx_sim %o% sdx_sim)
        xy_sim <- xy_sim * sqrt(vary/yvar_sim)
        yvar_sim <- yvar_sim/yvar_sim
        
        for (i in 1:nalpha) {
            for(j in 1:nlambda){
                ## node need to subscript matrix with [[]] because items in matrix are lists
                beta <- as.vector(fit_grid[[i,j]]$beta)
                loss_temp <- yvar_sim - 2*t(beta) %*% xy_sim +  t(beta) %*% xx_sim %*% beta
                loss_tot[i,j] <- loss_tot[i,j] + loss_temp
                loss_ssq[i,j] <- loss_ssq[i,j] + loss_temp^2
            }
        }
    }
    
    loss_mean <- loss_tot/nsim
    loss_var <-  (loss_ssq - loss_tot^2/nsim)/ (nsim-1)
    loss_var <- ifelse(loss_var < 0, 0, loss_var)
    loss_sd <- ifelse(loss_var > 0, sqrt(loss_var), 0)
    
    
    return(list(loss_mean=loss_mean, loss_sd=loss_sd))
}


 
loss_indices <- function(loss_mean, loss_sd){
    min_loss <- min(loss_mean)
    loss_min_index <- which(loss_mean ==  min_loss, arr.ind = TRUE)
    min_loss_sd <- loss_sd[which.min(loss_mean)]
    threshold <- min_loss + min_loss_sd
    within_1sd <- which(loss_mean <= threshold)
    loss_1sd_index <- which(loss_mean ==  max(loss_mean[within_1sd]), arr.ind=TRUE)
    return(list(loss_min_index=loss_min_index,loss_1sd_index=loss_1sd_index))
}


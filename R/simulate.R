
#' create variance matrix
#' @param xx X'X matrix
#' @param xy X'Y matrix
#' @param yvar variance of y
#' @export
make_var_matrix <- function(xx, xy, yvar){
    ## create var mat for later simulations
    v12 <- c(yvar, xy)
    vmat <- rbind(v12, cbind(xy, xx))
    return(vmat)
}


#' simulate y and X matrix from covariance
#' matrix:
#'        | var(y),  cov(y,x) |
#' vmat = | cov(x,y) cov(x,x) |
#' and return matrix [Y|X]
#' @param n number of rows
#' @param vmat variance matrix
#' @return matrix [Y|X]
#' @export
simulate_yx <- function(n, vmat) {
    p <- ncol(vmat)
    svd_decomp <- svd(vmat)
    U <- svd_decomp$u
    D <- diag(sqrt(pmax(svd_decomp$d, 0)))
    Z <- matrix(rnorm(n * p), nrow = n)
    return(Z %*% U %*% D)
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
#' @param yvar variance of y
#' @export
metrics_sim <- function(wishart_sim, fit_grid, yvar){
    ## compute metrics of fit_grid applied to 
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
        xy_sim <- xy_sim * sqrt(yvar/yvar_sim)
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
    loss_sd <- sqrt(loss_var)
    
    ## select the alpha/lambda within one standard error of the minimum loss
    min_loss <- min(loss_mean)
    min_loss_se <- loss_sd[which.min(loss_mean)]
    threshold <- min_loss + min_loss_se
    
    # Find all alphas/lambdas within 1 SD of minimum MSE
    within_1se <- which(loss_mean <= threshold)
    ## choose best that has max loss within the 1 SD
    index_best <- which(max(loss_mean[within_1se]) == loss_mean)
    ## index_best is index for a vector after loss_mean is converted
    ## to a vector. Since this conversion is by column-major, we
    ## can convert index_best to the corresponding row/col of 
    ## the loss_mean, and hence to the row/col of the grid of
    ## alpha/lambda to determine which alpha/lambda give the 
    ## best fit within 1 SD of minimum MSE
    
    index_lambda <- floor((index_best-1)/nalpha) + 1
    index_alpha <- ((index_best-1) %%  nalpha) + 1
    
    return(list(loss_mean=loss_mean, loss_sd=loss_sd, index_best=c(index_alpha, index_lambda, index_long=index_best)))
}

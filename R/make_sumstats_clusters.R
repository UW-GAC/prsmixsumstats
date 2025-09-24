#' Make summary stats for each cluster
#' 
#' Make summary stats for each cluster
#'
#' Each input is expected to be a data table where the first column is sample ID.
#' Scores with all missing values will be dropped with a message.
#' Covariates with all missing values will be dropped with a warning.
#' After applying the above filters, rows with any missing values will be dropped before
#' calculating the summary statistics.
#' 
#' this function writes an output file with a sumstats object for:
#' 1. all samples together
#' 2. each cluster with size > min_cluster_size
#' 
#' @param trait data.frame with two columns: sample ID, outcome
#' @param covariates data.frame where first column is sample ID and other columns are numeric covariates
#' @param scores data.frame where first column is sample ID and other columns are scores
#' @param clusters data.frame with two columns: sample ID, cluster
#' @param trait_name name of trait, used to name output file
#' @param cohort_name name of cohort, used to name output file
#' @param min_cluster_size minimum size for a cluster to have an output file
#' @export
make_sumstats_clusters <- function(trait, covariates, scores, clusters, trait_name, cohort_name,
                                   min_cluster_size = 20) {
    
  # filter any covariates that are all missing
  miss <- sapply(covariates, function(x) all(is.na(x)))
  dropped <- names(covariates)[miss]
  covariates <- covariates[,!miss]
  if (length(dropped) > 0) {
      warning("dropped the following covariates as all values are missing: ", paste(dropped, collapse=", "))
  }
  
  # filter any scores that are all missing
  miss <- sapply(scores, function(x) all(is.na(x)))
  dropped <- names(scores)[miss]
  scores <- scores[,!miss]
  if (length(dropped) > 0) {
      message("dropped the following scores as all values are missing: ", paste(dropped, collapse=", "))
  }
  
  # filter any rows with missing data
  is.miss <- apply(is.na(trait), 1, any)
  trait <- trait[!is.miss,,drop=FALSE]
  is.miss <- apply(is.na(covariates), 1, any)
  covariates <- covariates[!is.miss,,drop=FALSE]
  is.miss <- apply(is.na(scores), 1, any)
  scores <- scores[!is.miss,,drop=FALSE]
  
  # check for missing data in clusters
  if (any(is.na(clusters))) {
      warning("missing data in clusters")
  }
    
  ids <- intersect(trait[[1]], covariates[[1]])
  ids <- intersect(ids, scores[[1]])
  ids <- intersect(ids, clusters[[1]])
  trait <- trait[match(ids, trait[[1]]),]
  covariates <- covariates[match(ids, covariates[[1]]),]
  scores <- scores[match(ids, scores[[1]]),]
  clusters <- clusters[match(ids, clusters[[1]]),]
  stopifnot(all(trait[[1]] == covariates[[1]]))
  stopifnot(all(trait[[1]] == scores[[1]]))
  stopifnot(all(trait[[1]] == clusters[[1]]))
  message(nrow(trait), " observations with complete data")
  
  cov_scores <- cbind(covariates, scores[,-1])
  rm(scores)
  rm(covariates)
  
  res <- make_sumstats(x=as.matrix(cov_scores[,-1]), y=unlist(trait[,-1]))
  saveRDS(res, paste0(trait_name, "_", cohort_name, "_sumstats.rds"))

  cluster_names <- unique(clusters[[2]])
  for (c in cluster_names) {
    index <- which(clusters[[2]] %in% c)
    if (length(index) > min_cluster_size) {
        res <- make_sumstats(x=as.matrix(cov_scores[index,-1]), y=unlist(trait[index,-1]))
        saveRDS(res, paste0(trait_name, "_", cohort_name, "_cluster", c, "_sumstats.rds"))
    }
  }
}

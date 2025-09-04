#' make summary stats for each cluster

#' each input is expected to be a data table where the first column is sample ID
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

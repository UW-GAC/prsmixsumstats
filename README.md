# prsmixsumstats

[![R-CMD-check](https://github.com/UW-GAC/prsmixsumstats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UW-GAC/prsmixsumstats/actions/workflows/R-CMD-check.yaml)

R package to generate summary statistics for PRSmix elastic net

This package defines a `sumstats` R object. These objects are created by the `make_sumstats` function, which
takes two inputs: a design matrix X and an outcome vector Y. `make_sumstats` returns a `sumstats` object, 
which is a list with two elements; X'X and X'Y. Attributes provide additional information: `nsubj` 
(number of rows of X), `nobs` (observations with no missing values), `nmiss` (observations with missing values,
which are dropped before computation of summary stats), `colsum` (sum over columns of X), `ysum` (sum over Y, 
this is the number of cases for binary outcomes), `yssq` (sum of squares of Y), `centered` (boolean for 
whether the summary stats were centered on the mean).

`make_sumstats_clusters` is a convenience function that performs matching on its inputs by sample_id 
(assumed to be the first column of all input data frames), removes missing values, and calls
`make_sumstats` on all samples together and by cluster (defined in the `clusters` input).

`combine_sumstats` combines multiple `sumstats` objects. The resulting object is centered and scaled.

`glmnet_sumstats` runs an elastic net analysis on a `sumstats` object.

`sim_sumstats` and `eval_sim` simulate replicates of the training data and use the output of multiple calls to 
`glmnet_sumstats` to find the model that minimizes the loss.

More details on the methods implemented in this package can be found [here](https://docs.google.com/document/d/14ZW6sfuHlH0YooK7AUw9UHS8oXhHmJYv/edit?usp=sharing&ouid=107443797655395020525&rtpof=true&sd=true).

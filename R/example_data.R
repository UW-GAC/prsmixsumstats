set.seed(42)
ids <- paste0("A", 1001:1010)
n <- length(ids)

trait <- data.frame(
    sample_id = ids,
    CAD_mod = rbinom(n, 1, 0.3)
)

age <- runif(n, 50, 70)
covariates <- data.frame(
    sample_id = ids,
    age = sprintf("%.2f", age),
    age2 = sprintf("%.2f", age^2),
    sex = rbinom(n, 1, 0.5) + 1,
    smoking_ever = rep(0, n),
    smoking_never = rep(0, n),
    BMI = sprintf("%.2f", runif(n, 20, 40)),
    LDL = sprintf("%.0f", runif(n, 50, 150)),
    HDL = sprintf("%.0f", runif(n, 40, 80)),
    SBP = sprintf("%.0f", runif(n, 100, 140)),
    DBP = sprintf("%.0f", runif(n, 60, 90))
)
for (i in 1:35) {
    covariates[[paste0("PC", i)]] <- signif(runif(n, -5, 5), digits=4)
}

write.table(trait, "inst/extdata/example_trait.tsv", sep="\t", row.names=FALSE, quote=FALSE)
write.table(covariates, "inst/extdata/example_covariates.tsv", sep="\t", row.names=FALSE, quote=FALSE)

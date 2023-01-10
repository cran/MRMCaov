## ---- setup, include=FALSE----------------------------------------------------
options(
  prompt = 'R> ',
  continue = '+ ',
  fig.width = 7,
  fig.height = 4,
  fig.align = "center"
)

print_data <- function(x) {
  res <- head(x, n = 20)
  print(res)
  nmore <- nrow(x) - nrow(res)
  if (nmore) cat("... with", nmore, "more rows\n")
}

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("MRMCaov")

## ----citation, comment = ""---------------------------------------------------
## Text format
citation("MRMCaov")

## Bibtex format
toBibtex(citation("MRMCaov"))

## ----using_example_data-------------------------------------------------------
## Load MRMCaov library and VanDyke dataset
library(MRMCaov)
data(VanDyke, package = "MRMCaov")

## ----echo=FALSE---------------------------------------------------------------
print_data(VanDyke)

## ----using_mrmc_roc-----------------------------------------------------------
## Compare ROC AUC treatment means for the VanDyke example
est <- mrmc(
  binormal_auc(truth, rating), treatment, reader, case, data = VanDyke
)

## -----------------------------------------------------------------------------
print(est)

## -----------------------------------------------------------------------------
summary(est)

## -----------------------------------------------------------------------------
plot(est)

## -----------------------------------------------------------------------------
print(parameters(est))

## ----using_mrmc_binary--------------------------------------------------------
## Compare sensitivity for binary classification
VanDyke$binary_rating <- VanDyke$rating >= 3
est <- mrmc(
  binary_sens(truth, binary_rating), treatment, reader, case, data = VanDyke
)

## -----------------------------------------------------------------------------
print(est)

## -----------------------------------------------------------------------------
summary(est)

## ----using_mrmc_DeLong--------------------------------------------------------
## DeLong method
est <- mrmc(
  empirical_auc(truth, rating), treatment, reader, case, data = VanDyke,
  cov = DeLong
)

## -----------------------------------------------------------------------------
summary(est)

## ----using_mrmc_unbiased------------------------------------------------------
## Unbiased method
est <- mrmc(
  empirical_auc(truth, rating), treatment, reader, case, data = VanDyke,
  cov = unbiased
)

## -----------------------------------------------------------------------------
summary(est)

## ----using_mrmc_fixed_readers-------------------------------------------------
## Fixed readers
est <- mrmc(
  empirical_auc(truth, rating), treatment, fixed(reader), case, data = VanDyke
)

## -----------------------------------------------------------------------------
summary(est)

## ----using_mrmc_fixed_cases---------------------------------------------------
## Fixed cases
est <- mrmc(
  empirical_auc(truth, rating), treatment, reader, fixed(case), data = VanDyke
)

## -----------------------------------------------------------------------------
summary(est)

## ----echo=FALSE---------------------------------------------------------------
cat("Case identifier codings for factorial and nested study designs\n")
x <- t(VanDyke[c("reader", "treatment", "case", "case2", "case3")])
dimnames(x) <- list(
  Factor = rownames(x),
  Observation = seq_len(ncol(x))
)
n <- 30
print(x[, seq_len(n)], quote = FALSE)
cat("... with", ncol(x) - n, "more observations")

## ----using_mrmc_within_readers------------------------------------------------
## Cases nested within readers
est <- mrmc(
  empirical_auc(truth, rating), treatment, reader, case2, data = VanDyke
)

## -----------------------------------------------------------------------------
summary(est)

## ----using_mrmc_within_tests--------------------------------------------------
## Cases nested within tests
est <- mrmc(
  empirical_auc(truth, rating), treatment, reader, case3, data = VanDyke
)

## -----------------------------------------------------------------------------
summary(est)

## ----using_srmc_roc-----------------------------------------------------------
## Subset VanDyke dataset by reader 1
VanDyke1 <- subset(VanDyke, reader == "1")

## Compare ROC AUC treatment means for reader 1
est <- srmc(binormal_auc(truth, rating), treatment, case, data = VanDyke1)

## -----------------------------------------------------------------------------
print(est)

## -----------------------------------------------------------------------------
plot(est)

## -----------------------------------------------------------------------------
print(parameters(est))

## -----------------------------------------------------------------------------
summary(est)

## ----using_trmc_roc-----------------------------------------------------------
## Subset VanDyke dataset by treatment 1 and reader 1
VanDyke11 <- subset(VanDyke, treatment == "1" & reader == "1")

## Estimate ROC AUC for treatment 1 and reader 1
est <- stmc(binormal_auc(truth, rating), case, data = VanDyke11)

## -----------------------------------------------------------------------------
plot(est)

## -----------------------------------------------------------------------------
print(parameters(est))

## -----------------------------------------------------------------------------
summary(est)

## ----using_curves_one---------------------------------------------------------
## Direct referencing of data frame columns
# curve <- roc_curves(VanDyke$truth, VanDyke$rating)

## Indirect referencing using the with function
curve <- with(VanDyke, {
  roc_curves(truth, rating)
})
plot(curve)

## ----using_curves_reader------------------------------------------------------
## Grouped by reader
curves <- with(VanDyke, {
  roc_curves(truth, rating,
             groups = list(Reader = reader, Treatment = treatment))
})
plot(curves)

## ----using_curves_treatment---------------------------------------------------
## Grouped by treatment
curves <- with(VanDyke, {
  roc_curves(truth, rating,
             groups = list(Treatment = treatment, Reader = reader))
})
plot(curves)

## ----using_curves_binorm------------------------------------------------------
## Binormal curves
curves_binorm <- with(VanDyke, {
  roc_curves(truth, rating,
             groups = list(Treatment = treatment, Reader = reader),
             method = "binormal")
})
params_binorm <- parameters(curves_binorm)
print(params_binorm)
plot(curves_binorm)

## ----using_curves_binormLR----------------------------------------------------
## Binormal likelihood-ratio curves
curves_binormLR <- with(VanDyke, {
  roc_curves(truth, rating,
             groups = list(Treatment = treatment, Reader = reader),
             method = "binormalLR")
})
params_binormLR <- parameters(curves_binormLR)
print(params_binormLR)
plot(curves_binormLR)

## ----using_curves_points------------------------------------------------------
## Extract points at given specificities
curve_spec_pts <- points(curves, metric = "spec", values = c(0.5, 0.7, 0.9))
print(curve_spec_pts)
plot(curve_spec_pts, coord_fixed = FALSE)

## Extract points at given sensitivities
curve_sens_pts <- points(curves, metric = "sens", values = c(0.5, 0.7, 0.9))
print(curve_sens_pts)
plot(curve_sens_pts, coord_fixed = FALSE)

## ----using_curves_mean_spec---------------------------------------------------
## Average sensitivities at given specificities (default)
curves_mean <- mean(curves)
print(curves_mean)
plot(curves_mean)

## ----using_curves_mean_sens---------------------------------------------------
## Average specificities at given sensitivities
curves_mean <- mean(curves, metric = "sens")
print(curves_mean)
plot(curves_mean)

## ----using_metrics_roc--------------------------------------------------------
## Total area under the empirical ROC curve
empirical_auc(VanDyke$truth, VanDyke$rating)

## Partial area for specificity from 0.7 to 1.0
empirical_auc(VanDyke$truth, VanDyke$rating, partial = "spec", min = 0.70, max = 1.0)

## Partial area for sensitivity from 0.7 to 1.0
empirical_auc(VanDyke$truth, VanDyke$rating, partial = "sens", min = 0.70, max = 1.0)

## Sensitivity for given specificity
empirical_sens(VanDyke$truth, VanDyke$rating, spec = 0.8)

## Sensitivity for given specificity
empirical_spec(VanDyke$truth, VanDyke$rating, sens = 0.8)

## ----using_metrics_binary-----------------------------------------------------
## Create binary classification
VanDyke$binary_rating <- VanDyke$rating >= 3

## Sensitivity
binary_sens(VanDyke$truth, VanDyke$binary_rating)

## Specificity
binary_spec(VanDyke$truth, VanDyke$binary_rating)


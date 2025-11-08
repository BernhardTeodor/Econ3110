library(dplyr)
library(caret)
library(haven)
library(GenericML)

set.seed(3110)

alloutcomescontrols <- read_dta("Baseline/alloutcomescontrols.dta")

main <- read_dta("Followup/Income.dta") %>%
  left_join(
    alloutcomescontrols[, c("rescode", "followup", "edusec", "enterprise")],
    by = c("rescode", "followup")
  ) %>%
  mutate(age16 = age16m + age16f) %>%
  select(
    c(
      "indiv", "enterprise", "rescode", "entownership", "nrents", "followup",
      "treatment", "treated", "loanbaseline", "eduvoc", "edusec", "age16", "under16",
      "marrcohab", "age", "agesq", "buddhist", "hah1", "augb", "sepf", "novf",
      "aimag", "aimagcenter"
    )
  ) %>%
  filter(complete.cases(.) & indiv != 1 & followup == 1)

D <- as.numeric(main$treated)

Z <- as.matrix(main[, c(
  "loanbaseline", "eduvoc", "edusec", "age16", "under16", "marrcohab",
  "age", "agesq", "buddhist", "hah1", "augb", "sepf", "novf", "aimag", "aimagcenter"
)])

Y <- as.numeric(main$enterprise)

learners <- c(
  "lasso",
  "mlr3::lrn('ranger', num.trees = 1000, mtry = 15)",
  "mlr3::lrn('kknn')"
)


# Kvantilgrenser for GATES/CLAN
quantilecutoffs <- c(0.25, 0.5, 0.75)

# Differanser for GATES og CLAN
diffGATES <- setup_diff(subtract_from = "most",
                        subtracted = c(1, 2, 3))
diffCLAN <- setup_diff(subtract_from = "least",
                       subtracted = c(3, 2))

# Kjør GenericML
x <- GenericML(
  Z, D, Y, learners,
  num_splits = 200,
  quantile_cutoffs = quantilecutoffs,
  diff_GATES = diffGATES,
  diff_CLAN = diffCLAN,
  significance_level = 0.025,
  parallel = FALSE
)

# Skriv ut CLAN-estimatene for hver kovariat
for (var in c(
  "loanbaseline", "eduvoc", "edusec", "age16", "under16",
  "marrcohab", "age", "agesq", "buddhist", "hah1", "augb", "sepf", "novf",
  "aimag", "aimagcenter"
)) {
  print(var)
  print(get_CLAN(x, variable = var, plot = FALSE)[[1]]$estimate[1:4])
}

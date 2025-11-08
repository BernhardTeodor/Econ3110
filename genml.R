library(dplyr)
library(caret)
library(haven)
library(kknn)
library(GenericML)

set.seed(3110)

alloutcomescontrols <- read_dta("data/Baseline/all_outcomes_controls.dta")

main <- read_dta("data/Followup/Income.dta") %>%
  left_join(
    alloutcomescontrols[, c("rescode", "followup", "edusec", "enterprise")],
    by = c("rescode", "followup")
  ) %>%
  mutate(age16 = age16m + age16f) %>%
  select(
    c(
      "indiv", "enterprise", "rescode", "ent_ownership", "nrents", "followup",
      "treatment", "treated", "loan_baseline", "eduvoc", "edusec", "age16", "under16",
      "marr_cohab", "age", "age_sq", "buddhist", "hahl", "aug_b", "sep_f", "nov_f",
      "aimag", "aimagcenter"
    )
  ) %>%
  filter(complete.cases(.) & indiv != 1 & followup == 1)

D <- as.numeric(main$treated)

Z <- as.matrix(main[, c(
  "loan_baseline", "eduvoc", "edusec", "age16", "under16", "marr_cohab",
  "age", "age_sq", "buddhist", "hahl", "aug_b", "sep_f", "nov_f", "aimag", "aimagcenter"
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
  "loan_baseline", "eduvoc", "edusec", "age16", "under16",
  "marr_cohab", "age", "age_sq", "buddhist", "hahl", "aug_b", "sep_f", "nov_f",
  "aimag", "aimagcenter"
)) {
  print(var)
  
  svar <- get_CLAN(x, variable = var, plot = FALSE)[1]$estimate[1:4]
  print(svar)
}




df <- read_dta("data/Combined_allwaves_final.dta") # Leser data 

df <- df |> 
  mutate(miss_age_PAP = ifelse(is.na(age_med_BL), 1, 0)) |> 
  mutate(age_med_BL_control= ifelse(is.na(age_med_BL), 0, 1))|> 
  mutate(miss_household_size= ifelse(is.na(household_size), 1, 0))|> 
  mutate(household_size_control= ifelse(is.na(household_size), 0, household_size)) |> 
  mutate(miss_edu_category= ifelse(is.na(edu_nohs_BL), 1, 0))|> 
  mutate(edu_nohs_BL_control= ifelse(is.na(edu_nohs_BL), 0, edu_nohs_BL))|> 
  mutate(married_control= ifelse(is.na(married), 0, married))|> 
  mutate(divorced_separated_control= ifelse(is.na(divorced_separated), 0,
                                            divorced_separated))|> 
  mutate(single_control= ifelse(is.na(single), 0, single))|> 
  mutate(widowed_control= ifelse(is.na(widowed), 0, widowed))|> 
  mutate(miss_relationship= ifelse(is.na(rel_status_BL), 1, 0))|> 
  mutate(miss_cars= ifelse(is.na(cars), 1, 0))|> 
  mutate(one_car_control= ifelse(is.na(one_car), 0, one_car))|> 
  mutate(mult_cars_control= ifelse(is.na(mult_cars), 0, mult_cars))|> 
  mutate(miss_LF_BL= ifelse(is.na(LF_BL), 1, 0))|> 
  mutate(LF_BL_control= ifelse(is.na(LF_BL), 0, LF_BL)) |> 
  filter(endline_start_w3==1)





kovariater <- c("age_med_BL_control", "miss_age_PAP", "edu_nohs_BL_control",
  "miss_edu_category", "married_control", "single_control", "widowed_control",
  "miss_relationship", "household_size_control", "miss_household_size", "one_car_control",
  "miss_cars", "LF_BL_control", "miss_LF_BL")




data <- df[,c(kovariater, "license_w3", "treatment")] |> na.omit()



T_z <- as.matrix(data[, kovariater]) 

treat <- data$treatment |> as.numeric()

T_y <- data$license_w3 |> as.numeric() 


learners <- c(
  "lasso",
  "mlr3::lrn('ranger', num.trees = 10, mtry = 3)",
  "mlr3::lrn('kknn')"
)

#learners <- c("lasso", "mlr3::lrn('ranger', num.trees = 10)")



quantilecutoffs <- c(0.25, 0.5, 0.75)




diffGATES <- setup_diff(subtract_from = "most",
                        subtracted = c(1, 2, 3))
diffCLAN <- setup_diff(subtract_from = "least",
                       subtracted = c(3, 2))


x <- GenericML(
  T_z, treat, T_y, learners,
  num_splits = 2,
  quantile_cutoffs = quantilecutoffs,
  diff_GATES = diffGATES,
  diff_CLAN = diffCLAN,
  significance_level = 0.025,
  parallel = FALSE
)


for (var in kovariater)
{
  print(var)
  
  svar <- get_CLAN(x, variable = var, plot = FALSE)[1]$estimate[1:4]
  print(svar)
  print("")
  
}




get_GATES(x, plot = TRUE)




plot(x, type = "CLAN", CLAN_variable = "married_control",
     groups = c('G1','G2','G3','G4'))

plot(x, type = "CLAN", CLAN_variable = "one_car_control",
     groups = c('G1','G2','G3','G4'))



get_best(x)

































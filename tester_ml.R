

library(GenericML)

### 1. Data Generation ----
set.seed(31684591)

n  <- 1000                      # number of observations
p  <- 3                         # number of covariates
D  <- rbinom(n, 1, 0.5)         # random treatment assignment
Z  <- matrix(runif(n*p), n, p)  # design matrix
colnames(Z) <- paste0("z", 1:p) # column names
Y0 <- as.numeric(Z %*% rexp(p)) # potential outcome without treatment



Z |> rexp(3) #### Z blir antallet obs med rexp,3 er da mean length(/Z) = 3000, 
#matruse med 3000 obs(1000*3) med my 3Ender opp med 1000 obs med my 4 dvs mean = 1/my


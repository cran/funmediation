## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("funmediation")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("dziakj1/funmediation")

## -----------------------------------------------------------------------------
library(tvem)
library(refund)
library(boot)
library(funmediation)

## -----------------------------------------------------------------------------
set.seed(123)
simulation1 <- simulate_funmediation_example(nsub=500)

## -----------------------------------------------------------------------------
str(simulation1)

## -----------------------------------------------------------------------------
the_data <- simulation1$dataset

## -----------------------------------------------------------------------------
print(head(the_data))
summary(the_data)

## -----------------------------------------------------------------------------
model1 <- funmediation(data=the_data,
                             treatment=X,
                             mediator=M,
                             outcome=Y,
                             id=subject_id,
                             time=t,
                             nboot=10)	

## -----------------------------------------------------------------------------
print(model1)

## -----------------------------------------------------------------------------
plot(model1, what_plot="tvem")

## -----------------------------------------------------------------------------
plot(model1, what_plot="pfr")

## -----------------------------------------------------------------------------
plot(model1, what_plot="pfrgam")

## -----------------------------------------------------------------------------
plot(model1, what_plot="coef")

## -----------------------------------------------------------------------------
simulation_with_covariate <- simulate_funmediation_example(nsub=500,
                                             make_covariate_S=TRUE);
data_with_covariate <- simulation_with_covariate$dataset; 

## -----------------------------------------------------------------------------
model_with_tve_covariate <- funmediation(data=data_with_covariate,
                       treatment=X,
                       mediator=M,
                       outcome=Y,
                       tve_covariates_on_mediator = ~S,
                       covariates_on_outcome = ~S,  
                       id=subject_id,
                       time=t,
                       nboot=10)

## -----------------------------------------------------------------------------
print(model_with_tve_covariate)
plot(model_with_tve_covariate, what_plot="tvem")

## -----------------------------------------------------------------------------
model_with_tie_covariate <- funmediation(data=data_with_covariate,
                                          treatment=X,
                                          mediator=M,
                                          outcome=Y,
                                          tie_covariates_on_mediator = ~S,
                                          covariates_on_outcome = ~S,  
                                          id=subject_id,
                                          time=t,
                                          nboot=10)
print(model_with_tie_covariate)
plot(model_with_tie_covariate, what_plot="pfr")

## -----------------------------------------------------------------------------
data_with_covariate$binary_M <- 1*(data_with_covariate$M > mean(data_with_covariate$M))
head(data_with_covariate)

## -----------------------------------------------------------------------------
model_with_binary_M <- funmediation(data=data_with_covariate,
                             treatment=X,
                             mediator=binary_M,
                             outcome=Y,
                             id=subject_id,
                             time=t,
                             binary_mediator=TRUE,
                             nboot=10)

## -----------------------------------------------------------------------------
print(model_with_binary_M)
plot(model_with_binary_M, what_plot="coef")

## -----------------------------------------------------------------------------
simulation_with_binary_Y <- simulate_funmediation_example(simulate_binary_Y=TRUE, 
                                                          nsub=500)
data_with_binary_Y <- simulation_with_binary_Y$dataset
head(data_with_binary_Y)

## -----------------------------------------------------------------------------
model_with_binary_Y <- funmediation(data=data_with_binary_Y,
                                    treatment=X,
                                    mediator=M,
                                    outcome=Y,
                                    id=subject_id,
                                    time=t,
                                    binary_outcome=TRUE,
                                    nboot=10)

## -----------------------------------------------------------------------------
print(model_with_binary_Y)
plot(model_with_binary_Y, what_plot="coef")

## -----------------------------------------------------------------------------
set.seed(12345)
nboot <- 99
answers <- NULL
f1 <- function(t) {return(-(t/2)^.5)}
f2 <- function(t) {return(sqrt(t))}
f3 <- function(t) {return(sin(2*3.141593*t))}
the_simulation <- simulate_funmediation_example(nlevels=3,
                          alpha_X = list(f1,f2),
                          beta_M = f3,
                          beta_X = c(.2,.3))

## -----------------------------------------------------------------------------
head(the_simulation$dataset)

## -----------------------------------------------------------------------------
the_data <- the_simulation$dataset
ans_funmed_3groups <- funmediation(data=the_data,
             treatment=~X1+X2,
             mediator=M,
             outcome=Y,
             tvem_num_knots=5,
             id=subject_id,
             time=t,
             nboot=10)

## -----------------------------------------------------------------------------
print(ans_funmed_3groups)

## -----------------------------------------------------------------------------
plot(ans_funmed_3groups, use_panes=TRUE, what_plot = "pfr")

## -----------------------------------------------------------------------------
plot(ans_funmed_3groups, use_panes=TRUE, what_plot = "tvem")

## -----------------------------------------------------------------------------
plot(ans_funmed_3groups, use_panes=TRUE, what_plot = "coefs")


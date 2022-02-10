library(causalToolbox)
library(tidyverse, reshape)
library(dplyr)
library(ggplot2, reshape)


#data specific
library(foreign)
library(pander)
library(plyr)
library(sandwich)
library(lmtest)
library(data.table)
library(ggplot2)
library(reshape2)
library(glmnet)

library(parallel)
cores <- detectCores(all.tests = FALSE, logical = TRUE)

set.seed(5953921)

data <- read.dta('transphobia.rda')
data = transphobia
trans.tolerance.dvs.t0 <- c('therm_trans_t0', 'gender_norms_sexchange_t0',
                            'gender_norms_moral_t0', 'gender_norms_abnormal_t0')

trans.tolerance.dvs.t1 <- c('therm_trans_t1', 'gender_norm_sexchange_t1',
                            'gender_norm_moral_t1', 'gender_norm_abnormal_t1',
                            'gender_norm_trans_moral_wrong_t1')

trans.tolerance.dvs.t2 <- c('therm_trans_t2', 'gender_norm_sexchange_t2',
                            'gender_norm_moral_t2', 'gender_norm_abnormal_t2',
                            'gender_norm_trans_moral_wrong_t2',
                            'trans_teacher_t2', 'trans_bathroom_t2')

trans.tolerance.dvs.t3 <- c('therm_trans_t3', 'gender_norm_sexchange_t3',
                            'gender_norm_moral_t3', 'gender_norm_abnormal_t3',
                            'gender_norm_trans_moral_wrong_t3',
                            'trans_teacher_t3', 'trans_bathroom_t3')

trans.tolerance.dvs.t4 <- c('therm_trans_t4', 'gender_norm_sexchange_t4',
                            'gender_norm_moral_t4', 'gender_norm_abnormal_t4',
                            'gender_norm_trans_moral_wrong_t4',
                            'trans_teacher_t4', 'trans_bathroom_t4')

trans.law.dvs.t0 <- c('miami_trans_law_t0', 'miami_trans_law2_t0')
trans.law.dvs.t1 <- c('miami_trans_law_t1', 'miami_trans_law2_t1')
trans.law.dvs.t2 <- c('miami_trans_law_t2', 'miami_trans_law2_t2')
# Note: Beginning with t3, the definition was added.
trans.law.dvs.t3 <- c('miami_trans_law_withdef_t3', 'miami_trans_law2_withdef_t3')
trans.law.dvs.t4 <- c('miami_trans_law_withdef_t4', 'miami_trans_law2_withdef_t4')

#### Secondary Outcome: Gender Non-Conformity Index

gender.nonconformity.t0 <- c('gender_norm_looks_t0', 'gender_norm_rights_t0')
gender.nonconformity.t1 <- c('gender_norm_looks_t1', 'gender_norm_rights_t1')
# Note: Beginning with t2, an additional item was added to the measure.
gender.nonconformity.t2 <- c('gender_norm_looks_t2', 'gender_norm_rights_t2',
                             'gender_norm_dress_t2')
gender.nonconformity.t3 <- c('gender_norm_looks_t3', 'gender_norm_rights_t3',
                             'gender_norm_dress_t3')
gender.nonconformity.t4 <- c('gender_norm_looks_t4', 'gender_norm_rights_t4',
                             'gender_norm_dress_t4')

### Reverse Coded Items

reverse.coded.items <- c('gender_norms_moral_t0', 'gender_norm_moral_t1',
                         'gender_norm_moral_t2', 'gender_norm_moral_t3',
                         'gender_norm_moral_t4', 'gender_norms_abnormal_t0',
                         'gender_norm_abnormal_t1', 'gender_norm_abnormal_t2',
                         'gender_norm_abnormal_t3','gender_norm_abnormal_t4',
                         'gender_norm_trans_moral_wrong_t1',
                         'gender_norm_trans_moral_wrong_t2',
                         'gender_norm_trans_moral_wrong_t3',
                         'gender_norm_trans_moral_wrong_t4',
                         'trans_bathroom_t2', 'trans_bathroom_t3',
                         'trans_bathroom_t4', 'gender_norm_looks_t0',
                         'gender_norm_looks_t1', 'gender_norm_looks_t2',
                         'gender_norm_looks_t3', 'gender_norm_looks_t4',
                         'gender_norm_rights_t0', 'gender_norm_rights_t1',
                         'gender_norm_rights_t2', 'gender_norm_rights_t3',
                         'gender_norm_rights_t4', 'gender_norm_dress_t2',
                         'gender_norm_dress_t3', 'gender_norm_dress_t4')
for(item in reverse.coded.items) data[,item] <- -1 * data[,item]


### Procedue for Combining Outcomes into Indices

# We pre-specified the procedure below to calculate each index. In the interest of transparency we provide the full code.
# Note that we code all indicies such that higher values on the indices indicate more tolerance and success of the intervention.

# Compute factor analysis outcome
compute.factor.dv <- function(dv.names, respondent.booleans, print.loadings = TRUE){
  responders <- data[respondent.booleans,]

  # Factor analysis
  factor.obj <- princomp(responders[, dv.names], cor=TRUE)
  if(print.loadings) print(loadings(factor.obj))
  dv <- as.vector(factor.obj$scores[,1])

  # More positive values on the factor should indicate more tolerance; reverse otherwise.
  if(cor(dv, responders$miami_trans_law_t0, use="complete.obs") < 0) dv <- -1 * dv

  # Put in the order of the main data frame
  dv.in.order <- dv[match(data$id, responders$id)]

  # Rescale to mean 0 sd 1 in placebo group; treatment effects can then be interpreted
  # as the effect in standard deviations the treatment would have among an untreated
  # population.
  dv.in.order <- (dv.in.order - mean(dv.in.order[!data$treat_ind], na.rm=TRUE)) /
    sd(dv.in.order[!data$treat_ind], na.rm=TRUE)

  return(as.vector(dv.in.order))
}

all.dv.names.t1 <- c('miami_trans_law_t1', 'miami_trans_law2_t1', 'therm_trans_t1',
                     'gender_norm_sexchange_t1', 'gender_norm_moral_t1',
                     'gender_norm_abnormal_t1', 'gender_norm_trans_moral_wrong_t1')

all.dv.names.t2 <- c('miami_trans_law_t2', 'miami_trans_law2_t2', 'therm_trans_t2',
                     'gender_norm_sexchange_t2', 'gender_norm_moral_t2',
                     'gender_norm_abnormal_t2', 'gender_norm_trans_moral_wrong_t2')

all.dv.names.t3 <- c('miami_trans_law_withdef_t3', 'miami_trans_law2_withdef_t3',
                     'therm_trans_t3', 'gender_norm_sexchange_t3', 'gender_norm_moral_t3',
                     'gender_norm_abnormal_t3','gender_norm_trans_moral_wrong_t3')

all.dv.names.t4 <- c('miami_trans_law_withdef_t4', 'miami_trans_law2_withdef_t4',
                     'therm_trans_t4', 'gender_norm_sexchange_t4', 'gender_norm_moral_t4',
                     'gender_norm_abnormal_t4', 'gender_norm_trans_moral_wrong_t4')


trans.tolerance.dvs.t0 <- c('therm_trans_t0', 'gender_norms_sexchange_t0',
                            'gender_norms_moral_t0', 'gender_norms_abnormal_t0')

trans.tolerance.dvs.t1 <- c('therm_trans_t1', 'gender_norm_sexchange_t1',
                            'gender_norm_moral_t1', 'gender_norm_abnormal_t1',
                            'gender_norm_trans_moral_wrong_t1')

trans.tolerance.dvs.t2 <- c('therm_trans_t2', 'gender_norm_sexchange_t2',
                            'gender_norm_moral_t2', 'gender_norm_abnormal_t2',
                            'gender_norm_trans_moral_wrong_t2',
                            'trans_teacher_t2', 'trans_bathroom_t2')

trans.tolerance.dvs.t3 <- c('therm_trans_t3', 'gender_norm_sexchange_t3',
                            'gender_norm_moral_t3', 'gender_norm_abnormal_t3',
                            'gender_norm_trans_moral_wrong_t3',
                            'trans_teacher_t3', 'trans_bathroom_t3')

trans.tolerance.dvs.t4 <- c('therm_trans_t4', 'gender_norm_sexchange_t4',
                            'gender_norm_moral_t4', 'gender_norm_abnormal_t4',
                            'gender_norm_trans_moral_wrong_t4',
                            'trans_teacher_t4', 'trans_bathroom_t4')

trans.law.dvs.t0 <- c('miami_trans_law_t0', 'miami_trans_law2_t0')
trans.law.dvs.t1 <- c('miami_trans_law_t1', 'miami_trans_law2_t1')
trans.law.dvs.t2 <- c('miami_trans_law_t2', 'miami_trans_law2_t2')
# Note: Beginning with t3, the definition was added.
trans.law.dvs.t3 <- c('miami_trans_law_withdef_t3', 'miami_trans_law2_withdef_t3')
trans.law.dvs.t4 <- c('miami_trans_law_withdef_t4', 'miami_trans_law2_withdef_t4')

# In this code section we implement the procedures describe previously.

# First, misc. housekeeping.
# Recode age for small number of observations where it is missing.
data$vf_age[which(is.na(data$vf_age))] <- mean(data$vf_age, na.rm=TRUE)

# Language of interview
data$survey_language_es[is.na(data$survey_language_es)] <-
  data$survey_language_t0[is.na(data$survey_language_es)] == "ES"
data$survey_language_es[is.na(data$survey_language_es)] <- mean(data$survey_language_es, na.rm = TRUE)

# We subset to only those who came to door. contacted = came to door.
full.data <- data
data <- subset(data, contacted == 1)

# Compute the DVs in line with the above procedures.

# Omnibus DV of all primary outcomes.
data$all.dvs.t1 <- compute.factor.dv(all.dv.names.t1, data$respondent_t1==1 & !is.na(data$respondent_t1))
data$all.dvs.t2 <- compute.factor.dv(all.dv.names.t2, data$respondent_t2==1 & !is.na(data$respondent_t2))
data$all.dvs.t3 <- compute.factor.dv(all.dv.names.t3, data$respondent_t3==1 & !is.na(data$respondent_t3))
data$all.dvs.t4 <- compute.factor.dv(all.dv.names.t4, data$respondent_t4==1 & !is.na(data$respondent_t4))

# Trans tolerance DV.
data$trans.tolerance.dv.t0 <- compute.factor.dv(trans.tolerance.dvs.t0, data$respondent_t0==1 & !is.na(data$respondent_t0))
data$trans.tolerance.dv.t1 <- compute.factor.dv(trans.tolerance.dvs.t1, data$respondent_t1==1 & !is.na(data$respondent_t1))
data$trans.tolerance.dv.t2 <- compute.factor.dv(trans.tolerance.dvs.t2, data$respondent_t2==1 & !is.na(data$respondent_t2))
data$trans.tolerance.dv.t3 <- compute.factor.dv(trans.tolerance.dvs.t3, data$respondent_t3==1 & !is.na(data$respondent_t3))
data$trans.tolerance.dv.t4 <- compute.factor.dv(trans.tolerance.dvs.t4, data$respondent_t4==1 & !is.na(data$respondent_t4))

# Law DV.
# Create outcome scale by averaging over the two questions.
data$miami_trans_law_t0_avg <- (data$miami_trans_law_t0 + data$miami_trans_law2_t0)/2
data$miami_trans_law_t1_avg <- (data$miami_trans_law_t1 + data$miami_trans_law2_t1)/2
data$miami_trans_law_t2_avg <- (data$miami_trans_law_t2 + data$miami_trans_law2_t2)/2
# Note: Beginning with t3, the definition was added.
data$miami_trans_law_t3_avg <- (data$miami_trans_law_withdef_t3 +
                                  data$miami_trans_law2_withdef_t3)/2
# Note: Only one question was asked in t3 after the ad was shown, so no averaging is required.
data$miami_trans_law_t4_avg <- (data$miami_trans_law_withdef_t4 +
                                  data$miami_trans_law2_withdef_t4)/2

# Gender Non-Conformity DV
data$gender_nonconformity_t0 <- compute.factor.dv(gender.nonconformity.t0, data$respondent_t0==1 & !is.na(data$respondent_t0))
data$gender_nonconformity_t1 <- compute.factor.dv(gender.nonconformity.t1, data$respondent_t1==1 & !is.na(data$respondent_t1))
data$gender_nonconformity_t2 <- compute.factor.dv(gender.nonconformity.t2, data$respondent_t2==1 & !is.na(data$respondent_t2))
data$gender_nonconformity_t3 <- compute.factor.dv(gender.nonconformity.t3, data$respondent_t3==1 & !is.na(data$respondent_t3))
data$gender_nonconformity_t4 <- compute.factor.dv(gender.nonconformity.t4, data$respondent_t4==1 & !is.na(data$respondent_t4))


## Estimation Procedures

### Contact Rate

# This dummy records whether the intervention was actually delivered vs. was not for any reason.
# Note that we do not use this variable to conduct comparisons only to measure successful contact rates.
data$treatment.delivered <- data$exp_actual_convo == "Trans-Equality" & !is.na(data$canvass_trans_ratingstart)


### Complier Average Causal Effect Estimation


t0.covariate.names <- c('miami_trans_law_t0', 'miami_trans_law2_t0', 'therm_trans_t0',
                        'gender_norms_sexchange_t0', 'gender_norms_moral_t0', 'gender_norms_abnormal_t0',
                        'ssm_t0', 'therm_obama_t0', 'therm_gay_t0','vf_democrat', 'ideology_t0',
                        'religious_t0', 'exposure_gay_t0', 'exposure_trans_t0', 'pid_t0', 'sdo_scale',
                        'gender_norm_daugher_t0', 'gender_norm_looks_t0',
                        'gender_norm_rights_t0', 'therm_afams_t0', 'vf_female', 'vf_hispanic',
                        'vf_black', 'vf_age', 'survey_language_es', 'cluster_level_t0_scale_mean')
x <- data[,c(t0.covariate.names)]
x <- as.matrix(x, dimnames = list(NULL, names(x)))

# Function to compute clustered standard errors, from Mahmood Arai.
cl <- function(fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm), 2, function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

# Function to extract the average treatment effect from OLS with clustered SEs.
est.ate <- function(dv, include.obs = NULL, include.covariates = TRUE){
  if(is.null(include.obs)){
    include.obs <- !is.na(dv)
  }
  include.obs <- which(include.obs & !is.na(dv))

  if(include.covariates) lm.obj <- lm(dv[include.obs] ~ data$treat_ind[include.obs] +
                                        x[include.obs,])
  if(!include.covariates) lm.obj <- lm(dv[include.obs] ~ data$treat_ind[include.obs])

  # Calculate cluster-robust standard errors.
  result <- cl(lm.obj, data$hh_id[include.obs])[2,]

  # Adjust point estimate and standard error for contact rate in subsample.
  itt_d <- lm(treatment.delivered ~ treat_ind, data[include.obs,])$coefficients[2]
  result[1:2] <- result[1:2] / itt_d

  # Per pre-analysis plan, rejection region is top 4% and bottom 1% of sampling distribution,
  # so p-values are reported for this region; otherwise, we write "n.s."
  # See Olken (*43*) , page 70, footnote 5.
  # Note that cl() returns two-tailed p-values that must be converted to one-tailed.
  # Note that all DVs were recoded such that higher values indicated more tolerance.
  result[4] <- result[4] / 2 # p-value corresponds to mass under one side of distribution.
  rejection.region <- ((result[4] < .04 & result[1] > 0) | # Significant positive result.
                         (result[4] < .01 & result[1] < 0)) # Significant negative result.
  result <- round(result, 3)
  if(rejection.region) result[4] <- paste0(as.character(result[4]), "*")
  if(!rejection.region) result[4] <- "*n.s.*"
  if(result[4] == "0*") result[4] <- "0.000*" # Indicate precision of 0 p-value.
  names(result)[4] <- "*p*"
  return(result)
}

ate <- est.ate(data$all.dvs.t1)
print(ate)

### Heterogenous Treatment Effects

# Residualized Outcome
t1 <- subset(data, !is.na(trans.tolerance.dv.t1))
x.t1 <- t1[,c(t0.covariate.names)]
x.t1 <- as.matrix(x.t1, dimnames = list(NULL, names(x.t1)))
t1$t1.resid <- summary(lm(t1$trans.tolerance.dv.t1 ~ x.t1))$residuals

# Residualize the dependent variable, then transform it per (*44*).
t1.resid <- summary(lm(data$trans.tolerance.dv.t1 ~ x))$residuals
data$t1.resid[as.numeric(names(t1.resid))] <- t1.resid # Maps residuals back into data.
data$transformed.outcome <- with(data, t1.resid * (treat_ind - .5) / .25)

# Vectors describing rows where the outcome was observed.
include.obs <- !is.na(data$transformed.outcome)
x.subset <- cbind(data$treat_ind[include.obs], x[include.obs,])
transformed.outcome <- data$transformed.outcome[include.obs]


Y <- data$trans.tolerance.dv.t1[include.obs]
Tr <- data$treat_ind[include.obs]
X <- as.data.frame(x.subset)

set.seed(5953921)


full_data <- data.frame(X, Tr = Tr, Y = Y)
saveRDS(object = full_data, file = "data/cleaned_transphobia.rds")

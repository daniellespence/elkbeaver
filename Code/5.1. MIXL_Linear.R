# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)
library(plyr)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="mixl_trim_weight",
  mixing    = TRUE,  #
  nCores    = 3, #
  indivID   ="CaseID",
  weights = "weight")


file_name_mixl_estimation = paste0("output/mixl_trim_weight_", ".RData")

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
df <- read.csv("Data/MAINChoices.csv") #%>%
#  mutate(across(starts_with("cost_"), ~ ./ 100))

df$cost_1<-df$cost_1/100
df$cost_2<-df$cost_2/100

#-----------------------------------#
# Add and filter biased responses, V0N
#-----------------------------------#

bias_resp<- read.csv("Data/EBL_bias.csv")
nonVI<-read.csv("Data/non_VI.csv")

database <- left_join(df, bias_resp,  by="CaseID")
database <- left_join(database, nonVI,  by="CaseID")

database <- database%>%
  filter(bias != 1,
         non_VI !=1) ## remove biased responses & V0N

#-----------------------------------#
# Add weights 
#-----------------------------------#

weights<- read.csv("Data/Trimmedweights.csv")

database <- left_join(database, weights, by="CaseID")

#database <- database %>%
# mutate(across(starts_with("cost_"), ~ ./ 100))

database[is.na(database)] <- 0

#-----------------------------------#
# Need to set to data.frame for apollo
#-----------------------------------#
database <- as.data.frame(database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_cost = 0,
                mu_stsquo = 0,
                mu_advisory = 0,
                mu_catch_per_trip = 0,
                mu_wqi = 0,
                mu_proportion = 0,
                mu_weed = 0,
                sigma_stsquo = 0.1,
                sigma_advisory = 0.1,
                sigma_catch_per_trip = 0.1,
                sigma_wqi = 0.1,
                sigma_proportion = 0.1,
                sigma_weed = 0.1)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_stsquo", "draws_advisory","draws_catch_per_trip","draws_wqi","draws_proportion",
                     "draws_weed"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_stsquo"]] = mu_stsquo + sigma_stsquo * draws_stsquo
  randcoeff[["b_advisory"]] = mu_advisory + sigma_advisory * draws_advisory
  randcoeff[["b_catch_per_trip"]] = mu_catch_per_trip + sigma_catch_per_trip * draws_catch_per_trip
  randcoeff[["b_wqi"]] = mu_wqi + sigma_wqi * draws_wqi
  randcoeff[["b_proportion"]] = mu_proportion + sigma_proportion * draws_proportion
  randcoeff[["b_weed"]] = mu_weed + sigma_weed * draws_weed
  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']] = b_cost*cost_1 + b_advisory*advisory_1 + b_catch_per_trip*catch_per_trip_1 +
    b_wqi*wqi_1 + b_proportion*proportion_1+ b_weed*weed_1
  V[['alt2']] = b_cost*cost_2 + b_advisory*advisory_2 + b_catch_per_trip*catch_per_trip_2 +
    b_wqi*wqi_2 + b_proportion*proportion_2 + b_weed*weed_2
  V[['alt3']] = b_stsquo
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = list(alt1=1, alt2=1, alt3=1),
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Add in weights
  P = apollo_weighting(P,
                       apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings=list(hessianRoutine="maxLik"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #
apollo_saveOutput(model)

save(model, file = file_name_mixl_estimation)

load(file_name_mixl_estimation)


#---------------- MWTP

deltaMethod_settings = list(expression = c(SQ ="-mu_stsquo/b_cost*100", adv =  "-mu_advisory/b_cost", fish ="-mu_catch_per_trip/b_cost*100", wqi = "-mu_wqi/b_cost", prop = "-mu_proportion/b_cost", weed="-mu_weed/b_cost"))

apollo_deltaMethod(model, deltaMethod_settings)


ci95 = function(expression, model) {
  M  = apollo_deltaMethod(model, deltaMethod_settings)
  ci = 1.96*M[, "Robust s.e."]
  cat("95% Confidence Interval =", M[,"Value"] - ci, "to ", M[,"Value"] + ci, "\n")
}

ci95(list(expression), model)

  ##------ CS for best-case improvements

deltaMethod_settings = list(expression=c(WTP_cbc = "(mu_advisory/100*(115- 0.68*115) + mu_wqi/100*(47 - 72)) / b_cost*100", WTP_bc = "(mu_advisory/100*(115-0.57*115) + mu_wqi/100*(47-100)) / b_cost*100", WTP_st = "(mu_advisory/100*(115 - 58) + mu_weed/100*(30 - 0.5*30)+ mu_proportion/100*(88 - 82)) / b_cost*100", WTP_lt = "(mu_advisory/100*(115-29) + mu_weed/100*(30 - 6)+ mu_proportion/100*(88 - 67)) / b_cost*100"))

apollo_deltaMethod(model, deltaMethod_settings)


ci95 = function(expression, model) {
  M  = apollo_deltaMethod(model, deltaMethod_settings)
  ci = 1.96*M[, "Robust s.e."]
  cat("95% Confidence Interval =", M[,"Value"] - ci, "to ", M[,"Value"] + ci, "\n")
}

ci95(list(expression), model)


# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)
library(car)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
	modelName ="output/mnl_weight_log",
	modelDescr ="MNL model MAIN weighted_log" ,
	indivID   ="CaseID",
	weights = "weight")

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

df <- read.csv("Data/MAINChoices.csv") %>%
  mutate(across(starts_with("cost_"), ~ ./ 100))

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
# Add weights for trimmed data 
#-----------------------------------#

weights<- read.csv("Data/TrimmedWeights.csv")

database <- left_join(database, weights, by="CaseID")

database[is.na(database)] <- 0


#-----------------------------------#
# Need to set to data.frame for apollo
#-----------------------------------#
database <- as.data.frame(database)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_stsquo = 0,
								b_cost = 0,
								b_advisory = 0,
								b_catch_per_trip = 0,
								b_wqi = 0,
								b_proportion = 0,
								b_weed = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

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
	V[['alt1']] = b_cost*cost_1 + b_advisory*log(advisory_1) + b_catch_per_trip*log(catch_per_trip_1) +
		b_wqi*log(wqi_1) + b_proportion*log(proportion_1)+ b_weed*log(weed_1)
	V[['alt2']] = b_cost*cost_2 + b_advisory*log(advisory_2) + b_catch_per_trip*log(catch_per_trip_2) +
	  b_wqi*log(wqi_2) + b_proportion*log(proportion_2)+ b_weed*log(weed_2)
	V[['alt3']] = b_stsquo + b_cost*cost_3 + b_advisory*log(advisory_3) + b_catch_per_trip*log(catch_per_trip_3) +
	  b_wqi*log(wqi_3) + b_proportion*log(proportion_3)+ b_weed*log(weed_3)
	
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


deltaMethod(model, "-b_stsquo/b_cost*100") 
deltaMethod(model, "-b_weed/b_cost*(1/.5)") 
deltaMethod(model, "-b_proportion/b_cost") 
deltaMethod(model, "-b_wqi/b_cost") 
deltaMethod(model, "-b_catch_per_trip/b_cost*100") 
deltaMethod(model, "-b_advisory/b_cost") 



##------ CS for best-case improvements

deltaMethod(model, "(b_advisory*log(1.15/ 0.78) + b_wqi*log(0.47/0.72)) / b_cost*100",
            func = "1")
#### OR
deltaMethod(model, "(b_advisory/100*log(115/ 78) + b_wqi/100*log(47/72)) / b_cost*100",
            func = "1")

# Oxygenation: ultimate best case scenario (43% reduction in advisory days, all objectives for water quality met [100% CWQI])
deltaMethod(model, "(b_advisory*log(1.15/0.66) + b_wqi*log(0.47/1.00)) / b_cost*100",
            func = "2") 

##------ CS for watershed management plan goals


# more conservative goals: Reduce population/biomass of non-native fish species (82%), Reduce extent of Eurasian milfoil by 50% (15%), Reduce days of blue-green algae advisories by 50% after 3 years (58 days)

deltaMethod(model, "(b_advisory*log(1.15/0.58) + b_weed*log(0.30/0.15)+ b_proportion*log(0.88/0.82)) / b_cost*100",
            func = "3") 

# more conservative goals: Reduce population/biomass of non-native fish species (67%), Reduce extent of Eurasian milfoil by 80% (6%), Reduce days of blue-green algae advisories by 75% after 8 years (29 days)

deltaMethod(model, "(b_advisory*log(1.15/0.29) + b_weed/100*log(0.30/0.6)+ b_proportion*log(0.88/0.67)) / b_cost*100",
            func = "4") 

#wtp_logMNL= rbind(s1, s2, s3, s4)

#write.csv(wtp_logMNL, "output/wtp_logMNL.csv")

################################################################################
### Applying survey weights to full dataset
### EBL CEs
################################################################################

rm(list=ls(all=TRUE))

#-----------------------------------------------------------------------------
## load package and data

library(anesrake)
library(tidyverse)


#-----------------------------------------------------------------------------
## create weights for full dataset
#-----------------------------------------------------------------------------

df <- read_excel("Data/Raw Data/Elk_Beaver_Lake_Completes_Excel.xlsx")
# change variables to read easier

df2<- df%>%
  select(CaseID, SCREENER1, D_MEMBERSHIP, D_GENDER, D_AGE, starts_with(c("D_CULTURE")), 
                 O_D_CULTURE_ETHNICITY_C22, D_LIVING_SITUATION, D_MEMBER_IN_HOUSEHOLD, 
                 D_GRANDCHILDREN, D_EDUCATION, D_EMPLOYMENT, D_INCOME)

df2 = df %>%
mutate(D_AGE = case_when(D_AGE == 1 ~ "age1839",
                      D_AGE == 2 ~ "age1839",
                      D_AGE == 3 ~ "age4054",
                      D_AGE == 4 ~ "age5564",
                      D_AGE == 5 ~ "age6599",
                      TRUE ~ NA_character_))%>%
       mutate(D_EDUCATION = case_when(D_EDUCATION == 1 ~ "HS",
                               D_EDUCATION == 2 ~ "HS",
                               D_EDUCATION == 3 ~ "trade",
                               D_EDUCATION == 4 ~ "some",
                               D_EDUCATION == 5 ~ "bach",
                               D_EDUCATION == 6 ~ "adv",
                               TRUE ~ NA_character_)) %>%
       mutate(D_GENDER= case_when(D_GENDER == 1 ~ "man",
                           D_GENDER == 2 ~ "woman",
                           TRUE ~ NA_character_))%>%
       mutate(D_INCOME = case_when(D_INCOME == 1 ~ ">29",
                                   D_INCOME == 2 ~ "30to49",
                                   D_INCOME == 3 ~ "50to69",
                                   D_INCOME == 4 ~ "70to99",
                                   D_INCOME == 5 ~ "100to149",
                                   D_INCOME == 6 ~ "150+",
                                  TRUE ~ NA_character_))%>%
       mutate(D_EMPLOYMENT = case_when(D_EMPLOYMENT == 1 ~ "unemployed",
                                D_EMPLOYMENT == 2 ~ "PT",
                                D_EMPLOYMENT == 3 ~ "FT",
                                D_EMPLOYMENT == 4 ~ "student",
                                D_EMPLOYMENT == 5 ~ "retired",
                                D_EMPLOYMENT == 6 ~ "homemaker",
                                TRUE ~ NA_character_))%>%
       mutate(SCREENER1 = case_when(SCREENER1 == 1 ~ "V0N",
                               SCREENER1 == 2 ~ "V0S",
                               SCREENER1 == 3 ~ "V8K",
                               SCREENER1 == 4 ~ "V8L",
                               SCREENER1 == 5 ~ "V8M",
                               SCREENER1 == 6 ~ "V8N",
                               SCREENER1 == 7 ~ "V8P",
                               SCREENER1 == 8 ~ "V8R",
                               SCREENER1 == 9 ~ "V8S",
                               SCREENER1 == 10 ~ "V8T",
                               SCREENER1 == 11 ~ "V8V",
                               SCREENER1 == 12 ~ "V8W",
                               SCREENER1 == 13 ~ "V8X",
                               SCREENER1 == 14 ~ "V8Y",
                               SCREENER1 == 15 ~ "V8Z",
                               SCREENER1 == 16 ~ "V9A",
                               SCREENER1 == 17 ~ "V9B",
                               SCREENER1 == 18 ~ "V9C",
                               SCREENER1 == 19 ~ "V9E",
                               SCREENER1 == 20 ~ "V9Z",
                                  TRUE ~ NA_character_))
df2 <- as.data.frame(df2)

df2$D_GENDER<-as.factor(df2$D_GENDER)
df2$D_AGE<-as.factor(df2$D_AGE)
df2$D_INCOME<-as.factor(df2$D_INCOME)
#df2$D_EDUCATION<-as.factor(df2$D_EDUCATION)
#df2$SCREENER1<-as.factor(df2$SCREENER1)

#table(df2['SCREENER1'])
#-----------------------------------------------------------------------------
## Calculate Base weights
## base weights are the number of individuals living in the household
# divided by the average number of individuals living in households in CRD
# and then multiplied by the inverse of the response rate for the region they live in
# not sure about response rate by region. May need to ask Bri if she has that info
#for now, stic with household

#baseweight<- as.numeric(df2$D_MEMBER_IN_HOUSEHOLD)/2.2


#df2<-cbind(baseweight, df2)
#-----------------------------------------------------------------------------
## Create target pop dataframe with variables of interest


agetarget <- c(.33, .23, .18, .26)
names(agetarget)<-c("age1839", "age4054", "age5564", "age6599")

#edutarget <- c(.40, 0.08, 0.22, 0.18, 0.086)
#names(edutarget)<-c("HS", "trade", "some", "bach", "adv" )

gendertarget <- c(0.48, .52)
names(gendertarget)<-c("man", "woman")

incometarget <- c(.16, .17, .15, .18, .18, .13)
names(incometarget)<-c(">29", "30to49", "50to69", "70to99", "100to149", "150+")

#employ <- c(unemployed=0.03, PT=0.296, FT=0.269)


#-----------------------------------------------------------------------------
## Specify targets
targets <- list(agetarget, gendertarget, incometarget)
names(targets)<-c("D_AGE", "D_GENDER", "D_INCOME")

#-----------------------------------------------------------------------------
## Begin rake procedure

outsave<- anesrake(targets, df2, caseid=df$CaseID)

#adding baseweights causes error, thus was removed from line

caseweights <- data.frame(cases=outsave$caseid, weights=outsave$weightvec)

summary(caseweights)

summary(outsave)

df3 <- cbind(outsave$weightvec, df)

df3<-df3%>%
  rename(weight = "outsave$weightvec")

df3<- df3%>% select("CaseID", "weight")

write.csv(df3, row.names = F, "Data/FullWeights.csv")


#-----------------------------------------------------------------------------
## create weights for trimmed dataset
#-----------------------------------------------------------------------------
df4<-transform(df2, CaseID = as.numeric(CaseID))

## Adding and filtering biased responses, V0N
bias_resp<- read.csv("Data/EBL_bias.csv")
nonVI<-read.csv("Data/non_VI.csv")

df4 <- left_join(df4, bias_resp,  by="CaseID")
df4 <- left_join(df4, nonVI,  by="CaseID")

df4 <- df4 %>%
  filter(bias != 1,
         non_VI !=1) ## remove biased responses & V0N
#-----------------------------------------------------------------------------
## Begin rake procedure

outsave<- anesrake(targets, df4, caseid=df4$CaseID)

#adding baseweights causes error, thus was removed from line

caseweights <- data.frame(cases=outsave$caseid, weights=outsave$weightvec)

summary(caseweights)

summary(outsave)

df5 <- cbind(outsave$weightvec, df4)

df5<-df5%>%
  rename(weight = "outsave$weightvec")

df5<- df5%>% select("CaseID", "weight")

write.csv(df5, row.names = F, "Data/TrimmedWeights.csv")

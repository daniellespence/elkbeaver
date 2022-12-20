###############################################################################
############# Data trimming based on Moore et al. (2018) ######################
#############              3/16/2022                     ######################
###############################################################################

rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
library(tidyverse)
library(purrrlyr)

#------------------------------------#
# Import data
#------------------------------------#
library(readxl)
df <- read_excel("Data/Raw Data/Elk_Beaver_Lake_Completes_Excel.xlsx")


#------------------------------------#
# Protest responses
#------------------------------------#

protest <- filter(df, CHOICE_1_C3 == "1",
                      CHOICE_2_C3 == "1",
                      CHOICE_3_C3 == "1",
                      CHOICE_4_C3 == "1",
                      CHOICE_5_C3 == "1",
                      CHOICE_6_C3 == "1")

# 63 people always chose status quo

protest2 <- filter(protest, POST_DCE_3_A3 >=6)
protest3 <- filter(protest2, POST_DCE_3_A5 >=6) 

# n= 8 protests

df2 <- anti_join(df, protest3) # remove protests from data set

#------------------------------------#
# Hypothetical bias
#------------------------------------#

hypo <- filter(df2, CHOICE_1_C3 == "0",
                  CHOICE_2_C3 == "0",
                  CHOICE_3_C3 == "0",
                  CHOICE_4_C3 == "0",
                  CHOICE_5_C3 == "0",
                  CHOICE_6_C3 == "0")

# 376 people always chose policy options

hypo2 <- filter(hypo, POST_DCE_3_A1 == "1")


# n= 2

df3 <- anti_join(df2, hypo2) # remove cases of hypothetical bias from data set

#------------------------------------#
# Scenario rejection
#------------------------------------#

rej <- filter(df3, POST_DCE_3_A2 <=2)
#n=12

df4 <-anti_join(df3, rej)
#------------------------------------#
# Warm glow
#------------------------------------#

warm <- filter(df4, POST_DCE_3_A4 >=6)

warm = df %>%
  select(CaseID, SURVEY_VERSION, POST_DCE_3_A4, starts_with(c("C1_", "C2_", "C3_","C4_", "C5_", "C6_"))) %>%
  #	filter(GROUP == "A1" | GROUP == "B1") %>%
  select(-ends_with("ORDER")) %>%
  pivot_longer(-c(SURVEY_VERSION,CaseID,  POST_DCE_3_A4), names_to = "type", values_to = "value",
               values_transform = list(value = as.character)) %>%
  separate(type, into = c("question", "type"), sep = 3) %>%
  filter(type != "CHOICE_SITUATION") %>%
  separate(type, into = c("attribute", "alt"), sep = -2) %>%
  mutate(question = parse_number(question),
         alt = parse_number(alt),
         attribute = tolower(attribute),
         level = case_when(
           value == "40days per year" ~ .40, #(scale coefficients)
           value == "75days per year" ~ .75,
           value == "115days per year" ~ 1.15,
           value == "150days per year" ~ 1.50,
           
           value == "1fish per trip" ~ 1,
           value == "2fish per trip" ~ 2,
           value == "3fish per trip" ~ 3,
           value == "4fish per trip" ~ 4,
           
           value == "30%(almost always threatened)" ~ .30,
           value == "47%(frequently threatened)" ~ .47,
           value == "85%(rarely threatened)" ~ .85,
           value == "95%(almost never threatened)" ~ .95,
           
           value == "45%non-native" ~ .45,
           value == "65%non-native" ~ .65,
           value == "85%non-native" ~ .85,
           value == "100%non-native" ~ 1,
           
           value == "10%of lake area" ~ 0.1,
           value == "20%of lake area" ~ 0.2,
           value == "30%of lake area" ~ 0.3,
           value == "50%of lake area" ~ 0.5,
           
           value == "$0Increase in annual costs for 10 years" ~ 0,
           value == "$20Increase in annual costs for 10 years" ~ 20,
           value == "$70Increase in annual costs for 10 years" ~ 70,
           value == "$135Increase in annual costs for 10 years" ~ 135,
           value == "$230Increase in annual costs for 10 years" ~ 230,
           value == "$350Increase in annual costs for 10 years" ~ 350,
           TRUE ~ -99)) %>%
  select(-value) %>%
  filter(level != -99)

warm = warm %>%
  pivot_wider(names_from = c(attribute, alt), 
              values_from = level)

Choice = df %>% 
  select(CaseID, SURVEY_VERSION,  POST_DCE_3_A4, starts_with(c("CHOICE_1_C", "CHOICE_2_C", "CHOICE_3_C", "CHOICE_4_C", "CHOICE_5_C", "CHOICE_6_C")))%>% 
  pivot_longer(-c(SURVEY_VERSION, CaseID,  POST_DCE_3_A4), names_to = "alt", values_to = "choice") %>%
  separate(alt, into = c("attribute", "alt"), sep = -2) %>%
  mutate(alt = parse_number(alt),
         question = parse_number(attribute)) %>%
  select(-attribute)

Choice = transform(Choice, choice = as.numeric(choice))

sapply(Choice, class)

Choice <- Choice %>%
  mutate(choice = choice * alt) 

Choice<-Choice%>%
  filter(choice != 0) %>%
  select(-alt)


#Join all data together 	
dc = Choice %>%
  left_join(warm, by = c("CaseID", "SURVEY_VERSION", "question")) %>%
  rename(choice_set = question)

## filter out most expensive options
dc <- mutate(dc, 
             max = ifelse(cost_1 > cost_2, 1, 2))

             
dc$max <- ifelse(dc$cost_1 > dc$cost_2, "1",
                 ifelse(dc$cost_2>dc$cost_1, "2",
                       "0"))
dc<- mutate(dc,
            expen= ifelse(dc$choice == dc$max, 1, 0))


dc_sum <- dc %>% group_by(CaseID, POST_DCE_3_A4.x) %>%
  summarise(sum = sum(expen))

warm <- dc_sum %>% filter(sum == 6)
#n=18

warm <- filter(warm, POST_DCE_3_A4.x >=6)

# n=12

## creating a dataset with warmglow responses, then removing them from dataset

df5 <-filter(df4, CaseID %in% c("763", "804", "859", "873",  "1807", "7315", "7650",  "7762", "7788", "7842",  "7871",  "7880"))

df6 <- anti_join(df4, df5)


###------------ creating a variable to indicate which were biased, binding with main dataset

df7<-anti_join(df, df6, by="CaseID")

df7$bias<- "1"
df6$bias<- "0"

moore<-rbind(df6, df7)

# export csv
write_csv(moore, "Data/Raw Data/EBL_bias.csv")

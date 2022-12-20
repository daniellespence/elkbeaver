
#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
library(tidyverse)

#------------------------------------#
# Import data
#------------------------------------#
library(readxl)
df <- read.csv("Data/Raw Data/FinalDataset.csv")


#------------------------------------#
# Format Data for Choice Set Tasks
#------------------------------------#

design = df %>%
  select(CaseID, starts_with(c("C1_", "C2_", "C3_","C4_", "C5_", "C6_"))) %>%
  #	filter(GROUP == "A1" | GROUP == "B1") %>%
  select(-ends_with("ORDER")) %>%
  pivot_longer(-c(CaseID), names_to = "type", values_to = "value",
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


#Convert choice set data to wide
design = design %>%
  pivot_wider(names_from = c(attribute, alt), 
              values_from = level)

Choice = df %>% 
  select(CaseID, starts_with(c("CHOICE_1_C", "CHOICE_2_C", "CHOICE_3_C", "CHOICE_4_C", "CHOICE_5_C", "CHOICE_6_C")))%>% 
  pivot_longer(-c(CaseID), names_to = "alt", values_to = "choice") %>%
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
  left_join(design, by = c("CaseID", "question")) %>%
  rename(choice_set = question)

# export csv
write_csv(dc, "Data/MAINChoices.csv")



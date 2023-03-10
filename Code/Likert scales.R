#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#install.packages('pacman')

library(pacman)

p_load(tidyverse, haven, car, ggplot2, mgcv, reshape2, likert, cowplot,
       install = TRUE)

#------------------------------------#
# Import data
#------------------------------------#

df <- read_csv("Data/Raw Data/MainDataTrimmed.csv")

#------------------------------------#
# Select likert value questions
#------------------------------------#
values<-df %>%
  select(CaseID,starts_with(c("S6Q7")))

## Convert wide to long
v1 <- values %>%
  pivot_longer(cols = starts_with("S6Q7"), names_to = "question", values_to = "answer") 

# Translate likert scales and questions

v1$answer <- factor(v1$answer,
                    levels=1:7,
                    labels=c('Strongly Disagree', 'Disagree', 'Slighty disagree', 'Neutral',  'Slightly agree', 'Agree','Strongly Agree'))

v1$question <- factor(v1$question, 
                      levels = c("S6Q7_A1","S6Q7_A2","S6Q7_A3","S6Q7_A4","S6Q7_A5","S6Q7_A6","S6Q7_A7"),
                      labels = c("Swimming", "Boating", "Consuming fish", "Pets", "Current WQ","Future WQ" ,   "Wildlife"))


v1$Type<- ifelse(v1$question %in% c("Swimming", "Boating", "Consuming fish", "Pets"), "EBL is safe for...", "I am concerned about...")

# determine proportion of each likert response
v1_summary <- v1 %>%
  group_by(question, answer) %>%
  dplyr::summarize(freq = length(CaseID)) %>%
  ungroup %>% group_by(question) %>% 
  mutate(proportion = freq / sum(freq))

#------------Plotting separately then combining

v2<- v1[v1$question %in% c("Swimming", "Boating", "Consuming fish", "Pets"),]


v2_summary <- v2 %>%
  group_by(question, answer) %>%
  dplyr::summarize(freq = length(CaseID)) %>%
  ungroup %>% group_by(question) %>% 
  mutate(proportion = freq / sum(freq))



g2 <- ggplot(v2, aes(x=question)) +
  geom_bar(aes(fill=answer), position="fill") +
  geom_text(
    data=v2_summary,
    aes(y=freq, label=percent(proportion, accuracy = 1), group=answer),
    position=position_fill(vjust=0.5),
    color='gray25', size=3
  ) +
  scale_fill_brewer(palette='Spectral', direction=1) +
  scale_y_continuous(expand=expansion(0.005), labels=scales::percent_format()) +
  labs(x='EBl is safe for...', y='Percent Answered', fill = "Answer", title ='EBl is safe for...' 
  ) +
  theme_classic() +
  theme(legend.position='null', axis.title.x = element_blank(), axis.title.y = element_blank(),  axis.text.y = element_text(angle = 45, vjust = -1))+coord_flip()

g2

v3 <- v1[v1$question %in% c("Current WQ","Future WQ" ,"Wildlife"),]

v3_summary <- v3 %>%
  group_by(question, answer) %>%
  dplyr::summarize(freq = length(CaseID)) %>%
  ungroup %>% group_by(question) %>% 
  mutate(proportion = freq / sum(freq))

g3<-  ggplot(v3, aes(x=question)) +
  geom_bar(aes(fill=answer), position="fill") +
  geom_text(
    data=v3_summary,
    aes(y=freq, label=percent(proportion, accuracy = 1), group=answer),
    position=position_fill(vjust=0.5),
    color='gray25', size=3
  ) +
  scale_fill_brewer(palette='Spectral', direction=1) +
  scale_y_continuous(expand=expansion(0.001), labels=scales::percent_format()) +
  labs(x='I am concerned about...', y='Percent Answered', fill=" ", title = 'I am concerned about...'
  ) +
  theme_classic() +
  theme(legend.position='bottom', legend.direction = 'horizontal' , axis.title.y = element_blank(),  axis.text.y = element_text(angle = 45, vjust = -1))+coord_flip()

g3

p<- g2+g3 +plot_layout(guides = 'auto', nrow = 2)
p

#----------------- plotting with plot_likert

p_load("sjPlot", "sjmisc", "tidyverse", "purrrlyr", "likert", "ggplot2")

values <- values  %>% 
  rename("Swimming" = "S6Q7_A1",
         "Boating" = "S6Q7_A2",
         "Consuming fish" = "S6Q7_A3",
         "Pets" = "S6Q7_A4",
         "Current WQ" = "S6Q7_A5",
         "Future WQ" = "S6Q7_A6",
         "Wildlife" = "S6Q7_A7")


plot_likert(values, catcount = 7, values = 'sum.outside', show.n=F, legend.labels = c('Strongly Disagree', 'Disagree', 'Slighty disagree', 'Neither', 'Slightly agree', 'Agree', 'Strongly Agree'))+theme_bw()


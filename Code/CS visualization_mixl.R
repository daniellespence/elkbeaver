# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(tidyverse)
library(car)
library(pacman)
p_load(ggsci, ggplot2, gridExtra, ggpubr)
p_load(tidyverse, haven, car, ggplot2, mgcv, reshape2, patchwork, viridis, gratia, cowplot,
       install = TRUE)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

wtp <- read.csv("output/wtp_all_mixl.csv") 

wtp <- wtp %>% 
  filter(Model != 'neighbours')

# ################################################################# #
#### PLOT DATA                                                   ####
# ################################################################# #

f1 <- wtp %>% 
  ggplot(aes(y = estimate, x = scenario, colour = Model))+
  geom_point(position=position_dodge(.9), aes(y=estimate, colour=Model), size = 3) + 
  geom_pointrange(aes(ymin=wtp_lo, ymax=wtp_hi), position = position_dodge(0.9)) + 
  ylab("Economic benefits per household per year ($)") + 
  xlab("Scenario") + labs(colour="Model") +
  ggpubr::color_palette("uchicago")+
  scale_y_continuous(limits = c(100, 325)) 
 
f2 <- f1 + theme_bw()


f2 + facet_wrap( ~ Policy, scales= "free_x", nrow = 1)

##-----------------------------------------------------------------------------



f1 <- ggplot(data = oxy, 
       aes(x=scenario,
           y= estimate, 
           colour=Model)) +
    geom_point(position=position_dodge(.9), aes(y=estimate, colour=Model)) + 
  geom_pointrange(aes(ymin=wtp_lo, ymax=wtp_hi), position = position_dodge(0.9)) + 
  theme(axis.title.x = element_blank())+
  scale_y_continuous(limits = c(100, 325)) +
ggpubr::color_palette("uchicago")
f1  
#
#labs(colour="Model") + 
f2<- f1 + theme_bw()
f3 <- f2+ theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())  
f3

f4<- ggplot(data = wmp, 
                  aes(x=scenario,
                      y= estimate, 
                      colour=Model)) +
  
  geom_point(position=position_dodge(.9), aes(y=estimate, colour=Model)) + 
  geom_pointrange(aes(ymin=wtp_lo, ymax=wtp_hi), position = position_dodge(0.9)) + 
scale_y_continuous(limits = c(100, 325)) + ggpubr::color_palette("uchicago")  

f5 <-f4+     theme_bw()
f6 <- f5+ 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) 
f6


combined <- f3+ f6 & theme(legend.position = "right", legend.direction = "vertical")
combined

c1 <- combined + plot_layout(guides = "collect", nrow=1)
c1

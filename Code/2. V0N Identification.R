###############################################################################
############# Identifying non-Vancouver Island reponses ######################
#############              3/16/2022                     ######################
###############################################################################

#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
library(tidyverse)

#------------------------------------#
# Import data
#------------------------------------#

df<- read_excel("Data/Raw Data/Elk_Beaver_Lake_Completes_Excel.xls")

## V0N contains southern gulf islands, but also north island, and mainland
# Need to filter and remove those not considered part of CRD,
# keeping those who indicated they have property in the CRD.


df1= filter(df, SCREENER1 == "1")

## this would be IDs 2317, 7828

df2 = filter(df1, D_POSTAL_CODE== "V0N 2M0")
df3 = filter(df1, D_POSTAL_CODE== "V0N 2M1")# not sure whether to include. this person indicates that they voted as if they would NOT be charged the amount shown, and that made voting easier. not consequential.
df4 = filter(df1, D_POSTAL_CODE== "V0N 2M2")
df5 = filter(df1, D_POSTAL_CODE== "V0N1P0")
df6 = filter(df1, D_POSTAL_CODE== "V8P 3E7")
df7 = filter(df, CaseID=="7828")


df8=filter(df, SCREENER1 != "1")

df8=rbind(df8, df2, df4, df5, df6, df7)

df9 <- anti_join(df, df8, by="CaseID")

df8$non_VI<-"0"
df9$non_VI<-"1"

df10<-rbind(df8, df9)

df10<- df10 %>% 
  select("CaseID", "non_VI")

write.csv(df10, row.names = F, file="Data/non_VI.csv")

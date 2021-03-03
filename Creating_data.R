#Creating data for analysis
#Kati Puukko, 13.1.2021


# Installing and loading packages #

library(dplyr) 
library(tidyverse)
library(foreign)
library(data.table)
library(magrittr)
library(ggplot2)
library(MplusAutomation)
library(jtools) 
library(sjPlot)
library(effects)
library(carData)
library(WRS2)
library(GGally)
library(psych)
library(corrplot)
library(vioplot)
library(writexl)
library(QuantPsyc)
library(MASS)


# Import data to R #
setwd('C:/LocalData/kpuukko/Parents2021')
fulldata <- read.spss("C:/LocalData/kpuukko/Parents2021/Parents_6ja8luokka_2020.sav", use.value.labels=FALSE, to.data.frame=TRUE)

# Check data #
glimpse(fulldata) # 1392 observations, 221 variables in the original data
summary(fulldata)
str(fulldata)
head(fulldata)

# Create new data frame with selected variables
d <- fulldata %>% 
  dplyr::select (Digipar1_1, Digipar1_2, Digipar1_3, Digipar1_4, Digipar1_5, Digipar2_1, Digipar2_2, 
                 Digipar2_3, Digipar2_4, Digipar2_5, Digipar2_6, Digipar3_1, Digipar3_2, Digipar4, 
                 ICTskills1_1, ICTskills1_2, ICTskills1_3, ICTskills1_4, ICTskills1_5, ICTskillsK_1, ICTskillsK_2, 
                 ICTskillsK_3, ICTskillsK_4, ICTskillsK_5, ICTskillsK_6, addic.s_1, addic.s_2, addic.s_3, 
                 addic.g_1, addic.g_2, addic.g_3, role, orgin.1, orgin.2, language_1, language_2, language_3, 
                 language_4, language_5, language_6, language_7, language_8, Edu, Curac_1, Curac_2, Curac_3,
                 Curac_4, Curac_5, Curac_6, Curac_7, Curac_8, jobrole, Fam_1, Fam_2, Fam_3, Fam_4, Fam_5, Fam_6,
                 Fam_7, Fam_8, Parentinvol_1, Parentinvol_2, Parentinvol_3, Parentinvol_4, Parentinvol_5, 
                 questionnaire_6, questionnaire_8, LivOther1_1, Finsitu, diffic, ID, Duplicate_IDs, toimi, dumrol)

# Recode tech1 and tech2 dummy (not necessary)
d <- d %>% 
  mutate(Digipar3_1 = recode(Digipar3_1, "1"=0, "2"=1)) %>%
  mutate(Digipar3_2 = recode(Digipar3_2, "1"=0, "2"=1))

# Rename variables #

#Digital parenting

colnames(d)[1] <- "ena1"
colnames(d)[2] <- "ena2"
colnames(d)[3] <- "ena3"
colnames(d)[4] <- "ena4"
colnames(d)[5] <- "ena5"
colnames(d)[6] <- "res1"
colnames(d)[7] <- "res2"
colnames(d)[8] <- "res3"
colnames(d)[9] <- "mon1"
colnames(d)[10] <- "mon2"
colnames(d)[11] <- "mon3"
colnames(d)[12] <- "tech1"
colnames(d)[13] <- "tech2"
colnames(d)[14] <- "rev1"


#Digital skills

colnames(d)[15] <- "skill1"
colnames(d)[16] <- "skill2"
colnames(d)[17] <- "skill3"
colnames(d)[18] <- "skill4"
colnames(d)[19] <- "skill5"
colnames(d)[20] <- "skill6"
colnames(d)[21] <- "skill7"
colnames(d)[22] <- "skill8"
colnames(d)[23] <- "skill9"
colnames(d)[24] <- "skill10"
colnames(d)[25] <- "skill11"


#Excessive use

colnames(d)[26] <- "esm1"
colnames(d)[27] <- "esm2"
colnames(d)[28] <- "esm3"
colnames(d)[29] <- "egm1"
colnames(d)[30] <- "egm2"
colnames(d)[31] <- "egm3"

#Origin 

colnames(d)[33] <- "ori1"
colnames(d)[34] <- "ori2"

#Languages

colnames(d)[35] <- "lang1"
colnames(d)[36] <- "lang2"
colnames(d)[37] <- "lang3"
colnames(d)[38] <- "lang4"
colnames(d)[39] <- "lang5"
colnames(d)[40] <- "lang6"
colnames(d)[41] <- "lang7"
colnames(d)[42] <- "lang8"

#Parent involvement

colnames(d)[61] <- "parent1"
colnames(d)[62] <- "parent2"
colnames(d)[63] <- "parent3"
colnames(d)[64] <- "parent4"
colnames(d)[65] <- "parent5"

#Questionnaire 6th and 8th grade

colnames(d)[66] <- "q6"
colnames(d)[67] <- "q8"

#Number of children

colnames(d)[68] <- "cnumb"

# Checking data # 
colnames(d)
str(d) 
head(d)
glimpse(d)
summary(d)
describe(d)

# Distributions #
hist(d$ena1)
hist(d$ena1)
hist(d$ena3)
hist(d$ena4)
hist(d$ena5)
hist(d$res1)
hist(d$res2)
hist(d$res3)
hist(d$mon1)
hist(d$mon2)
hist(d$mon3)
hist(d$tech1)
hist(d$tech2)
hist(d$rev1)


# Recode background variables #

#Number of children 
d <- d %>% #jatkuva
  mutate(cnumb = as.factor(cnumb))
d <- d %>% #kategorinen 1= 1 lapsi, 2= 2 lasta, 3= 3 tai enemmän
  mutate(cclass = recode(cnumb, "1"=1, "1."=1, "2" =2, "3"=3, "4"=3, "5"=3, "6"=3, "4 lasta jotka on alle 18" =3))

#Parental role 
d <- d %>% #dummy mother 1=yes, 0=no
  mutate(mother = recode(role, "1"=1, "2"=0, "3"=0, "4"=0, "5"=0, "6"=0, "7"=0, "8"=0, "9"=0))
d <- d %>% #dummy father 1=yes, 0=no
  mutate(father = recode(role, "1"=0, "2"=0, "3"=1, "4"=0, "5"=0, "6"=0, "7"=0, "8"=0, "9"=0))

d <- d %>%
  mutate(mmot = recode(mother, "1" = 0)) %>%
  mutate(ffat = recode(father, "1" = 1))

d$pclass2 <- rowSums(d[,c("yh","mh","uh")],na.rm = TRUE)

#Country of birth from 1 to 0 = Finland, from 2 to 1 = Other
d <- d %>% #dummy
  mutate(bcount = recode(ori1, "1"=0, "2"=1))

#Education level 1= Perusaste, 2 = Keskiaste, 3= Alempi korkeakoulu, 4 = Ylempi korkeakoulu, 5 = Tutkijakoulutus
d <- d %>%
  mutate(Eclass = recode(Edu, "1"= 1, "2"=2, "3"=2, "4"= 2, "5"=3, "6"= 4, "7"= 3, "8"= 4, "9"= 5))

#Education level dummy 1= Korkeakoulututkinto, 0= Ei korkeakoulututkintoa
d <- d %>%
  mutate(Edum = recode(Eclass, "1"= 0, "2"=0, "3"=1, "4"= 1, "5"=1))

#6 vai 8 luokkalaisen vanhempi
d <- d %>%
  mutate(kuusi = recode(q6, "1" = 0)) %>%
  mutate(kasi = recode(q8, "1" = 1))

d$cage <- rowSums(d[,c("kuusi","kasi")],na.rm = TRUE)

#Family structure summamuuttuja
d <- d %>%
  mutate(yh = recode(Fam_1, "1" = 1)) %>%
  mutate(mh = recode(Fam_2, "1" = 2)) %>%
  mutate(uh = recode(Fam_6, "1" = 3))

d$pclass2 <- rowSums(d[,c("yh","mh","uh")],na.rm = TRUE)

#Koodattu niin, että 4 ja 5 arvon saaneet ovat blended family
d <- d %>%
  mutate(pclass2 = recode(pclass2, "1" = 1, "2" = 2, "3" = 3, "4" = 3, "5"= 3))

#Sum scores
d$tech_c <- rowSums(d[,c("tech1","tech2")])
d$ena_c <- rowMeans(d[,c("ena1","ena2","ena3","ena4")],na.rm = TRUE)
d$mon_c <- rowMeans(d[,c("mon1","mon2","mon3")],na.rm = TRUE)
d$res_c <- rowMeans(d[,c("res1","res2","res3")],na.rm = TRUE)
d$pvol_c <- rowMeans(d[,c("parent1","parent2","parent3","parent4","parent5")],na.rm = TRUE)
d$digi_c <- rowMeans(d[,c("skill1","skill2","skill3","skill4","skill5", "skill6", "skill7", "skill8", "skill9", "skill10", "skill11")],na.rm = TRUE)
d$exc_c <- rowMeans(d[,c("esm1", "esm2", "esm3")],na.rm = TRUE)

#Distributions
hist(d$tech_c)
hist(d$ena_c)
hist(d$mon_c)
hist(d$res_c)
hist(d$pvol_c)
hist(d$digi_c)
hist(d$exc_c)

#Correlations
d3 <- d %>% 
  dplyr::select(ena_c, res_c, mon_c, tech_c, rev1, Finsitu, Edu)

corr.test(d3)
ggpairs(d3)

#Descriptives
head(d)
describe(d)

#Save Mplus data with sum scores
prepareMplusData(d, "parent_mplus2.dat")

#Merging student and parent data
#Kati Puukko, 11.2.2021


# Loading packages #

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

sfulldata <- read.spss("C:/LocalData/kpuukko/Parents2021/6_8luokka_oppilasdata.sav", use.value.labels=FALSE, to.data.frame=TRUE)

sfulldata


# Oppilasdata #

glimpse(sfulldata) #Rows: 2,707, Columns: 333
summary(sfulldata)
str(sfulldata)
head(sfulldata)

od <- sfulldata %>% 
  dplyr::select (ID, OGender, P_Q1, P_Q2, adig1, adig3, adig4, adig7, adig8, adig11, adig12, adig13, adig14, adig16, adig17,
adig18, adig19, bdig3, bdig5, bdig6, bdig8, bdig10, bdig12, bdig13, bdig14, bdig15, bdig16, bdig17, bdig18,
bdig19, cdig1, cdig2, cdig3, cdig7, cdig8, cdig9, cdig10, cdig11, cdig13, cdig16, cdig17, cdig18, cdig19, 
conlea1, conlea2, conlea3, conlea4, conlea5, conlea6, negadigi1, negadigi2, negadigi3, negadigi4,
negadigi5, Ofina, MGender)

glimpse(od) #2707 vastausta 77 muuttujaa
str(od)

# Oppilasdata Sum scores
od$oeiu <- rowMeans(od[,c("negadigi1", "negadigi2", "negadigi3", "negadigi4", "negadigi5")],na.rm = TRUE)

# Vanhempien data #

glimpse(d) #1392 vastausta 93 riviä
summary(d)
str(d)
head(d)

# Merging datafiles #
gdata <- left_join(d, od, by= "ID") #kaikki vanhempien rivit oli niiden lapsista dataa tai ei
ydata <- inner_join(d, od, by = "ID") #mätsää vain vanhemmat joiden lapsista on dataa ja lapset joiden vanhemmista on dataa

glimpse(gdata)
str(gdata)

glimpse(ydata)
str(ydata)

ydata <- as.data.frame(ydata)
gdata <- as.data.frame(gdata)

#Number of children 
ydata <- ydata %>% #jatkuva
  mutate(cnumb = as.factor(cnumb))

gdata <- gdata %>% #jatkuva
  mutate(cnumb = as.factor(cnumb))

str(ydata)

# Save Mplus data #
prepareMplusData(ydata, "merged_mplus2.dat")
prepareMplusData(gdata, "merged_mplus3.dat")


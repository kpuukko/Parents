
#Avataan käytetyt paketit (ks. pakettien lähteet ja käytetyt versiot liitteen lopusta)

library(dplyr)
library(foreign)
library(lavaan)
library(psych)
library(semPlot)
library(stringi)
library(tidyLPA)

#Käytetyn tiedoston avaus

setwd('C:/LocalData/kpuukko/Parents_2021')
d

#Jos haluaa filtteröidä pois puuttuvan datan
d2 <- d[rowSums(is.na(d[,1:14]))==1,]

#Plot-taulukoiden tarkastelu 2-10 ryhmän ratkaisuilla
d2 %>%
  dplyr::select(ena_c, res1, res2, res3, mon1, mon2, mon3, rev1, tech_c) %>%
  estimate_profiles(1:4, package = "MplusAutomation") %>% 
  plot_profiles(rawdata = FALSE,add_line = TRUE,ci = FALSE, sd = FALSE)

#Muodostetaan 1-8 ryhm�n mallit
model <- d2 %>%
  dplyr::select(ena1, ena2, ena3, ena4, res1, res2, res3, mon1, mon2, mon3, rev1, tech_c) %>%
  estimate_profiles(1:8, 
                    variances = c("equal"),
                    covariances = c("zero"), 
                    package = "MplusAutomation")

#Tiedot mallien sopivuudesta ja niiden vertailua
get_fit(model)
compare_solutions(model, statistics = c("AIC", "BIC"))
plot_profiles(model,rawdata = FALSE,add_line = TRUE,ci = FALSE, sd = FALSE)

x <- get_data(model)
x <- as.data.frame(x)
x2 <- tidyr::spread(x, Class_prob, Probability)

View(x2)
class.desc <- describeBy(x2, x2$Class)
class.desc

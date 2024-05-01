library(readxl)
library(ggplot2)
library(openxlsx)
library(mvtnorm)
library(survival)
library(MASS)
library(TH.data)
library(tidyr)
library(tidyverse)
library(dplyr)
library(palmerpenguins)
library(emmeans)
library(multcomp)
library(ggsci)
library(tidytext)
library(ggpubr)
library(ggrepel)
library(webr)




lw <- read_excel("Length-Weight.xlsx", sheet = "Sheet1")




lw_model <- lm(weight ~ Length, lw)
summary(lw_model)

lw %>% 
  ggplot(aes(Length, weight, color = weight))+
  geom_point(size = 2, alpha = 1, show.legend = F)+
  geom_smooth(method = "lm", se = F, show.legend = F, color = "red", lwd = 0.8)+
  theme_classic()+
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(colour = "black")
        
        )+
  labs( y = "Wight (g)", x = "Lenght (cm)")+
  annotate("text", x = 14, y = 160, label = "P value= 0.00")+
  annotate("text",x = 14, y = 145, label = "Adjusted R^2 = 0.84")+
  annotate("text", x = 14, y = 130, label = " y = 7.48x - 103.06")

+ggsave("lenghtweight.png", dpi = 1000, height = 7, width = 7)





# log value ---------------------------------------------------------------


log_model <- lm(log(weight) ~ log(Length), lw)
summary(log_model)

a <- exp(coef(log_model)[1])
a
b <- coef(log_model)[2]
b

lw %>% 
  ggplot(aes(log(Length), log(weight), color = weight))+
  geom_point(size = 2, alpha = 1, show.legend = F)+
  geom_smooth(method = "lm", se = F, show.legend = F, color = "red", lwd = 0.8)+
  theme_classic()+
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs( y = "logWight (g)", x = "logLenght (cm)")+
  annotate("text", x = 2.5, y = 5.8, label = "P value= 0.00")+
  annotate("text",x = 2.55, y = 5.5, label = "Adjusted R^2 = 0.95")+
  annotate("text", x = 2.508, y = 5.2, label = " y = 3.21x - 5.94")+
  annotate("text", x = 2.6, y = 4.9, label = " Log(W) = Log(a) + bLog(L)")+
  annotate("text", x = 2.64, y = 4.6, label = " Relationship, W = 0.0026L^3.21")
  

ggsave("loglenghtweight.pdf", dpi = 1000, height = 7, width = 7)





# LBSPR -------------------------------------------------------------------

library(LBSPR)
MyLengths <- new("LB_lengths")
datdir <- DataDir()
datdir
MyPrars <- new("LB_pars")
MyPrars@Species <- "Pabda"
MyPrars@Linf <- 36.84
MyPrars@L50 <- 21.3
MyPrars@L95 <- 23.43
MyPrars@MK <- 1.5
MyPrars@L_units <- "cm"
Len2 <- new("LB_lengths", LB_pars=MyPrars,
            file=paste0(datdir, "/LengthData.csv"),
            dataType="freq", header=F)
plotSize(Len2)
myFit2 <- LBSPRfit(MyPrars, Len2)
myFit <- LBSPRfit(MyPrars, Len2)
myFit2@Ests
data.frame(rawSL50=myFit@SL50, rawLS95=myFit@SL95, rawFM=myFit@FM,
           rawSPR=myFit@SPR)
plotSize(myFit2)
plotMat(myFit2)
plotEsts(myFit)

Mypars <- new("LB_pars", verbose=F)
Mypars@Linf <- 36.84
Mypars@L50 <- 21.3
Mypars@L95 <- 23.43
Mypars@MK <- 1.5
Mypars@SPR <- 0.28
Mypars@BinWidth <- 2

LenDat <- new("LB_lengths", LB_pars=Mypars, file=paste0(datdir, "/lbspr.csv"),
              dataType="freq", verbose=F)
Mod <- LBSPRfit(Mypars, LenDat, verbose = F)
yr <- 1
Mypars@SL50 <- Mod@SL50[yr]
Mypars@SL95 <- Mod@SL95[yr]
plotTarg(Mypars, LenDat, yr=yr)






# working with new data ---------------------------------------------------


lw <- read.csv("new.csv")



log_model <- lm(log(Weight) ~ log(Length), lw)
summary(log_model)

log_model$coefficients

a <- exp(coef(log_model)[1])
a



lw %>% 
  ggplot(aes(log(Length), log(Weight), color = log(Weight)))+
  geom_point(size = 2, alpha = 1, show.legend = F)+
  geom_smooth(method = "lm", se = F, show.legend = F, color = "red", lwd = 0.8)+
  theme_classic()+
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs( y = "logWight (g)", x = "logLenght (cm)")+
  annotate("text", x = 2.5, y = 5.8, label = "P value= 0.00")+
  annotate("text",x = 2.55, y = 5.5, label = "Adjusted R^2 = 0.95")+
  annotate("text", x = 2.508, y = 5.2, label = " y = 3.21x - 2.584")+
  annotate("text", x = 2.6, y = 4.9, label = " Log(W) = Log(a) + bLog(L)")+
  annotate("text", x = 2.64, y = 4.6, label = " Relationship, W = 0.0026L^2.584")


ggsave("new_loglenghtweight.pdf", dpi = 1000, height = 6, width = 6)






























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
library(ggsci)
library(webr)
fuad <- read_excel("TPData.xlsx", sheet = "working")


all <- fuad %>% 
  select(1:5) %>%
  pivot_longer(cols = 2:5, names_to = "type", values_to = "value") %>% 
  ggplot(aes(Station, value, fill = Station))+
  geom_bar(stat = "identity")+
  facet_wrap(~type, scales = "free_y")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(colour = "black"))+
  labs(y = "Value")

ggsave("all weight.png", all, height = 8, width = 8, dpi = 1500)






mbw <- fuad %>% 
  select("Station", "Mean body weight (g)",  "SDB") %>% 
  ggplot(aes(Station, fuad$`Mean body weight (g)`, fill = Station))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = fuad$`Mean body weight (g)`-SDB, ymax= fuad$`Mean body weight (g)` +SDB
                    ), size = 0.7, width = 0.5)+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Mean Body Weight (g)")

ggsave("MBW.png", mbw, height = 5, width = 5, dpi = 1500)

mgw <- fuad %>% 
  select("Station", "Mean Gut weight",  "SDG") %>% 
  ggplot(aes(Station, fuad$`Mean Gut weight`, fill = Station))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = fuad$`Mean Gut weight`-SDG, ymax= fuad$`Mean Gut weight` +SDG
  ), size = 0.7, width = 0.5)+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Mean Gut weight (g)")

ggsave("MGW.png", mgw, height = 5, width = 5, dpi = 1500)


  
stacked <- fuad %>% 
  select(1,3, 5) %>% 
  pivot_longer(cols = 2:3, names_to = "weight", values_to = "value") %>% 
  ggplot(aes(Station, value, fill = weight))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Mean weight")

ggsave("stacked.png", stacked , height = 5, width = 5, dpi = 1500)



perc <- fuad %>% 
  select(1,3, 5) %>% 
  mutate(MGW  = (`Mean Gut weight`/ sum(`Mean Gut weight`))*100,
         MBW = (`Mean body weight (g)`/ sum(`Mean body weight (g)`))*100) %>% 
  pivot_longer(cols = 4:5, names_to = "Weight", values_to = "value") %>% 
  ggplot(aes(Station, value, fill = Weight))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Percentage (%)")

ggsave("Percentage.png", perc , height = 5, width = 5, dpi = 1500)


data <- read_excel("TPData.xlsx", sheet = "Sheet2")


# without category --------------------------------------------------------

cp <- data %>% 
  pivot_longer(cols = 2:12, names_to = "category", values_to = "value") %>% 
  filter(! Type == "Percentage") %>% 
  #View()
  ggplot(aes(category, value, fill = category))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 2)+
  ylim(-10, 42)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "black")
        )+
  labs(y = "Number")+
  geom_text(aes(label = value))

ggsave("PieDonut.png", cp , height = 7, width = 8, dpi = 1500)

data <- read_excel("TPData.xlsx", sheet = "Sheet2")


TSCS <- data %>% 
  mutate(Percent = (number/sum(number))*100) %>% 
  ggplot(aes(Type, Percent, fill = category))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Percentage (g)",  color = "Category")
  

ggsave("TSCS_percent.png", TSCS , height = 5, width = 6, dpi = 2000)


# Donut chart -------------------------------------------------------------
library(webr)
data %>% 
  PieDonut(aes(Type, category, count = number))

ggsave("DONUT.png", ch , height = 5, width = 6, dpi = 2000)


TSCD <- data %>% 
  ggplot(aes(Type, number, fill = category))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Number")

ggsave("TSCD.png", TSCD , height = 5, width = 5, dpi = 1500)

# percent -----------------------------------------------------------------

TSCDP <- data %>% 
  group_by(Type) %>% 
  mutate(Percent = (number/ sum(number)*100)) %>% 
  ggplot(aes(Type, Percent, fill = category))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Percentage (%)", fill = "Category")

ggsave("TSCD_percent.png", TSCDP , height = 5, width = 5, dpi = 1500)


data %>% 
  ggplot(aes(Type, number, fill = category))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 1)+
  ylim(-5, 30)
  
  
  
  
  
  
  theme_classic()+
  theme(axis.text = element_text(colour = "black"))+
  labs(y = "Number")








  

# Sample data for three layers
layer1_data <- data.frame(
  Class = c("A1", "B1", "C1"),
  Value = c(20, 30, 15)
)

layer2_data <- data.frame(
  Class = c("A2", "B2", "C2"),
  Value = c(15, 25, 10)























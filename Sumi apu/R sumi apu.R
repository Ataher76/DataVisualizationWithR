
# Sumi apu mrn 15-16 ------------------------------------------------------

# ND IUCN red list status ----------------------------------------------------------------------
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
ND <- read_excel("ND.xlsx", sheet = "working")



#Saving the summary data

summary <- ND %>% 
  group_by(Category, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  write.xlsx("output.xlsx")

#Another way of saving data frame as excel file
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", summary)
saveWorkbook(wb, "ND_summaryy.xlsx", overwrite = T)



#ND1
NDJ1 <- ND %>% 
  group_by(Category, IUCN_RLS) %>% 
  summarise(Number = n()) %>% 
  arrange(desc(Number)) %>% 
  ggplot(aes(x = factor(Category), y = log(Number), fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = -0.8)+
  theme(panel.grid = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black", angle = 45, size = 9),
        axis.text.y = element_text(colour = "black", size = 8),
        axis.title.x =  element_blank())
#geom_text(aes(x = Category, y = log(Count), label = Count))


ggsave("ND_polar.png", NDJ1, dpi = 1000, width = 8, height = 6)


#ND stacked bar
NDJ2 <- ND %>% 
  group_by(Category, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = (Category), y = log(Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(colour = "black", size = 10),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        axis.line = element_line(color = "black"))

ggsave("ND_stacked.png", NDJ2, dpi = 2000, width = 6, height = 6.5)



#ND2
ND %>% 
  group_by(Category, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = Category, y = (Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity", position = "dodge" )+
  ylim(-1, 115)+
  coord_polar(theta = "x", start = -1)+
  theme(panel.grid = element_line(colour = "black"),
        panel.background = element_blank())
#geom_text(aes(x = Category, y = (Count),  label = Count))


# nijhum dwip PieDonut chart ----------------------------------------------

ND %>% 
  group_by(Category, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  group_by(Category) %>%
  mutate(percent = Count/ sum(Count)*100 )
  PieDonut(aes(Category, IUCN_RLS, count = Count))






# Saint Martin Island (STMI) IUCN red list status ----------------------------------------------------------------------



stmi <- read_excel("STMI.xlsx", sheet = "working")

stmi %>% 
  group_by(Case, IUCN_RLS) %>% 
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% 
  write.xlsx("STMI_summary.xlsx", overwrite = T)

#STMI1
stmi1 <- stmi %>% 
  group_by(Case, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = Case, y = log(Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 1.5)+
  theme(panel.grid = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black", angle = 20, size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        axis.title = element_blank())
#geom_text(aes(label = Count))
ggsave("stmi_polar.png", stmi1, dpi = 1000, width = 8, height = 6)

#STMI bar

stmi2 <- stmi %>% 
  group_by(Case, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = Case, y = log(Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(
    axis.text.x = element_text(colour = "black", angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(colour = "black", size = 10),
  )


ggsave("stmi_stacked.png", stmi2, dpi = 2000, width = 6, height = 6.5)

#STMI1
mydata %>% 
  group_by(Case, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = Case, y = (Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity", position = "dodge" )+
  ylim(-1, 15)+
  coord_polar(theta = "x", start = 0.8)+
  theme(panel.grid = element_line(colour = "black"),
        panel.background = element_blank())
#geom_text(aes(label = Count))



# Saint martin reword -----------------------------------------------------

st <- read_excel("STMI.xlsx", sheet = "Sheet1")
#Stacked bar and dodge bar
sts <- st %>% 
  ggplot(aes(Type, Number, fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
 #geom_bar(stat = "identity", position = "dodge")+
  theme(panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank()
        )
ggsave("New_stmi_stacked.png", sts, dpi = 2000, width = 6, height = 6.5)


stp <- st %>% 
  ggplot(aes(x = Type, y = (Number) , fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = -1)+
  ylim(-1, 15)+
  theme(panel.grid = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.title.x = element_blank()
  )
ggsave("New STMI_polar.png", stp, dpi = 2000, width = 6, height = 6.5)

library(webr)

st %>% 
PieDonut( aes(Type, IUCN_RLS, count = Number),
          )












# Swatch of No Ground IUCN red list status ------------------------------------------------------------

library(readxl)
library(tidyverse)
library(Matrix)
library(ggplot2)
library(openxlsx)

SNG <- read_excel("SNG.xlsx", sheet = "working")


SNG%>% 
  group_by(Condition, Category, IUCN_RLS) %>% 
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% 
  write.xlsx("SNG_summaryy.xlsx", overwrite = T)

#SNG1
sng1 <- SNG %>% 
  group_by(Condition, Category , IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = Category, y = log(Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
  facet_wrap(~Condition)+
  coord_polar(theta = "x", start = -2.5)+
  theme(panel.grid = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black", angle = 55, size = 8),
        axis.text.y = element_text(colour = "black", size = 8),
        axis.title = element_blank())
#geom_text(aes(label = Count))
ggsave("SNG_polar.png", sng1, dpi = 1000, width = 10, height = 6)



#SNG bar plot

sng2 <- SNG %>% 
  group_by(Condition, Category , IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = Category, y = log(Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity")+
  facet_wrap(~Condition)+
  theme_classic()+
  theme(axis.title.y = element_text(colour = "black", size = 8),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 8),
        axis.line = element_line(color = "black"))

ggsave("SNG_stacked.png", sng2, dpi = 2000, width = 6, height = 6.5)


#SNG2
SNG %>% 
  group_by(Condition, Category, IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  ggplot(aes(x = Category, y = (Count), fill = IUCN_RLS))+
  geom_bar(stat = "identity", position = "dodge" )+
  ylim(-1, 15)+
  facet_wrap(~Condition)+
  coord_polar(theta = "x", start = -2)+
  theme(panel.grid = element_line(colour = "black"),
        panel.background = element_blank())
#geom_text(aes(label = Count))




# PieDonut chart of SNG ---------------------------------------------------

SNG %>% 
  #view()
  group_by(Condition, Category , IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  filter(Condition %in% "Recorded") %>% 
  group_by(Category) %>% 
  mutate(Percentage = (Count / sum(Count)*100)) %>% 
  #view()
  PieDonut(aes(Category, IUCN_RLS, count = Count))


SNG %>% 
  group_by(Condition, Category , IUCN_RLS) %>% 
  summarise(Count = n()) %>% 
  filter(Condition %in% "Suspected") %>% 
  group_by(Category) %>% 
  mutate(Percentage = (Count / sum(Count)*100)) %>% 
  #view()
  PieDonut(aes(Category, IUCN_RLS, count = Count))















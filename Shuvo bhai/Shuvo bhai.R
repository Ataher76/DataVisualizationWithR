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


# MPs by shape and color in gills -----------------------------------------


SCG <- read_excel("ShuvoMP.xlsx", sheet = "MPsSCG")
glimpse(SCG)


# Color wise MPs  only fiber ----------------------------------------------

SCGbox <- SCG %>% 
  select(1:9) %>% 
  #filter( TypeFiber %in% "Fiber") %>% 
  pivot_longer(cols = 4:9, names_to = "Color", values_to = "Value") %>% 
  ggplot(aes(Station, Value, fill = Color))+
  geom_boxplot(show.legend = F, outlier.shape = NA )+
  geom_jitter(size = 1, show.legend = F)+
  facet_wrap(~Color)+
  theme_bw()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
        )+
  labs( y = "MPs")

ggsave("FiberBoxSCG.pdf", SCGbox, dpi = 1000, height = 6, width = 8)


stacked <- SCG %>% 
  select(1:9) %>% 
  #filter( TypeFiber %in% "Fiber") %>% 
  pivot_longer(cols = 4:9, names_to = "Color", values_to = "Value") %>% 
  ggplot(aes(Station, Value, fill = Color))+
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_npg()+
  theme_classic()+
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(colour = "black")
        )

ggsave("FiberStackedSCG.pdf", stacked, dpi = 1000, height = 6, width = 6)


# Color wise MPs only Filament ---------------------------------------------

Filbox <- SCG %>% 
  select(1, 12:16) %>% 
  #filter( TypeFiber %in% "Filament") %>% 
  pivot_longer(cols = 2:6, names_to = "Color", values_to = "Value") %>% 
  #filter(! Color == "Yellowish" ) %>% 
  ggplot(aes(Station, Value, fill = Color))+
  geom_boxplot(outlier.shape = NA, show.legend = F)+
  geom_jitter(size = 1, show.legend = F)+
  facet_wrap(~Color)+
  theme_bw()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs( y = "MPs")

ggsave("FilamentBoxSCG.pdf", Filbox, dpi = 1000, height = 6, width = 8)



stackedfill <- SCG %>% 
  select(1, 12:16) %>% 
  #filter( TypeFiber %in% "Filament") %>% 
  pivot_longer(cols = 2:6, names_to = "Color", values_to = "Value") %>% 
  #filter(! Color == "Yellowish" ) %>%
  ggplot(aes(Station, Value, fill = Color))+
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_npg()+
  theme_classic()+
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(colour = "black")
  )

ggsave("FilamentStackedSCG.pdf", stackedfill, dpi = 1000, height = 6, width = 6)






# all MPs station wise ----------------------------------------------------

allSCG <- SCG %>% 
  select(1, 10, 17:21) %>% #View()
  pivot_longer(cols = 2:7, names_to = "Type", values_to = "Value") %>% #View()
  ggplot(aes(Station, Value, fill = Type))+
  geom_boxplot(show.legend = F, outlier.shape = NA )+
  geom_jitter(size = 1.2, alpha = 0.7, show.legend = F)+
  facet_wrap(~Type, scales = "free_y")+
  theme_bw()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
        )+
  labs(y = "MPs")+
  scale_fill_npg()

ggsave("AllBoxSCG.pdf", allSCG, dpi = 1000, height = 6, width = 8)



AllStacked <- SCG %>% 
  select(1, 10, 17:21) %>% #View()
  pivot_longer(cols = 2:7, names_to = "Type", values_to = "Value") %>% #View()
  group_by(Station) %>% 
  mutate(Percentage = (Value/sum(Value))*100) %>% 
  #View()
  ggplot(aes(Station, Value, fill = Type))+
  geom_bar(stat = "identity", position = "fill")+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black"))+
  scale_fill_npg()+
  labs(y = "Percentage (%)")

ggsave("AllStackedSCG.pdf", AllStacked, dpi = 1000, height = 6, width = 6)

library(writexl)

SCG %>% 
  select(1, 10, 17:21) %>% #View()
  pivot_longer(cols = 2:7, names_to = "Type", values_to = "Value") %>% 
  group_by( Station, Type) %>%
  summarize(Mean = mean(Value),
            SD = sd(Value)) %>% 
  ggplot(aes(Station, Mean, fill = Type))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD, width = 0.5))
  #pivot_wider(names_from = Type, values_from = c("Mean", "SD")) %>% #View()
  #write.xlsx("summary data.xlsx", overwrite = T)
  





# Total MPs categorywise --------------------------------------------------

mean <- SCG %>% 
  select(1, 10, 17:21) %>% #View()
  group_by(Station) %>% 
  mutate(TotalMPs = rowSums(across(everything())))  #%>% View()


# mean+SD bar graph station wise ------------------------------------------------------

TotalBar <- mean %>% 
  group_by(Station) %>% 
  summarize(mean = mean(TotalMPs),
            SD = sd(TotalMPs)) %>% 
  ggplot(aes(Station, mean, fill = Station))+
  geom_bar(stat = "identity", show.legend = F)+
  geom_errorbar(aes(ymin = mean-SD, ymax = mean+SD, width = 0.5))+
  scale_fill_npg()+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(color = "black")
        )+
  labs(y = "Mean MPs")
 
ggsave("TotalBar.png", TotalBar, dpi = 1000, height = 6, width = 6) 





model <- aov(data = mean, TotalMPs ~ Station)
summary(model)
tukey <- TukeyHSD(model)
plot(tukey)

emmean <- emmeans(model, specs = "Station")
emmean_cld <- cld(emmean, Letters = letters)

TotalboxSCG <-  mean %>% 
  ggplot(aes(Station, TotalMPs, fill = Station))+
  geom_boxplot(show.legend = F, outlier.shape = NA)+
  geom_jitter(size = 1, show.legend = F)+
  theme_classic()+
  scale_fill_npg()+
  geom_text(data = emmean_cld, aes(x = Station, y = upper.CL, label = .group),
            size = 5, hjust = 0.2, vjust = 0.2, show.legend = F)+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
        )+
  labs( y = "Total MPs")
  
ggsave("TotalBoxSCG.png", TotalboxSCG, dpi = 1000, height = 6, width = 6)



  


# MPs by shape and color in DT -----------------------------------------

SDT <- read_excel("ShuvoMP.xlsx", sheet = "MPsSDT")
glimpse(SDT)

SDTbox <- SDT %>% 
  select(1:9) %>% 
  #filter( TypeFiber %in% "Fiber") %>% 
  pivot_longer(cols = 4:9, names_to = "Color", values_to = "Value") %>% 
  ggplot(aes(Station, Value, fill = Color))+
  geom_boxplot(show.legend = F, outlier.shape = NA )+
  geom_jitter(size = 1, show.legend = F)+
  facet_wrap(~Color)+
  theme_bw()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs( y = "MPs")

ggsave("FiberBoxSDT.pdf", SDTbox, dpi = 1000, height = 6, width = 8)


stackedSDT <- SDT %>% 
  select(1:9) %>% 
  #filter( TypeFiber %in% "Fiber") %>% 
  pivot_longer(cols = 4:9, names_to = "Color", values_to = "Value") %>% 
  ggplot(aes(Station, Value, fill = Color))+
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_npg()+
  theme_classic()+
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs(y = "Percentage (%)")

ggsave("FiberStackedSDT.pdf", stackedSDT, dpi = 1000, height = 6, width = 6)




# Color wise MPs only Filament ---------------------------------------------

FilboxSDT <- SDT %>% 
  select(1:2, 11:14) %>% 
  #filter( TypeFiber %in% "Filament") %>% 
  pivot_longer(cols = 4:6, names_to = "Color", values_to = "Value") %>% 
  #filter(! Color == "Yellowish" ) %>% 
  ggplot(aes(Station, Value, fill = Color))+
  geom_boxplot(outlier.shape = NA, show.legend = F)+
  geom_jitter(size = 1, show.legend = F)+
  facet_wrap(~Color)+
  theme_bw()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs( y = "MPs")

ggsave("FilamentBoxSDT.pdf", FilboxSDT, dpi = 1000, height = 3.5, width = 8)



stackedfillSDT <- SDT %>% 
  select(1:2, 11:14) %>% 
  #filter( TypeFiber %in% "Filament") %>% 
  pivot_longer(cols = 4:6, names_to = "Color", values_to = "Value") %>% 
  #filter(! Color == "Yellowish" ) %>%
  ggplot(aes(Station, Value, fill = Color))+
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_npg()+
  theme_classic()+
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs(y = "Percentage (%)")

ggsave("FilamentStackedSDT.pdf", stackedfillSDT, dpi = 1000, height = 6, width = 6)




# all MPs station wise ----------------------------------------------------

allSDT <- SDT %>% 
  select(1:2,10, 15: 20) %>% #View()
  pivot_longer(cols = 3:9, names_to = "Type", values_to = "Value") %>% #View()
  ggplot(aes(Station, Value, fill = Type))+
  geom_boxplot(show.legend = F, outlier.shape = NA )+
  geom_jitter(size = 1.2, alpha = 0.7, show.legend = F)+
  facet_wrap(~Type, scales = "free_y")+
  theme_bw()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs(y = "MPs")+
  scale_fill_npg()

ggsave("AllBoxSDT.pdf", allSDT, dpi = 1000, height = 6, width = 8)



AllStackedSDT <- SDT %>% 
  select(1:2,10, 15: 20) %>% #View()
  pivot_longer(cols = 3:9, names_to = "Type", values_to = "Value") %>% #View()
   ggplot(aes(Station, Value, fill = Type))+
  geom_bar(stat = "identity", position = "fill")+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
        )+
  labs(y = "Percentage (%)")+
  scale_fill_npg()

ggsave("AllStackedSDT.pdf", AllStackedSDT, dpi = 1000, height = 6, width = 6)





# Total MPs in the DT station wise -------------------------------------




# Total MPs categorywise --------------------------------------------------

mean <- SDT %>% 
  select(1,10, 15: 20) %>% #View()
  group_by(Station) %>% 
  mutate(TotalMPs = rowSums(across(everything())))  #%>% View()


# mean+SD bar graph station wise ------------------------------------------------------

TotalBarSDT <- mean %>% 
  group_by(Station) %>% 
  summarize(mean = mean(TotalMPs),
            SD = sd(TotalMPs)) %>% 
  ggplot(aes(Station, mean, fill = Station))+
  geom_bar(stat = "identity", show.legend = F)+
  geom_errorbar(aes(ymin = mean-SD, ymax = mean+SD, width = 0.5))+
  scale_fill_npg()+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(color = "black")
  )+
  labs(y = "Mean MPs")

ggsave("TotalBarSDT.png", TotalBarSDT, dpi = 1000, height = 6, width = 6) 



model <- aov(data = mean, TotalMPs ~ Station)
summary(model)
tukey <- TukeyHSD(model)
plot(tukey)

emmean <- emmeans(model, specs = "Station")
emmean_cld <- cld(emmean, Letters = letters)

TotalboxSDT <-  mean %>% 
  ggplot(aes(Station, TotalMPs, fill = Station))+
  geom_boxplot(show.legend = F, outlier.shape = NA)+
  geom_jitter(size = 1, show.legend = F)+
  theme_classic()+
  scale_fill_npg()+
  geom_text(data = emmean_cld, aes(x = Station, y = upper.CL, label = .group),
            size = 5, hjust = 0.2, vjust = 0.2, show.legend = F)+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black")
  )+
  labs(y = "Total MPs")

ggsave("ToalBoxSDT.png", TotalboxSDT, dpi = 1000, height = 6, width = 6)







# analysis of CLW ---------------------------------------------------------

# toal weight + toal MPs --------------------------------------------------

clw <- read_excel("ShuvoMP.xlsx", sheet = "CLW")

cmod <- lm(`Total Mps (G+DT)` ~ `Crab weight (gm)`, clw)
summary(cmod)


bw <- clw %>% 
  ggplot(aes(`Crab weight (gm)`, `Total Mps (G+DT)`, color = Station))+
  geom_point(size = 4, alpha = 0.8)+
  geom_smooth(method = "lm", lwd = 0.8, se = F, color = "black")+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(color = "black"))+
  labs( y = "Total MPs")+
  annotate("text", x = 68, y = 495, label = "P = 0.00")+
  annotate("text",x = 68, y = 490, label = "R^2 = 1.00")+
  annotate("text", x = 68, y = 485, label = " y = 5.65x + 68.74")

ggsave("BWMPsNew.png", bw, dpi = 1000, height = 5, width = 6)



# GL and MPS in DT --------------------------------------------------------

clw <- read_excel("ShuvoMP.xlsx", sheet = "CLW")
variable.names(clw)
dt <- lm(`Total MPs in DT` ~ `Digestive tract weight(gm)`, clw)
summary(dt)


DT <- clw %>% 
  ggplot(aes(`Digestive tract weight(gm)`, `Total MPs in DT`, color = Station))+
  geom_point(size = 4, alpha = 0.8)+
  geom_smooth(method = "lm", lwd = 0.8, se = F, color = "black")+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(color = "black"))+
  labs( y = "MPs")+
  annotate("text", x = 8.44, y = 250, label = "P = 0.009")+
  annotate("text",x = 8.44, y = 245, label = "R^2 = 0.322")+
  annotate("text", x = 8.44, y = 240, label = " y = 131.44x - 883.43")


ggsave("DTMPsNew.png", DT, dpi = 1000, height = 5, width = 6)




# GL and mps in GL --------------------------------------------------------

clw <- read_excel("ShuvoMP.xlsx", sheet = "CLW")
variable.names(clw)
gl <- lm(`Total MPs in Gills` ~ `Gill weight (gm)`, clw)
summary(gl)


GL <- clw %>% 
  ggplot(aes(`Gill weight (gm)`, `Total MPs in Gills`, color = Station))+
  geom_point(size = 4, alpha = 0.8)+
  geom_smooth(method = "lm", lwd = 0.8, se = F, color = "black")+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(color = "black"))+
  labs( y = "MPs")+
  annotate("text", x = 4.4, y = 240, label = "P = 0.00")+
  annotate("text",x = 4.4, y = 235, label = "R^2 = 0.775")+
  annotate("text", x = 4.4, y = 230, label = " y = 22.809x + 114.80")

ggsave("GLMPsNew.png", GL, dpi = 1000, height = 5, width = 6)



# correlation -------------------------------------------------------------


library("PerformanceAnalytics")

cor <- clw %>% 
  select(2:9)

chart.Correlation(cor, histogram = T)





# size data of gills ------------------------------------------------------

SG <- read_excel("shuvoMP.xlsx", sheet = "MPsSG")

sizegl <- SG %>% 
  pivot_longer(cols = 3:7, names_to = "Size", values_to = "Value") %>% 
  ggplot(aes(Type, Value, fill = Size))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~Station)+
  theme_bw()+
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(colour = "black"))+
  labs( y = "Percentage (%)")+
  scale_fill_npg()

ggsave("MPsSizeGill.pdf", sizegl, dpi = 1000, height = 7, width = 9 )



DT<- read_excel("shuvoMP.xlsx", sheet = "MPsDT")
names(DT)
sizeDT <- DT %>% 
  pivot_longer(cols = 3:7, names_to = "Size", values_to = "Value") %>% 
  ggplot(aes(Type, Value, fill = Size))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~Station)+
  theme_bw()+
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(colour = "black"))+
  labs( y = "Percentage (%)")+
  scale_fill_npg()

ggsave("MPsSizeDT.pdf", sizeDT, dpi = 1000, height = 7, width = 9 )






























  

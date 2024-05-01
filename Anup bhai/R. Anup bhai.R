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
library(GGally)
library(PerformanceAnalytics)
library(webr)
library(plotly)
library(stats)

anup <- read_excel("AnupTP.xlsx", sheet = "Working")


df <- anup %>% 
  select(5:6, 9:12) %>% 
  drop_na() %>% 
  pivot_longer(cols = 3:6, names_to = "Size", values_to = "Value") %>% #View()
  filter(! .Station == "Tabalchari Bazar (St-3)" | !Type == "Film") %>%  #View()
  group_by( .Station , Type) %>%  
 mutate(percentage = (Value / sum(Value)*100))

#8.7+56.5+30.4+4.35

desired_Type <- c("Fiber", "Fragment", "Film")
df$Type <- factor(df$Type, levels = desired_Type)

desired_size <- c("100-500 (µm)", "500-1500 (µm)", "1500-3000 (µm)", "3000-5000 (µm)")
df$Size <- factor(df$Size, levels = desired_size)
glimpse(df)

library(scales)

#View(df)
stacked <- df %>% 
  ggplot(aes(Type , percentage, fill = Size)) +
  geom_bar(stat = "identity")+
  facet_wrap(~.Station, scales = "free_x")+
  theme(panel.background = element_blank(),
        axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(colour = "black"),
        
        )+
  labs(x = "Microplastic Type", y = "Percentage (%)")+
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 0.1, suffix = "%"))

ggsave("stacked_size_F.png", stacked, height = 6, width = 8, dpi = 1000)

anup %>% 
  select(5:12) %>% 
  drop_na() %>% 
  group_by(.Station) %>% 
  mutate(percentage = (MPs...7/ sum(MPs...7)*100)) %>% View()
  #View()
  ungroup() %>% 
  PieDonut(aes(.Station, Type, count= MPs...7))

  

# avg ---------------------------------------------------------------------
 
anup %>% 
    select(5:6, 8:12) %>% 
    drop_na() %>% 
    group_by(.Station) %>% 
    #mutate(percentage = (.avg/ sum(.avg)*100)) %>% View()
  #View()
  ungroup() %>% 
    PieDonut(aes(.Station, Type, count = .avg))
  
anup <- read_excel("AnupTP.xlsx", sheet = "Working")


anup_box <- anup %>% 
  select(12:14) %>% 
  pivot_longer(cols = 1:3, names_to = "Station", values_to = "Value")

model <- aov(data = anup_box, Value ~ Station)
summary(model)
tukey <- TukeyHSD(model)
plot(tukey)

emmean <- emmeans(model, specs = "Station")
emmean_cld <- cld(emmean, Letters = letters)

ab <- anup_box %>% 
  ggplot(aes(Station, Value, fill = Station))+
  geom_boxplot(alpha = 1, show.legend = F, outlier.shape = NA )+
  geom_jitter(alpha = 1, size = 2, show.legend = F)+
  theme_classic()+
  theme(axis.title = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black")
        )+
  geom_text(data = emmean_cld, aes(x = Station, y = upper.CL, label = .group),
            size = 7, hjust = 0.2, vjust = 0.2, show.legend = F)+
  labs(y = "Number of Microplastic", x = "Sampling Station")

  scale_fill_npg()

ggsave("AnupBox_fill.png", ab, dpi = 2000, height = 6, width = 5)








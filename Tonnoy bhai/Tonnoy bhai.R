
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
ggpairs(iris, aes(color = Species))



?ggpairs

# Body weight -------------------------------------------------------------

data <- read_excel("BodyWeight.xlsx", sheet = "Body weight")

data %>% 
  ggplot(aes(Category, Value  fill = Category))+
  geom_boxplot()+
  facet_wrap(~Type, scales = "free_y")

an <- data %>% 
  filter(! Value == 0)

anova <- aov(Value~Category,  data)
summary(anova)

tukey <- TukeyHSD(anova)
plot(tukey)


emmean <- emmeans(anova, specs = "Category")
emmean_cld <- cld(emmean, Letters = letters)

body <- data %>% 
  ggplot(aes(Category, Value, fill = Category))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha = 1)+
  geom_jitter(size = 1, show.legend = F, alpha = 1)+
  geom_text(data = emmean_cld, aes(Category, upper.CL, label = .group),
            color = "black", vjust = 0.5, hjust = 0.6)+
  theme(panel.background = element_blank(),
        axis.title = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black", face = "italic")
        )+
  labs(x = "Species", y = "MPs/body")+
  coord_flip()
  
  
ggsave("body_box.png", body, dpi = 1500, width = 6, height = 8)




# GUT ------------------------------------------------------------

gut <- read_excel("BodyWeight.xlsx", sheet = "GW")

data <- gut %>% 
  pivot_longer(cols =1:7, names_to = "Category", values_to = "Value")


anova <- aov(Value~Category,  data)
summary(anova)

tukey <- TukeyHSD(anova)
plot(tukey)

emmean <- emmeans(anova, specs = "Category")
emmean_cld <- cld(emmean, Letters = letters)

 gut <- data %>% 
  ggplot(aes(Category, Value, fill = Category))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha = 1)+
  geom_jitter(size = 1, show.legend = F, alpha = 1)+
  geom_text(data = emmean_cld, aes(Category, upper.CL, label = .group),
            color = "black", vjust = -0.5, hjust = 0.2)+
  theme(panel.background = element_blank(),
        axis.title = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black", face = "italic")
        
  )+
  labs(x = "Species", y = "MPs/Gut")

ggsave("Mps_gut.png", gut, dpi = 1500, width = 7, height = 6)



# MPS ---------------------------------------------------------------------


Micro <- read_excel("BodyWeight.xlsx", sheet = "Micro")

data <- Micro %>% 
  pivot_longer(cols =1:7, names_to = "Category", values_to = "Value")


anova <- aov(Value~Category,  data)
summary(anova)

tukey <- TukeyHSD(anova)
plot(tukey)

emmean <- emmeans(anova, specs = "Category")
emmean_cld <- cld(emmean, Letters = letters)

Microbox <- data %>% 
  ggplot(aes(Category, Value, fill = Category))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha = 1)+
  geom_jitter(size = 1, show.legend = F, alpha = 1)+
  geom_text(data = emmean_cld, aes(Category, upper.CL, label = .group),
            color = "black", vjust = -1, hjust = 0.3)+
  theme(panel.background = element_blank(),
        axis.title = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black", face = "italic")
        
  )+
  labs(x = "Species", y = "MPs")

ggsave("Micro_box.png", Microbox, dpi = 1500, width = 7, height = 6)




# BLG ---------------------------------------------------------------------

blg <- read_excel("BodyWeight.xlsx", sheet = "BLG")

blg$`Total Length (cm)` <- as.numeric(blg$`Total Length (cm)`)
blg$`Body Weight (g)` <- as.numeric(blg$`Body Weight (g)`)
blg$`Gut Weight (g)` <- as.numeric(blg$`Gut Weight (g)`)
blg$Category <- as.factor(blg$Category)


bglmbox <- blg %>% 
  pivot_longer(2:5, names_to = "length", values_to = "value") %>% 
  ggplot(aes(Category, value, fill = Category ))+
  geom_boxplot(outlier.shape = NA, show.legend = F)+
  #geom_jitter(show.legend = F, size = 1, alpha = 0.5 )+
  #geom_jitter(size = 1)+
  facet_wrap(~length, scales = "free_y")+
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black")
        )

ggsave("bglm.png", bglmbox, dpi = 1500, width = 8, height = 7)

# Linear model with eqn. r. p value ---------------------------------------

model <- lm(blg$MPs ~ blg$`Body Weight (g)`, blg)
summary(model)
rs <- round((summary(model)$r.squared), 3)
pv <- round((summary(model)$coefficients[2,4]), 3)

eqn <- 'y = 0.03x + 5.29'
c <- c(eqn = eqn, "R^2" = rs, pv = pv)
mpbw <- blg %>% 
  ggplot(aes(`Body Weight (g)`, MPs, color = Category))+
  geom_point(size = 3, alpha = 1)+
  geom_smooth(method = "lm", se = F, lwd = 0.8, color = "black")+
  theme(panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank()
        )
  labs(color = "Species")+
  geom_text(data = model, 
            aes(x = max(blg$`Body Weight (g)`), y = max(blg$MPs),
                        label = paste("y =", round(as.numeric(coefficients(model)[2]), 3),
                                                   "x+", round(as.numeric(coefficients(model)[1]), 3))),
                        hjust = 1, vjust = -0.7, color = "black")

ggsave("Regression of BW vs MPs.pdf", mpbw, dpi = 3000, width = 8, height = 7)




# Regression of Gut weight vs MPs  ----------------------------------------

model <- lm(blg$MPs ~ blg$`Gut Weight (g)`, blg)
summary(model)
rs <- round((summary(model)$r.squared), 3)
pv <- round((summary(model)$coefficients[2,4]), 3)

eqn <- 'y = 0.54x + 5.25'
c <- c(eqn = eqn, "R^2" = rs, pv = pv)
mpgw <- blg %>% 
  ggplot(aes(`Gut Weight (g)`, MPs, color = Category))+
  geom_point(size = 3, alpha = 1)+
  geom_smooth(method = "lm", se = F, lwd = 0.8, color = "black")+
  theme(panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank()
  )+
  labs(color = "Species")+
  geom_text(data = model, 
            aes(x = max(blg$`Gut Weight (g)`), y = max(blg$MPs),
                label = paste("y =", round(as.numeric(coefficients(model)[2]), 3),
                              "x+", round(as.numeric(coefficients(model)[1]), 3))),
            hjust = 1, vjust = +3, color = "black")

ggsave("Regression of GW vs MPs.pdf", mpgw, dpi = 3000, width = 8, height = 7)



# coorelation plot of this ------------------------------------------------

library("PerformanceAnalytics")
coor <- blg %>% 
  select(2:5)

 chart.Correlation(coor, histogram = T, pch= 19)
 

# bar plot of feeding zone ------------------------------------------------
bar <- read_excel("BodyWeight.xlsx", sheet = "Sheet4")

barc <- bar %>% 
  ggplot(aes(Type, value, fill = Type))+
  geom_bar(stat = "identity", show.legend = F, width = 0.6)+
  geom_errorbar(aes(ymin = value-std, ymax = value+std),
                size = 0.5, width = 0.2)+
  #geom_text(aes(label = label, vjust= ))+
  theme(panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(colour = "black")
        )+
  labs(x = "Feeding Zone", y = "Average MPs")

ggsave("feeding zone.png", barc, dpi = 1500, width = 6, height = 6)




# New analysis with color and shape data ----------------------------------
library(readxl)
bar <- read_excel("BodyWeight.xlsx", sheet = "Sheet4")

# pie donut with size -----------------------------------------------------


bar %>% 
  select(6:14) %>% 
  pivot_longer(cols =3:9, names_to = "Species", values_to = "Value") %>% 
  group_by(Species, Category) %>% 
  mutate(Percent = Value/(sum(Value))*100) %>% 
  filter(Category %in% "Size") %>% 
  PieDonut(aes(Species, Color, count = Value))

# pie donut with color ----------------------------------------------------

bar %>% 
  select(6:14) %>% 
  pivot_longer(cols =3:9, names_to = "Species", values_to = "Value") %>% 
  group_by(Species, Category) %>% 
  mutate(Percent = Value/(sum(Value))*100) %>% 
  filter(Category %in% "Color") %>% 
  PieDonut(aes(Species, Color, count = Value))

# Stacked bar with size ---------------------------------------------------

stackedsize <- bar %>% 
  select(6:14) %>% 
  pivot_longer(cols =3:9, names_to = "Species", values_to = "Value") %>% 
  group_by(Species, Category) %>% 
  mutate(Percent = Value/(sum(Value))*100) %>% 
  filter(Category %in% "Size") %>% 
  ggplot(aes(Species, Percent, fill = Color)) +
  geom_bar(stat = "identity")+
  theme(panel.background = element_blank(),
        axis.title.y = element_text(colour = "black"),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(colour = "black", size = 9),
        axis.text.x = element_text(face = "italic")
  )+
  labs(y = "Percentage (%)", fill = "Size")+
  
  #scale_fill_d3() 
  #scale_fill_cosmic()
  #scale_fill_aaas()
  scale_fill_npg()
#scale_fill_jco()
ggsave("S.Size.png", stackedsize, dpi = 1500, height = 7, width = 8)


# Stacked bar by type -----------------------------------------------------

stackedtype <- bar %>% 
  select(6:14) %>% 
  pivot_longer(cols =3:9, names_to = "Species", values_to = "Value") %>% 
  group_by(Species, Category) %>% 
  mutate(Percent = Value/(sum(Value))*100) %>% 
  filter(Category %in% "Type") %>% 
  ggplot(aes(Species, Percent, fill = Color)) +
  geom_bar(stat = "identity")+
  theme(panel.background = element_blank(),
        axis.title.y = element_text(colour = "black"),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(colour = "black", size = 9),
        axis.text.x = element_text(face = "italic")
        )+
  labs(y = "Percentage (%)", fill = "Type")+

  #scale_fill_d3() 
  #scale_fill_cosmic()
  #scale_fill_aaas()
  scale_fill_npg()
  #scale_fill_jco()
ggsave("S.Type.png", stackedtype, dpi = 1500, height = 7, width = 8)

# Stacked bar by color ----------------------------------------------------


stackedcolor <- bar %>% 
  select(6:14) %>% 
  pivot_longer(cols =3:9, names_to = "Species", values_to = "Value") %>% 
  group_by(Species, Category) %>% 
  mutate(Percent = Value/(sum(Value))*100) %>% 
  filter(Category %in% "Color") %>% 
  ggplot(aes(Species, Percent, fill = Color)) +
  geom_bar(stat = "identity")+
  theme(panel.background = element_blank(),
        axis.title.y = element_text(colour = "black"),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(colour = "black", size = 10),
        axis.text.x = element_text(face = "italic")
  )+
  labs(y = "Percentage (%)")+
  
  scale_fill_d3() 
  #scale_fill_cosmic()
  #scale_fill_aaas()
  #scale_fill_npg()
#scale_fill_jco()

ggsave("S.color.png", stackedcolor, dpi = 1500, height = 7, width = 8)





ploy <- read_excel("BodyWeight.xlsx", sheet = "PlasticType")

ploy %>% 
  pie(Percentage, labels = Type)


?pie()

ploy %>% 
  ggplot(aes(Type, Percentage, fill = Type))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_polar(theta = "x")


pie(ploy$Percentage, labels = ploy$Type)



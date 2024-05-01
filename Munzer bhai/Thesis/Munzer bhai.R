# Munzer bhai---------------------------------------------------------
library(readxl)
library(writexl)
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
library(plotly)
library(plot3D)
library(plotrix)
library(vegan)
library(palmerpenguins)
library(tidyverse)
library(ggstatsplot)
library(rstantools)


#Tree graph showing classification
library(dendextend)
mydata <- read_excel("Index.xlsx", sheet = "Index", )


#3D pie chart

phylum <- mydata %>% 
  select(2:9) %>% 
  count(Phylum) %>% 
  mutate(percentage= (n/(sum(n))*100))


pie3D(phylum$percentage, labels = txdata$Phylum, explode = 0.15,
      start = 0.7, shade = 0.45, col = 2:9)



class <- mydata %>% 
  select(2:9) %>% 
  count(Class) %>% 
  mutate(percentage= (n/(sum(n))*100))


pie3D(class$percentage, labels = class$Class, explode = 0.15,
      start = 2.5, shade = 0.45, col = 2:8)



subclass <- mydata %>% 
  select(2:9) %>% 
  count(Subclass) %>% 
  mutate(percentage= (n/(sum(n))*100))


pie3D(subclass$percentage, labels = subclass$Subclass, explode = 0.15,
      start = 1, shade = 0.4, col = 2:7)


order <- mydata %>% 
  select(2:9) %>% 
  count(Order) %>% 
  mutate(percentage= (n/(sum(n))*100))

pie3D(order$percentage, labels = order$Subclass, explode = 0.15,
      start = 1, shade = 0.4, col = 2:7)

order %>% 
  ggplot(aes(x = Order, y = (n), fill = Order))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_polar(theta = "x", start = -2)+
  ylim(-2, 4)+
  theme(panel.background = element_blank(),
        panel.grid =  element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", angle = 0, size = 7.5),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank())+
  geom_text(aes(label = n))




family <- mydata %>% 
  select(2:9) %>% 
  count(Family) %>% 
  mutate(percentage= (n/(sum(n))*100))

pie3D(order$percentage, labels = order$Subclass, explode = 0.15,
      start = 1, shade = 0.4, col = 2:7)

family %>% 
  ggplot(aes(x = Family, y = (n), fill = Family))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_polar(theta = "x", start = -2)+
  ylim(-2, 8)+
  theme(panel.background = element_blank(),
        panel.grid =  element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", angle = 0, size = 7.5),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank())+
  geom_text(aes(label = n))


corr <- mydata %>% 
  select(10:13) %>% 
  chart.Correlation(histogram = T, pch = 10)
  
  



library(vegan)

datad <- mydata %>%
  select(9:13) %>% 
  filter( !Species %in% c("Penaeus monodon" ,"Macrobrachium rosenbergii"))
#simpson index

simpson_winter <- round(diversity(datad$AVGW, index = "simpson"), 3)
simpson_winter
simpson_pre <- round(diversity(datad$AVGPre, index = "simpson"), 3)
simpson_pre
simpson_m <- round(diversity(datad$AVGM, index = "simpson"), 3)
simpson_m
simpson_post <- round(diversity(datad$AVGPost, index = "simpson"), 3)
simpson_post


simpson_D_diversity <- c(simpson_winter, simpson_pre, simpson_m, simpson_post)
simpson_D_1_diversity <- c(1-simpson_winter, 1-simpson_pre, 1-simpson_m, 1-simpson_post)
# invasimp


invasimp_winter <- round(diversity(datad$AVGW, index = "invsimpson"), 3)
invasimp_winter
invasimp_pre <- round(diversity(datad$AVGPre, index = "invsimpson"), 3)
invasimp_pre
invasimp_m <- round(diversity(datad$AVGM, index = "invsimpson"), 3)
invasimp_m
invasimp_post <- round(diversity(datad$AVGPost, index = "invsimpson"), 3)
invasimp_post

invasimpson <- c(invasimp_winter, invasimp_pre, invasimp_m, invasimp_post)


#unbias

unbias_simp_winter <- round(1 - 1/(simpson_winter), 3)
unbias_simp_winter
unbias_simp_pre <- round(1 - 1/(simpson_pre),3)
unbias_simp_pre
unbias_simp_m <- round(1 - 1/(simpson_m),3)
unbias_simp_m
unbias_simp_post <- round(1 - 1/(simpson_post),3)
unbias_simp_post


simpson_unbias <- c(unbias_simp_winter, unbias_simp_pre, unbias_simp_m, unbias_simp_post)

#install.packages("breakaway")
#devtools::install_github("joey711/phyloseq")
library(breakaway)

Shannon_winter <- round(diversity(datad$AVGW, index = "shannon"), 3)
Shannon_winter
Shannon_pre <- round(diversity(datad$AVGPre, index = "shannon"), 3)
Shannon_pre
Shannon_m <- round(diversity(datad$AVGM, index = "shannon"), 3)
Shannon_m
Shannon_post <- round(diversity(datad$AVGPost, index = "shannon"), 3)
Shannon_post

shanon_diversity <- c( Shannon_winter, Shannon_pre, Shannon_m, Shannon_post)



#species richness
library(vegan)
spr_winter <- specnumber(datad$AVGW)
spr_winter
spr_pre <- specnumber(datad$AVGPre)
spr_pre
spr_m <- specnumber(datad$AVGM)
spr_m
spr_post <- specnumber(datad$AVGPost)
spr_post

species_richness <- c(spr_winter, spr_pre, spr_m, spr_post)

#Margalef's index

mar_winter <- ((spr_winter-1)/log(sum(datad$AVGW)))
mar_winter
mar_pre <- ((spr_pre-1)/log(sum(datad$AVGPre)))
mar_pre
mar_m <- ((spr_m-1)/log(sum(datad$AVGM)))
mar_m
mar_post <- ((spr_post-1)/log(sum(datad$AVGPost)))
mar_post

margalef_index <- round(c(mar_winter, mar_pre, mar_m, mar_post), 3)

#Pielous evenness index

pie_winter <- Shannon_winter/log(spr_winter)
pie_winter
pie_pre <- Shannon_pre/log(spr_pre)
pie_pre
pie_m <- Shannon_m/log(spr_m)
pie_m
pie_post <- Shannon_post/log(spr_post)
pie_post

pie_index <- round(c(pie_winter, pie_pre, pie_m, pie_post), 3)


#fisher alpha
fisher_w <-  round(fisher.alpha(round(datad$AVGW)), 3)
fisher_w
fisher_pre <-  round(fisher.alpha(round(datad$AVGPre)), 3)
fisher_pre
fisher_m <-  round(fisher.alpha(round(datad$AVGM)),3)
fisher_m
fisher_post <-  round(fisher.alpha(round(datad$AVGPost)),3)
fisher_post

fisher_alpha <- c(fisher_w, fisher_pre, fisher_post, fisher_post)


#View(mydata)

datad

toal_ind <- datad %>% 
  summarise(total_w = sum(AVGW),
            total_pre = sum(AVGPre),
            total_m = sum(AVGM),
            total_post = sum(AVGPost))


all_diversity <- data.frame(simpson_D_diversity, simpson_D_1_diversity, shanon_diversity,
                            margalef_index, pie_index,
                            invasimpson, simpson_unbias, species_richness, fisher_alpha)

rownames(all_diversity) <- c("Winter", "Pre_monsoon", "Monsoon", "Post_monsoon")
library(writexl)

write.xlsx(all_diversity, "diversityt.xlsx", )



diversitydata <- read_excel("diversityt.xlsx")

#doesn't work
diversitydata %>% 
  select(1:5) %>% 
  #filter(! Index == "Individuals") %>% 
  filter(! Index == "species_richness") %>% 
  filter(! Index == "fisher_alpha") %>% 
  pivot_longer( cols = 2:5,
                values_to = "value",
                names_to = "season") %>% 
  ggplot(aes(Index, value, col = season))+
  geom_boxplot()
  
  #facet_wrap(~Index, scales = "free_y")


#barplot with diversity data

barplot <- diversitydata %>% 
  select(1:5) %>% 
  slice(1:4, 5) %>% 
  pivot_longer(cols = 2:5, names_to = "season", values_to = "value") %>% 
  group_by(Index) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  ggplot(aes(reorder(Index, -mean), mean, fill = Index))+
  geom_bar(stat = "identity", position = "dodge", show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                size = 0.8, width = 0.3, position = "dodge")+
  theme_bw()

ggsave("D_barplot.pdf", barplot, dpi = 800, width = 6, height = 5)

#all barplot

 diversitydata %>% 
  select(1:6) %>% #View()
  #slice(1:4, 5) %>% 
  pivot_longer(cols = 2:6, names_to = "index", values_to = "value") %>%# View()
  #group_by(Index) %>% 
  #summarise(mean = mean(value),
   #         sd = sd(value)) %>% 
  ggplot(aes(index, value, fill = Season))+
  geom_bar(stat = "identity", position = "dodge", show.legend = T)+

  #geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                #size = 0.8, width = 0.3, position = "dodge")+
  scale_fill_futurama()+
  theme_bw()+
  geom_text(aes(label = round(value,2), x = index), vjust = -1, position = position_dodge(width = 0.9) )+
  theme(legend.position = c(0.9, 0.9), legend.justification = c(1, 1))


ggsave("alldbar.pdf", alldbar, dpi = 800, width = 8, height = 5)





#boxplot

dobxplot <- diversitydata %>% 
  select(1:5) %>% 
  slice(1:4, 5) %>% 
  pivot_longer(cols = 2:5, names_to = "season", values_to = "value") %>% 
  ggplot(aes(Index, value, fill = Index))+
  geom_boxplot(show.legend = F, outlier.shape = NA, alpha = 0.8)+
  theme_bw()
  
ggsave("D_boxplot.pdf", dobxplot, dpi = 800, width = 6, height = 5)




#appendix data winter

appendix_winter <- datad %>% 
  select(1:2) %>% 
  filter(!AVGW ==0) %>% 
 mutate(Pi = AVGW/sum(AVGW),
        lnPi = log(Pi),
        Pi_lnPi = Pi*lnPi,
        Pi_square = Pi^2
        )

#appendix data pre_

appendix_pre <- datad %>% 
  select(1, 3) %>% 
  filter(!AVGPre ==0) %>% 
  mutate(Pi = AVGPre /sum(AVGPre),
         lnPi = log(Pi),
         Pi_lnPi = (Pi*lnPi),
         Pi_square = (Pi^2)
  )

#appendix data m_

appendix_m <- datad %>% 
  select(1, 4) %>% 
  filter(!AVGM == 0) %>% 
  mutate(Pi = AVGM /sum(AVGM),
         lnPi = log(Pi),
         Pi_lnPi = Pi*lnPi,
         Pi_square = Pi^2
  )

#appendix data post_

appendix_post <- datad %>% 
  select(1, 5) %>% 
  filter(!AVGPost == 0) %>% 
  mutate(Pi = AVGPost /sum(AVGPost),
         lnPi = log(Pi),
         Pi_lnPi = Pi*lnPi,
         Pi_square = Pi^2
  )


# making excel file with multiple sheet

write_xlsx(list("Sheet1" = appendix_winter,
                "Sheet2" = appendix_pre,
                "Sheet3" = appendix_m,
                "Sheet4" = appendix_post
                 ), "AllAppendixx.xlsx"
           )







#subset(mydata, (mydata$AVGW) > 0)


?diversity()

diversity_ind <- 1 - (sum(mydata$AVGW*(mydata$AVGW-1))/(sum(mydata$AVGW*(sum(mydata$AVGW-1)))))

#working with master sheet
mdata <- read_excel("Index.xlsx", sheet = "Mastersheet")




#Phylum wise
pdata <- mdata %>% 
  select(2, 17) %>% 
  group_by(Phylum) %>% 
  summarise( totalp = sum(SubTotal)) %>% 
  ungroup() %>% 
  mutate(perp = totalp/sum(totalp)*100)

library(plotrix)

#Class wise
cdata <- mdata %>% 
  select(4, 17) %>% 
  group_by(Class) %>% 
  summarise( clas = sum(SubTotal)) %>% 
  ungroup() %>% 
  mutate(perc = clas/sum(clas)*100)

  #pie3D(cl$clas, labels = cl$Class, explode = 0.15, start =2 , shade = 0.45, col = 2:8)
  
  
  
 

#Order wise
odata <- mdata %>% 
  select(6, 17) %>% 
  group_by(Order) %>% 
  summarise( ord = sum(SubTotal)) %>% 
  ungroup() %>% 
  mutate(pero = ord/sum(ord)*100)


#PFamily wise
fdata <- mdata %>% 
  select(7, 17) %>% 
  group_by(Family) %>% 
  summarise( totalf = sum(SubTotal)) %>% 
  ungroup() %>% 
  mutate( perf = totalf/ sum(totalf)*100)

#genus wise
gdata <- mdata %>% 
  select(8, 17) %>% 
  group_by(Genus) %>% 
  summarise(totalg = sum(SubTotal)) %>% 
  ungroup() %>% 
  mutate(perg = totalg/sum(totalg)*100)

#species wise
sdata <- mdata %>% 
  select(9, 17) %>% 
  group_by(Species) %>% 
  summarise(totals = sum(SubTotal)) %>% 
  ungroup() %>% 
  mutate(pers = totals/sum(totals)*100)

library(writexl)


write_xlsx(list(
  "sheet1" = pdata,
  "sheet2" = cdata,
  "sheet3" = odata,
  "sheet4" = fdata,
  "sheet5" = gdata,
  "sheet6" = sdata
                ),
  "PercentageData.xlsx"
  )


write_xlsx(list("Sheet1" = appendix_winter,
                "Sheet2" = appendix_pre,
                "Sheet3" = appendix_m,
                "Sheet4" = appendix_post
), "AllAppendixx.xlsx"
)



###################


mdata %>% 
  group_by(Phylum, Class, Order) %>% 
  summarise(value = sum(SubTotal)) %>% 
  ggplot(aes(Class, value, fill = Order))+
  geom_bar(stat = "identity", , position = "dodge")+
  facet_wrap(~Class, scales = "free_y")
  
  
  
  PieDonut(aes(Phylum, Class, count = value))


#######################

#calculating shrimp family percentage
mdata %>% 
    select(6:8, 18) %>% 
    filter( Code == 2) %>% 
    mutate( percentage = round((SubTotal/sum(SubTotal)*100),3)) %>% 
    group_by(Family) %>% 
    summarise( percentage = sum(percentage))
    

#################

#calculating finfish family percentage

  mdata %>% 
    select(6:8, 18) %>% 
    filter( Code == 3) %>% 
    mutate( percentage = round((SubTotal/sum(SubTotal)*100),3)) %>% 
    group_by(Family) %>% 
    summarise( percentage = sum(percentage)) %>% 
    View()



  
#########################
  
totalcatch <- sum(mdata$SubTotal)
mdata %>% 
    select(6:8, 18) %>% 
    filter( Family == "Palaemonidae") %>% 
    mutate( percentage = round((SubTotal/totalcatch*100),3)) %>% 
    group_by(Family) %>% 
    summarise( percentage = sum(percentage)) %>% 
    View()



#correlation analysis

cordata <- read_excel("CorrelationAnalysis.xlsx")



cordata %>% 
  select(2:6) %>% 
  chart.Correlation(histogram = T, pch = 200)


x <- cordata$Salinity
y <- cordata$Temperature 

correlation <- cor(x,y)
cor_test <- cor.test(x,y)





###############################





Group <- c("Benthos","Benthos","Benthos","Benthos","Benthos","Benthos","Zooplankton","Zooplankton","Zooplankton","Zooplankton",
           "Zooplankton","Zooplankton","Fish","Fish","Fish","Fish","Fish","Fish","Phytoplankton","Phytoplankton","Phytoplankton","Phytoplankton")
Domain <- rep("Eukaryota", length(Group))
Kingdom <- c(rep("Animalia", 18), rep("Chromalveolata", 4))
Phylum <- c("Annelida","Annelida","Arthropoda","Arthropoda","Porifera","Sipunculida","Arthropoda","Arthropoda","Arthropoda",
            "Arthropoda","Echinoidermata","Chorfata","Chordata","Chordata","Chordata","Chordata","Chordata","Chordata","Heterokontophyta",
            "Heterokontophyta","Heterokontophyta","Dinoflagellata")
Class <- c("Polychaeta","Polychaeta","Malacostraca","Malacostraca","Demospongiae","NA","Malacostraca","Malacostraca",
           "Malacostraca","Maxillopoda","Ophiuroidea","Actinopterygii","Chondrichthyes","Chondrichthyes","Chondrichthyes","Actinopterygii",
           "Actinopterygii","Actinopterygii","Bacillariophyceae","Bacillariophyceae","Prymnesiophyceae","NA")
Order <- c("NA","NA","Amphipoda","Cumacea","NA","NA","Amphipoda","Decapoda","Euphausiacea","Calanioda","NA","Gadiformes",
           "NA","NA","NA","NA","Gadiformes","Gadiformes","NA","NA","NA","NA")                     
Species <- c("Nephtys sp.","Nereis sp.","Gammarus sp.","Diastylis sp.","Axinella sp.","Ph. Sipunculida","Themisto abyssorum","Decapod larvae (Zoea)",
             "Thysanoessa sp.","Centropages typicus","Ophiuroidea larvae","Gadus morhua eggs / larvae","Etmopterus spinax","Amblyraja radiata",
             "Chimaera monstrosa","Clupea harengus","Melanogrammus aeglefinus","Gadus morhua","Thalassiosira sp.","Cylindrotheca closterium",
             "Phaeocystis pouchetii","Ph. Dinoflagellata")   
dat <- data.frame(Group, Domain, Kingdom, Phylum, Class, Order, Species)
dat

dat <- mydata %>% 
  select(1:7)


# Data: remove Group
#dat <- data.frame(Domain, Kingdom, Phylum, Class, Order, Species)

# Start a new plot
par(mar=c(0,0,0,0))
plot(NA, xlim=c(0,ncol(dat)+1), ylim=c(0,nrow(dat)+1), 
     type="n", axes=FALSE, xlab="", ylab="", main="")

# Compute the position of each node and find all the edges to draw
positions <- NULL
links <- NULL
for(k in 1:ncol(dat)) {
  y <- tapply(1:nrow(dat), dat[,k], mean)
  y <- y[ names(y) != "NA" ]
  positions <- rbind( positions, data.frame(
    name = names(y),
    x = k,
    y = y
  ))
}
links <- apply( dat, 1, function(u) { 
  u <- u[ !is.na(u) & u != "NA" ]
  cbind(u[-length(u)],u[-1]) 
} )
links <- do.call(rbind, links)
rownames(links) <- NULL
links <- unique(links[ order(links[,1], links[,2]), ])

# Draw the edges
for(i in 1:nrow(links)) {
  from <- positions[links[i,1],]
  to   <- positions[links[i,2],]
  lines( c(from$x, from$x, to$x), c(from$y, to$y, to$y) )
}

# Add the text
text(positions$x, positions$y, label=positions$name, col = 2:8)





ndata <- read_excel("diversity.xlsx", sheet = "Sheet1")


ndata %>% 
  pivot_longer(cols = 2:5,
               names_to = "season", 
               values_to = "value") %>%
  PieDonut(aes( season, MajorGroups, count = value))
  



needdata <- ndata %>% 
  slice(-4) %>% 
  pivot_longer(cols = 2:5,
               names_to = "Seasons", 
               values_to = "Number")



needdata$Seasons <- factor(needdata$Seasons, levels = c("Winter", "Pre-monsoon", "Monsoon", "Post-monsoon"))

needdata %>%   
ggplot(aes(Seasons, Number, fill = MajorGroups))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_bw()+
  scale_fill_npg(alpha = 1)+
  theme(legend.position = c(0.7, 0.8),
        )


labels <- c("C", "A", "B", "D")
values <- c(30, 20, 25, 25)

# Create a dataframe
data <- data.frame(labels, values)

# Specify the desired order of categories
data$labels <- factor(data$labels, levels = c("D", "C", "A", "B"))

# Create the plot
p <- ggplot(data, aes(x = labels, y = values)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Value", title = "Bar Plot with Ordered Categories")

# Display the plot
print(p)




#last plot


winter <- c(6237, 6097, 838)
pre_monsoon <- c(3613, 2738, 4028)
monsoon <- c(835, 470, 236)
post_monsoon <- c(2872, 1531, 132)


#data <- data.frame(winter, pre_monsoon, monsoon, post_monsoon)

#rownames(data) <- c("Shrimp", "Others", "Fish",)
library(readxl)
data <- read_excel("datax.xlsx")

data <- data %>% 
  pivot_longer( cols = 2:5, names_to = "group", values_to = "value" )
#empty_bar <- 4

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 5
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)



# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-7000,7000) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)








empty_bar <- 1000
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=1) +
  ylim(-6300,6300) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
























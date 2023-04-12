#Author: Nik
#Last edited: 1/12/2023
#Disease data for Flutriafol Movement Paper

#set working directions
setwd(dir="D:/MSU/Master/Research/Thesis/2 Flutriafol Systemic Movement/")

#load package
library(readr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(multcompView)
library(RColorBrewer)
library(wesanderson)
library(carData)
library(car)
library(RColorBrewer)
library("emmeans")
library("multcomp")
library("multcompView")
library(readxl)

####Disease Data

#load disease rating datasets
Disease <- read_xlsx("D:/MSU/Master/Research/Thesis/2 Flutriafol Systemic Movement/Disease Data.xlsx")
Disease$Year <- factor(Disease$Year)
Disease$Rating_Leaf <- factor(Disease$Rating_Leaf) 
View(Disease)

#### Statistical Analyses

####2019 Data
#2019 FirstRating example p-value and LSD calculation
#create dataframe
Year.2019_Rating1 <- data.frame(Treatment=c("Untreated Check", "Untreated Check","Untreated Check","Untreated Check","Xyway LFR In-furrow","Xyway LFR In-furrow","Xyway LFR In-furrow","Xyway LFR In-furrow"),
                                Rating=c(0.6, 0.3, 0, 0.1, 0, 0, 0, 0.1))
# need to run ANOVA first and write the outcome of the model fitting
ANOVA.2019.Rating1 = lm(Rating ~ Treatment, data=Year.2019_Rating1 )
#LSD
LSD.2019.Rating1 <- emmeans(ANOVA.2019.Rating1, ~Treatment, adjust='none')     #write the means in a separate data set
pairs(LSD.2019.Rating1, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.2019.Rating1,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 


#### Graph

#subset data for 2021 data only, group by rating leaf and rating number
Rating2021 <- filter(Disease, Year %in% c("2021"), Disease %in% c("GLS")) 

#Disease Rating for 2021 only
Disease2021 <- ggplot(Rating2021, aes(x = Treatment,y = DIX,fill = Rating_Leaf)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  geom_col(position=position_dodge2(preserve = "single")) +
  ggtitle("Disease Rating in 2021") +
  xlab("") +
  ylab("Disease Severity Index") +
  scale_x_discrete(limits = c("UTC", "IF","2x2", "0x0")) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(size = 15, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 15, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif", angle = 90),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif")) 

ggsave(Disease2021, filename = "Disease Rating 2021.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)


####AUDPC 

#load AUDPC data
AUDPC <- read.csv("D:/MSU/Master/Research/Thesis/2 Flutriafol Systemic Movement/AUDPC 2020-2021.csv")
AUDPC$Year <- factor(AUDPC$Year)

#example: check significance of AUDPC
#create dataframe
AUDPCdata <- data.frame(Treatment=c("Untreated Check", "Untreated Check", "Untreated Check", "Untreated Check", "In-furrow","In-furrow", "In-furrow", "In-furrow", "2x2","2x2", "2x2", "2x2", "0x0", "0x0", "0x0", "0x0"),
                        AUDPC=c(2.55, 17.5, 12.982, 0, 
                                0, 0, 6.15, 0,
                                2.55, 0, 5.1, 5.15,
                                0, 0, 4.25, 1.7085))
# need to run ANOVA first and write the outcome of the model fitting
ANOVA.AUDPC = lm(AUDPC ~ Treatment, data=AUDPCdata )
#LSD
LSD.AUDPC <- emmeans(ANOVA.AUDPC, ~Treatment, adjust='none')            #write the means in a separate data set
pairs(LSD.AUDPC, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.AUDPC,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 


#AUDPC plot
#facetwrap for AUDPC data
## Mean and SE 
MeanSE.AUDPC <- AUDPC %>%
  group_by(Treatment, Year, Disease) %>%
  summarise(avg = mean(AUDPC),
            se = sd(AUDPC)/sqrt(length(AUDPC)))

##plotting AUDPC bar with added letter
plotAUDPC <- ggplot(MeanSE.AUDPC, aes(x = Disease, y = avg, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymax = avg + se, ymin = avg - se), position = position_dodge(width=0.9), width = 0.25) +
  #geom_text(aes(x = Year, y = avg + se, label = as.matrix(ascend.AUDPC$groups)), position = position_dodge(width = 0.9), vjust = -(0.5)) +
  xlab("") +
  ylab("Mean AUDPC") +
  scale_fill_manual(values=c("#e28743", "#eab676", "#76b5c5"), limits = c("Untreated Check", "Xyway In-furrow", "Xyway 2x2"))+
  scale_color_manual(values=c("#e28743", "#eab676", "#76b5c5"))+
  facet_wrap(~Year, ncol = 3) +
  theme(axis.text.x = element_text(size = 15, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 15, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 15, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 15, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        legend.position = c(0.5,0.8),
        strip.text.x = element_text(size = 10, face = "bold", family = "serif"),
        title = element_text(size = 16, family = "serif"))

ggsave(plotAUDPC, filename = "AUDPC Overall.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)


####Yield data

#example: check significance of yield data
#create dataframe
Yielddata <- data.frame(Treatment=c("Untreated Check", "Untreated Check", "Untreated Check", "Untreated Check", "In-furrow", "In-furrow", "In-furrow", "In-furrow"),
                        Yield=c(240.7276779,233.885586,244.5679645,288.1799357,
                                259.8164369,273.6119273,269.1973282,270.2839874))
# need to run ANOVA first and write the outcome of the model fitting
ANOVA.Yield = lm(Yield ~ Treatment, data=Yielddata )
#LSD
LSD.Yield <- emmeans(ANOVA.Yield, ~Treatment, adjust='none')            #write the means in a separate data set
pairs(LSD.Yield, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.Yield,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 


#Author: Nik
#Last edited: 1/12/2023
#Analyzing flutriafol movement in corn leaves


#set working directions
setwd(dir="D:/MSU/Master/Research/Thesis/2 Flutriafol Systemic Movement/")

#load package
library(readr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(multcompView)
library(wesanderson)
library(carData)
library(car)
library(RColorBrewer)
library("emmeans")
library("multcomp")
library("multcompView")
library(agricolae)
library(ggpubr)
library("patchwork")

#load datasets
Flutriafol <- read_csv("D:/MSU/Master/Research/Thesis/2 Flutriafol Systemic Movement/2019-2021 Data.csv", na = 'NA')
Flutriafol$Year <- factor(Flutriafol$Year)
Flutriafol$Treatment <- factor(Flutriafol$Treatment)

View(Flutriafol)

####ANOVA

#Two-way ANOVA for unbalanced data
result <- Anova(lm(Mean~Treatment*Year,data = Flutriafol), type="II")
result


#Two-way ANOVA to check if statistically significant btwn treatment
#If you think that these two variables might interact to create an synergistic effect, replace the plus symbol (+) by an asterisk (*)
model1 <- aov(lm(Mean~Treatment*Year, data=Flutriafol))
summary(model1)

#From the ANOVA results, we can conclude the following, based on the p-values and a significance level of 0.05:

#the p-value of treatment is < 2e-16 (significant), which indicates that the different treatment are associated with significant flutriafol conc.
#the p-value of year is 3.47e-08 (significant), which indicates that the different year are associated with significant flutriafol conc.
#the p-value for the interaction between Trt*Year is 0.0293 (significant), which indicates that the relationships between flutriafol conc and year depends on the trt method.


#perform LSD test for Treatment*Year interaction
LSD.model1 <- LSD.test(model1, c("Treatment", "Year"))
LSD.model1

# Original order of LSD$group for interaction
ascend.model1 = LSD.model1$groups %>%
  group_by(rownames(LSD.model1$groups)) %>%
  arrange(rownames(LSD.model1$groups))
ascend.model1

## Mean and SE for interaction effect (AxB)
MeanSE.model1 = Flutriafol %>%
  group_by(Treatment, Year) %>%
  summarise(avg = mean(Mean),
            se = sd(Mean)/sqrt(length(Mean)))

##plotting bar with added letter
plot <- ggplot(MeanSE.model1, aes(x = Year, y = avg, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymax = avg + se, ymin = avg - se), position = position_dodge(width=0.9), width = 0.25) +
  geom_text(aes(x = Year, y = avg + se, label = as.matrix(ascend.model1$groups)), position = position_dodge(width = 0.9), vjust = -(0.5)) +
  xlab("") +
  ylab("Mean Flutriafol Concentration (µg/mL)") +
  scale_fill_manual(values=wes_palette(n=4, name="Moonrise2"), labels = c("Lucento", "Xyway 0x0", "Xyway 2x2", "Xyway in-furrow"))+
  scale_color_manual(values=wes_palette(n=4, name="Moonrise2"), labels = c("Lucento", "Xyway 0x0", "Xyway 2x2", "Xyway in-furrow"))+
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
ggsave(plot, filename = "treatment.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)


#see significance between 2019 only
Control1 <- data_frame(Plant = c("Root", "Stalk",
                                "E-7", "E-6", "E-5", "E-4",
                                "E-3", "E-2", "E-1", #rearrange y axis
                                "E", "E+1", "E+2", "E+3", 
                                "E+4", "E+5", "E+6"),
                      Year = c("2019"), Treatment = c("Control"), Mean = c(0), SD = c(0))
Xyway2019 <- Flutriafol[Flutriafol$Year == "2019",]
Year2019 <- rbind(Control1, Xyway2019)
ANOVA.2019 = lm(Mean ~ Treatment, data=Year2019 )
summary(ANOVA.2019)
#LSD
LSD.2019 <- emmeans(ANOVA.2019, ~Treatment, adjust='none')     #write the means in a separate data set
pairs(LSD.2019, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.2019,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 


#see significance between 2020 only
Control2 <- data_frame(Plant = c("Root", "Stalk",
                                 "E-6", "E-5", "E-4",
                                 "E-3", "E-2", "E-1", #rearrange y axis
                                 "E", "E+1", "E+2", "E+3", 
                                 "E+4", "E+5", "E+6", "E+7", "E+8"),
                       Year = c("2020"), Treatment = c("Control"), Mean = c(0), SD = c(0))
Xyway2020 <- Flutriafol[Flutriafol$Year == "2020",]
Year2020 <- rbind(Control1, Xyway2020)
ANOVA.2020 = lm(Mean ~ Treatment, data=Year2020 )
summary(ANOVA.2020)
#LSD
LSD.2020 <- emmeans(ANOVA.2020, ~Treatment, adjust='none')     #write the means in a separate data set
pairs(LSD.2020, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.2020,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 



#see significance between 2021 only
Control3 <- data_frame(Plant = c("Root", "Stalk",
                                 "E-5", "E-4",
                                 "E-3", "E-2", "E-1", #rearrange y axis
                                 "E", "E+1", "E+2", "E+3", 
                                 "E+4", "E+5", "E+6", "E+7"),
                       Year = c("2021"), Treatment = c("Control"), 
                       Mean = c(0,0,0,0,0.129826441, 0.208609615, 0.198818149, 0, 0.096101135,
                                0.257904138, 0.867934083, 0.089872871,0.053189817,0.057793771,0.099682793), 
                       SD = c(0))
Xyway2021 <- Flutriafol[Flutriafol$Year == "2021",]
Year2021 <- rbind(Control1, Xyway2021)
ANOVA.2021 = lm(Mean ~ Treatment, data=Year2021 )
summary(ANOVA.2021)
#LSD
LSD.2021 <- emmeans(ANOVA.2021, ~Treatment, adjust='none')     #write the means in a separate data set
pairs(LSD.2021, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.2021,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 

#see significance for in-furrow only
Infurrow <- Flutriafol[Flutriafol$Treatment == "Xyway_infurrow",]
ANOVA.infurrow = lm(Mean ~ Year, data=Infurrow )
summary(ANOVA.infurrow)
#LSD
LSD.infurrow <- emmeans(ANOVA.infurrow, ~Year, adjust='none')     #write the means in a separate data set
pairs(LSD.infurrow, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.infurrow,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 

#see significance for 2x2 only
Xyway2x2 <- Flutriafol[Flutriafol$Treatment == "Xyway_2x2",]
ANOVA.xyway2x2 = lm(Mean ~ Year, data=Xyway2x2 )
summary(ANOVA.xyway2x2)
#LSD
LSD.xyway2x2 <- emmeans(ANOVA.xyway2x2, ~Year, adjust='none')     #write the means in a separate data set
pairs(LSD.xyway2x2, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.xyway2x2,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 


####Graph

#compare treatment in 2019 only
Data2019 <- Flutriafol[Flutriafol$Year == "2019",]
Treatment2019 <- ggplot(Data2019,                                     
                        aes(x = Mean,
                            y = Plant,
                            fill = Treatment)) +
  #geom_bar(stat = "identity",position = "dodge") +
  geom_col(position=position_dodge2(preserve = "single")) +
  scale_y_discrete(limits = c("Root", "Stalk",
                              "E-6", "E-5", "E-4",
                              "E-3", "E-2", "E-1", #rearrange y axis
                              "E", "E+1", "E+2", "E+3", 
                              "E+4", "E+5", "E+6")) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c("#eab676"))+
  scale_color_manual(values=c("#eab676"))+
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

#compare treatment in 2020 only
Data2020 <- Flutriafol[Flutriafol$Year == "2020",]
Treatment2020 <- ggplot(Data2020,                                     
                   aes(x = Mean,
                       y = Plant,
                       fill = Treatment)) +
  #geom_bar(stat = "identity",position = "dodge") +
  geom_col(position=position_dodge2(preserve = "single")) +
  scale_y_discrete(limits = c("Root", "Stalk",
                              "E-6", "E-5", "E-4",
                              "E-3", "E-2", "E-1", #rearrange y axis
                              "E", "E+1", "E+2", "E+3", 
                              "E+4", "E+5", "E+6", "E+7", "E+8")) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c("#eab676", "#76b5c5", "#21130d"), limits = c("Xyway In-furrow", "Xyway 2x2", "Lucento"))+
  scale_color_manual(values=c("#eab676", "#76b5c5", "#21130d"))+
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

#compare treatment in 2021 only
Data2021 <- Flutriafol[Flutriafol$Year == "2021",]
Treatment2021 <- ggplot(Data2021,                                     
                        aes(x = Mean,
                            y = Plant,
                            fill = Treatment)) +
  #geom_bar(stat = "identity",position = "dodge") +
  geom_col(position=position_dodge2(preserve = "single")) +
  scale_y_discrete(limits = c("Root", "Stalk",
                              "E-5", "E-4",
                              "E-3", "E-2", "E-1", #rearrange y axis
                              "E", "E+1", "E+2", "E+3", 
                              "E+4", "E+5", "E+6", "E+7")) +
  xlab("Flutriafol Concentration (µg/mL)") +
  ylab("") +
  scale_fill_manual(values=c("#eab676", "#76b5c5", "#873e23"), limits = c("Xyway In-furrow", "Xyway 2x2", "Xyway 0x0"))+
  scale_color_manual(values=c("#eab676", "#76b5c5", "#873e23"))+
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

ggsave(Treatment2019, filename = "Treatment 2019.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(Treatment2020, filename = "Treatment 2020.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(Treatment2021, filename = "Treatment 2021.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)

###combine Treatment 2019-2021
Treatment2019to2021 <-
ggarrange(Treatment2019, Treatment2020, Treatment2021, 
          labels = c("A", "B", "C"),
          nrow = 3)
ggsave(Treatment2019to2021, filename = "Treatment 2019 to 2021.png",  bg = "transparent", dpi=600,units="in", height=10, width=7)

SLA <- read.csv("Data/SLA_Canadian cities.csv")

library(vegan)
library(car)
library(tidyverse)

glimpse(SLA)
unique(SLA$City)

#Histogram

histogram <- ggplot(SLA, aes(x = value, fill = City)) +
  facet_wrap(~City)+
  geom_histogram(bins = 50)

histogram

#One way ANOVA

ANOVA <- lm(value ~ City, data = SLA)
Anova(ANOVA, type = "2")

#Response: value
#Sum Sq   Df F value Pr(>F)
#City         889    5  1.5407 0.1739
#Residuals 253337 2195  

Summary <- SLA %>% group_by(City) %>% summarise(SLA.avg = mean(value),
                                                SLA.sd = sd(value),
                                                SLA.n = length(value),
                                                SLA.se = sd(value)/sqrt(n()))
Summary

ANOVA.fig <- ggplot(Summary, aes(City, SLA.avg, fill = City)) +
  geom_col() +
  geom_errorbar(aes(ymin = SLA.avg - SLA.se, ymax = SLA.avg + SLA.se), width = 0.2) +
  theme_classic() +
  ylim(0,40)

ANOVA.fig

ggsave("Figures/SLA_Barchart.JPEG")

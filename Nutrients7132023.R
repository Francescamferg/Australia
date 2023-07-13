#####Loading in Nutrient data#####
Nut_data <- read.csv("https://raw.githubusercontent.com/Francescamferg/Australia/main/Nutrients7132023.csv?token=GHSAT0AAAAAACFBR577OYUERYFRP2X4AOGMZFPWN7A")
View(Nut_data)

#####Loading in packages#####
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)

#####NH3#####
#Box Plot
NH3box <- boxplot(Nut_data$NH3_mgN_L ~ Nut_data$SITE, 
        xlab = "Site", ylab = "Concentration NH3 (mg/L)")
#Compute the analysis of variance
NH3.aov <- aov(Nut_data$NH3_mgN_L ~ Nut_data$SITE)
#summary of the analysis
summary(NH3.aov)

#####NOx#####
#Box Plot
NOx <- boxplot(Nut_data$NOX_mgN_L ~ Nut_data$SITE, 
                  xlab = "Site", ylab = "Concentration NOx (mg/L)")
#Compute the analysis of variance
NOx.aov <- aov(Nut_data$NOX_mgN_L ~ Nut_data$SITE)
#summary of the analysis
summary(NOx.aov)
#Statistically significant 
#tukey Test
TukeyHSD(NOx.aov)

#####PO4#####
#Box Plot
PO4 <- boxplot(Nut_data$PO4_Pmg_L ~ Nut_data$SITE, 
               xlab = "Site", ylab = "Concentration PO4 (mg/L)")
#Compute the analysis of variance
PO4.aov <- aov(Nut_data$PO4_Pmg_L ~ Nut_data$SITE)
#summary of the analysis
summary(PO4.aov)
#tukey Test
TukeyHSD(PO4.aov)

#####TKN#####
#Box Plot
TKN <- boxplot(Nut_data$TKN_Nmg_L ~ Nut_data$SITE, 
               xlab = "Site", ylab = "Concentration TKN (mg/L)")
#Compute the analysis of variance
TKN.aov <- aov(Nut_data$TKN_Nmg_L ~ Nut_data$SITE)
#summary of the analysis
summary(TKN.aov)
#tukey Test
TukeyHSD(TKN.aov)

#####TN#####
#Box Plot
TN <- boxplot(Nut_data$TN_Nmg_L ~ Nut_data$SITE, 
               xlab = "Site", ylab = "Concentration TN (mg/L)")
#Compute the analysis of variance
TN.aov <- aov(Nut_data$TN_Nmg_L ~ Nut_data$SITE)
#summary of the analysis
summary(TN.aov)
#tukey Test
TukeyHSD(TN.aov)

#####TP#####
#Box Plot
TP <- boxplot(Nut_data$TP_Pmg_L ~ Nut_data$SITE, 
              xlab = "Site", ylab = "Concentration TP (mg/L)")
#Compute the analysis of variance
TP.aov <- aov(Nut_data$TP_Pmg_L ~ Nut_data$SITE)
#summary of the analysis
summary(TP.aov)
#tukey Test
TukeyHSD(TP.aov)
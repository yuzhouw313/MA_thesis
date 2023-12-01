##########
#########
########
#######
######			Replication Code for:
#####				"The Politicization of COVID-19 and Anti-Asian Racism in the United States: An Experimental Approach"
####				
####				
####				
#####				Survey fielded on 18-19 May 2020; N=2,025
######			
#######			Author: D.G. Kim
########			
#########                                                 
##########			

rm(list=ls())
library(ggplot2)
library(dplyr)
library(psy)
library(ggplot2)
library(stargazer)
library(factoextra)
library(polycor)
library(mediation)
library(psych)
library(GPArotation)
library(lavaan)
library(nFactors)
library(sjPlot)
library(sjmisc)
library(ggExtra)  
library(interplot)
library(gridExtra)
library(tidyverse)


data <- read.csv("JEPS_Kim_Data_clean.csv") # n = 2,025
data <- data %>% filter(data$race == 1 | data$race == 2 | data$race ==3) # subsetting data to white, black, hispanic respondents
table(data$race) # final n for analysis = 1,852

###########################################################################
### Treatment groups

data$treat[data$control_time_Page.Submit>0] <- 1  # Control n=602
data$treat[data$corona_time_Page.Submit>0] <- 2   # Coronavirus condition n=619
data$treat[data$origin_time_Page.Submit>0] <- 3   # Chinese origin condition n=631
data$treat <- factor(data$treat)
table(data$treat)

###########################################################################
### Resentment Scales
# Asian American Resentment (AAR)

data$aar <- ((6-data$aar_1) + (6-data$aar_2) +(6-data$aar_3) + (6-data$aar_4) +
                (6-data$aar_5) + (6-data$aar_6))*(1/6) # creating a composite additive scale
data$aar_z <- scale(data$aar, center = TRUE, scale = TRUE)

AAR <-with(data, data.frame(aar_1, aar_2, aar_3, aar_4, aar_5, aar_6))
cronbach(AAR) # Alpha = 0.88

# Symbolic Racism
data$sr <- (((6-data$sr_1) + data$sr_2 + data$sr_3 + (6-data$sr_4)))*(1/4)
data$sr_z <- scale(data$sr, center = TRUE, scale = TRUE) 

SR <-with(data, data.frame((6-sr_1), sr_2, sr_3, (6-sr_4)))
cronbach(SR) # Alpha = 0.69

# Latino Resentment Scale

data$latino <- (1/7)*((6-data$latino_1) + (data$latino_2) +(data$latino_3) + (6-data$latino_4) +
                         (6-data$latino_5) + (6-data$latino_6) + (6-data$latino_7))
data$latino_z <- scale(data$latino, center = TRUE, scale = TRUE)

LATINO <-with(data, data.frame(6-latino_1, latino_2, latino_3, 6-latino_4, 6-latino_5, 6-latino_6, 6-latino_7))
cronbach(LATINO) # Alpha = 0.71

###########################################################################
## Feeling Thermometer 
# Asian American - own race

data$diff01 <- data$feeling_4 - data$feeling_1 # Asian - White
data$diff02 <- data$feeling_4 - data$feeling_2 # Asian - Black
data$diff03 <- data$feeling_4 - data$feeling_3 # Asian - Hispanic

data$feeling_asian <- ifelse(data$race=="1", data$diff01, 
                       ifelse(data$race=="2", data$diff02,
                              ifelse(data$race=="3", data$diff03,NA)))
data$feeling_asian_z <- scale(data$feeling_asian, center = TRUE, scale = TRUE) # Z score (standardized)

# Chinese American - own race

data$diff04 <- data$feeling_8 - data$feeling_1 # Chinese - White
data$diff05 <- data$feeling_8 - data$feeling_2 # Chinese - Black
data$diff06 <- data$feeling_8 - data$feeling_3 # Chinese - Hispanic

data$feeling_chinese <- ifelse(data$race=="1", data$diff04, 
                       ifelse(data$race=="2", data$diff05,
                              ifelse(data$race=="3", data$diff06,NA)))
data$feeling_chinese_z <- scale(data$feeling_chinese, center = TRUE, scale = TRUE) # Z score (standardized)

# African American - own race

data$diff07 <- data$feeling_2 - data$feeling_1 # Black - White
data$diff08 <- data$feeling_2 - data$feeling_3 # Black - Hispanic
data$diff09 <- data$feeling_2 - data$feeling_4 # Black - Asian

data$feeling_black <- ifelse(data$race=="1", data$diff07, 
                               ifelse(data$race=="3", data$diff08,
                                      ifelse(data$race=="4", data$diff09,NA)))
data$feeling_black_z <- scale(data$feeling_black, center = TRUE, scale = TRUE) # Z score (standardized)

# Hispanic - own race

data$diff10 <- data$feeling_3 - data$feeling_1 # Hispanic - White
data$diff11 <- data$feeling_3 - data$feeling_2 # Hispanic - Black
data$diff12 <- data$feeling_3 - data$feeling_4 # Hispanic - Asian

data$feeling_hispanic <- ifelse(data$race=="1", data$diff10, 
                               ifelse(data$race=="2", data$diff11,
                                      ifelse(data$race=="4", data$diff12,NA)))
data$feeling_hispanic_z <- scale(data$feeling_hispanic, center = TRUE, scale = TRUE) # Z score (standardized)

###########################################################################
### Stereotype Measures ###################################################
## Asian - own race #######################################################
# Violent

data$diff13 <- data$violent_asian - data$violent_white # Asian - White
data$diff14 <- data$violent_asian - data$violent_black # Asian - Black
data$diff15 <- data$violent_asian - data$violent_hispanic # Asian - Hispanic

data$violent_aa <- ifelse(data$race=="1", data$diff13, 
                       ifelse(data$race=="2", data$diff14,
                              ifelse(data$race=="3", data$diff15,NA)))

data$violent_aa_z <- scale(data$violent_aa, center = TRUE, scale = TRUE)

hist(data$violent_aa)

# Hardworking (larger values indicating hardworking)
data$diff16 <- data$lazy_asian - data$lazy_white # Asian - White
data$diff17 <- data$lazy_asian - data$lazy_black # Asian - Black
data$diff18 <- data$lazy_asian - data$lazy_hispanic # Asian - Hispanic

data$hard_aa <- ifelse(data$race=="1", data$diff16, 
                    ifelse(data$race=="2", data$diff17,
                           ifelse(data$race=="3", data$diff18,NA)))

data$hard_aa_z <- scale(data$hard_aa, center = TRUE, scale = TRUE)

# Intelligent
data$diff19 <- data$intel_asian - data$intel_white # Asian - White
data$diff20 <- data$intel_asian - data$intel_black # Asian - Black
data$diff21 <- data$intel_asian - data$intel_hispanic # Asian - Hispanic

data$intel_aa <- ifelse(data$race=="1", data$diff19, 
                     ifelse(data$race=="2", data$diff20,
                            ifelse(data$race=="3", data$diff21,NA)))

data$intel_aa_z <- scale(data$intel_aa, center = TRUE, scale = TRUE)

# Untrustworthy (larger values indicating untrustworthiness)
data$diff22 <- (8-data$trust_asian) - (8-data$trust_white) # Asian - White
data$diff23 <- (8-data$trust_asian) - (8-data$trust_black) # Asian - Black
data$diff24 <- (8-data$trust_asian) - (8-data$trust_hispanic) # Asian - Hispanic

data$untrust_aa <- ifelse(data$race=="1", data$diff22, 
                       ifelse(data$race=="2", data$diff23,
                              ifelse(data$race=="3", data$diff24,NA)))

data$untrust_aa_z <- scale(data$untrust_aa, center = TRUE, scale = TRUE)

table(data$untrust_aa)

# Combined score (larger values indicating negative stereotypes)

data$stereo_aa <- (data$untrust_aa + data$violent_aa + (-1)*(data$hard_aa) + (-1)*(data$intel_aa))*(1/4)
hist(data$stereo_aa)

## Chinese American - own race ############################################
# Violent

data$diff25 <- data$violent_chinese - data$violent_white # chinese - White
data$diff26 <- data$violent_chinese - data$violent_black # chinese - Black
data$diff27 <- data$violent_chinese - data$violent_hispanic # chinese - Hispanic

data$violent_ca <- ifelse(data$race=="1", data$diff25, 
                       ifelse(data$race=="2", data$diff26,
                              ifelse(data$race=="3", data$diff27,NA)))

data$violent_ca_z <- scale(data$violent_ca, center = TRUE, scale = TRUE)

# Hardworking (larger values indicating hardworking)
data$diff28 <- data$lazy_chinese - data$lazy_white # chinese - White
data$diff29 <- data$lazy_chinese - data$lazy_black # chinese - Black
data$diff30 <- data$lazy_chinese - data$lazy_hispanic # chinese - Hispanic

data$hard_ca <- ifelse(data$race=="1", data$diff28, 
                    ifelse(data$race=="2", data$diff29,
                           ifelse(data$race=="3", data$diff30,NA)))

data$hard_ca_z <- scale(data$hard_ca, center = TRUE, scale = TRUE)

# Intelligent
data$diff31 <- data$intel_chinese - data$intel_white # chinese - White
data$diff32 <- data$intel_chinese - data$intel_black # chinese - Black
data$diff33 <- data$intel_chinese - data$intel_hispanic # chinese - Hispanic

data$intel_ca <- ifelse(data$race=="1", data$diff31, 
                     ifelse(data$race=="2", data$diff32,
                            ifelse(data$race=="3", data$diff33,NA)))

data$intel_ca_z <- scale(data$intel_ca, center = TRUE, scale = TRUE)

# Untrustworthy (larger values indicating untrustworthiness)
data$diff34 <- (8-data$trust_chinese) - (8-data$trust_white) # chinese - White
data$diff35 <- (8-data$trust_chinese) - (8-data$trust_black) # chinese - Black
data$diff36 <- (8-data$trust_chinese) - (8-data$trust_hispanic) # chinese - Hispanic

data$untrust_ca <- ifelse(data$race=="1", data$diff34, 
                       ifelse(data$race=="2", data$diff35,
                              ifelse(data$race=="3", data$diff36,NA)))

data$untrust_ca_z <- scale(data$untrust_ca, center = TRUE, scale = TRUE)

# Combined score (larger values indicating negative stereotypes)

data$stereo_ca <- (data$untrust_ca + data$violent_ca + (-1)*(data$hard_ca) + (-1)*(data$intel_ca))*(1/4)
hist(data$stereo_ca)

## African American - own race ############################################
# Violent

data$diff37 <- data$violent_black - data$violent_white # black - White
data$diff38 <- data$violent_black - data$violent_hispanic # black - Hispanic
data$diff39 <- data$violent_black - data$violent_asian # black - Asian

data$violent_ba <- ifelse(data$race=="1", data$diff37, 
                          ifelse(data$race=="3", data$diff38,
                                 ifelse(data$race=="4", data$diff39,NA)))

data$violent_ba_z <- scale(data$violent_ba, center = TRUE, scale = TRUE)

# Hardworking (larger values indicating hardworking)
data$diff40 <- data$lazy_black - data$lazy_white # black - White
data$diff41 <- data$lazy_black - data$lazy_hispanic # black - hispanic
data$diff42 <- data$lazy_black - data$lazy_asian # black - asian

data$hard_ba <- ifelse(data$race=="1", data$diff40, 
                       ifelse(data$race=="3", data$diff41,
                              ifelse(data$race=="4", data$diff42,NA)))

data$hard_ba_z <- scale(data$hard_ba, center = TRUE, scale = TRUE)

# Intelligent
data$diff43 <- data$intel_black - data$intel_white # black - White
data$diff44 <- data$intel_black - data$intel_hispanic # black - hispanic
data$diff45 <- data$intel_black - data$intel_asian # black - asian

data$intel_ba <- ifelse(data$race=="1", data$diff43, 
                        ifelse(data$race=="3", data$diff44,
                               ifelse(data$race=="4", data$diff45,NA)))

data$intel_ba_z <- scale(data$intel_ba, center = TRUE, scale = TRUE)

# Untrustworthy (larger values indicating untrustworthiness)
data$diff46 <- (8-data$trust_black) - (8-data$trust_white) # black - White
data$diff47 <- (8-data$trust_black) - (8-data$trust_hispanic) # black - hispanic
data$diff48 <- (8-data$trust_black) - (8-data$trust_asian) # black - asian

data$untrust_ba <- ifelse(data$race=="1", data$diff46, 
                          ifelse(data$race=="3", data$diff47,
                                 ifelse(data$race=="4", data$diff48,NA)))

data$untrust_ba_z <- scale(data$untrust_ba, center = TRUE, scale = TRUE)

# Combined score (larger values indicating negative stereotypes)

data$stereo_ba <- (data$untrust_ba + data$violent_ba + (-1)*(data$hard_ba) + (-1)*(data$intel_ba))*(1/4)
hist(data$stereo_ba)

## Hispanic American - own race ############################################
# Violent

data$diff49 <- data$violent_hispanic - data$violent_white # hispanic - White
data$diff50 <- data$violent_hispanic - data$violent_black # hispanic - black
data$diff51 <- data$violent_hispanic - data$violent_asian # hispanic - Asian

data$violent_ha <- ifelse(data$race=="1", data$diff49, 
                          ifelse(data$race=="2", data$diff50,
                                 ifelse(data$race=="4", data$diff51,NA)))

data$violent_ha_z <- scale(data$violent_ha, center = TRUE, scale = TRUE)

# Hardworking (larger values indicating hardworking)
data$diff52 <- data$lazy_hispanic - data$lazy_white # hispanic - White
data$diff53 <- data$lazy_hispanic - data$lazy_black # hispanic - black
data$diff54 <- data$lazy_hispanic - data$lazy_asian # hispanic - asian

data$hard_ha <- ifelse(data$race=="1", data$diff52, 
                       ifelse(data$race=="2", data$diff53,
                              ifelse(data$race=="4", data$diff54,NA)))

data$hard_ha_z <- scale(data$hard_ha, center = TRUE, scale = TRUE)

# Intelligent
data$diff55 <- data$intel_hispanic - data$intel_white # hispanic - White
data$diff56 <- data$intel_hispanic - data$intel_black # hispanic - black
data$diff57 <- data$intel_hispanic - data$intel_asian # hispanic - asian

data$intel_ha <- ifelse(data$race=="1", data$diff55, 
                        ifelse(data$race=="2", data$diff56,
                               ifelse(data$race=="4", data$diff57,NA)))

data$intel_ha_z <- scale(data$intel_ha, center = TRUE, scale = TRUE)

# Untrustworthy (larger values indicating untrustworthiness)
data$diff58 <- (8-data$trust_hispanic) - (8-data$trust_white) # hispanic - White
data$diff59 <- (8-data$trust_hispanic) - (8-data$trust_black) # hispanic - black
data$diff60 <- (8-data$trust_hispanic) - (8-data$trust_asian) # hispanic - asian

data$untrust_ha <- ifelse(data$race=="1", data$diff58, 
                          ifelse(data$race=="3", data$diff59,
                                 ifelse(data$race=="4", data$diff60,NA)))

data$untrust_ha_z <- scale(data$untrust_ha, center = TRUE, scale = TRUE)

# Combined score (larger values indicating negative stereotypes)

data$stereo_ha <- (data$untrust_ha + data$violent_ha + (-1)*(data$hard_ha) + (-1)*(data$intel_ha))*(1/4)
hist(data$stereo_ha)

###########################################################################
# Ethnocentrism Measure

# data$violent, data$intel, data$trust, data$lazy (larger values indicating hardworking)

# Whites
data$dif01 <- data$intel_white - data$intel_black 
data$dif02 <- data$intel_white - data$intel_hispanic 
data$dif03 <- data$intel_white - data$intel_asian 

data$dif04 <- data$violent_black - data$violent_white 
data$dif05 <- data$violent_hispanic - data$violent_white 
data$dif06 <- data$violent_asian - data$violent_white 

data$dif07 <- data$trust_white - data$trust_black 
data$dif08 <- data$trust_white - data$trust_hispanic 
data$dif09 <- data$trust_white - data$trust_asian 

data$dif10 <- data$lazy_white - data$lazy_black 
data$dif11 <- data$lazy_white - data$lazy_hispanic 
data$dif12 <- data$lazy_white - data$lazy_asian

data$dif_white <- (1/12)*(data$dif01 + data$dif02 + data$dif03 + data$dif04 + data$dif05 + data$dif06 + data$dif07 + data$dif08 + data$dif09 + data$dif10 + data$dif11 + data$dif12 )
data$dif_white <- (1/6)*(data$dif_white) # scale from -1 to 1
hist(data$dif_white)

# Blacks
data$dif13 <- data$intel_black - data$intel_white 
data$dif14 <- data$intel_black - data$intel_hispanic 
data$dif15 <- data$intel_black - data$intel_asian 

data$dif16 <- data$violent_white - data$violent_black 
data$dif17 <- data$violent_hispanic - data$violent_black
data$dif18 <- data$violent_asian - data$violent_black 

data$dif19 <- data$trust_black - data$trust_white
data$dif20 <- data$trust_black - data$trust_hispanic 
data$dif21 <- data$trust_black - data$trust_asian 

data$dif22 <- data$lazy_black - data$lazy_white 
data$dif23 <- data$lazy_black - data$lazy_hispanic 
data$dif24 <- data$lazy_black - data$lazy_asian

data$dif_black <- (1/12)*(data$dif13 + data$dif14 + data$dif15 + data$dif16 + data$dif17 + data$dif18 + data$dif19 + data$dif20 + data$dif21 + data$dif22 + data$dif23 + data$dif24 )
data$dif_black <- (1/6)*(data$dif_black) # scale from -1 to 1
hist(data$dif_black)

# Hispanics
data$dif25 <- data$intel_hispanic - data$intel_white 
data$dif26 <- data$intel_hispanic - data$intel_black
data$dif27 <- data$intel_hispanic - data$intel_asian 

data$dif28 <- data$violent_white - data$violent_hispanic
data$dif29 <- data$violent_black - data$violent_hispanic
data$dif30 <- data$violent_asian - data$violent_hispanic

data$dif31 <- data$trust_hispanic - data$trust_white
data$dif32 <- data$trust_hispanic - data$trust_black
data$dif33 <- data$trust_hispanic - data$trust_asian 

data$dif34 <- data$lazy_hispanic - data$lazy_white 
data$dif35 <- data$lazy_hispanic - data$lazy_hispanic 
data$dif36 <- data$lazy_hispanic - data$lazy_asian

data$dif_hispanic <- (1/12)*(data$dif25 + data$dif26 + data$dif27 + data$dif28 + data$dif29 + data$dif30 + data$dif31 + data$dif32 + data$dif33 + data$dif34 + data$dif35 + data$dif36)
data$dif_hispanic <- (1/6)*(data$dif_hispanic) # scale from -1 to 1
hist(data$dif_hispanic)

# Asians
data$dif37 <- data$intel_asian - data$intel_white 
data$dif38 <- data$intel_asian - data$intel_black
data$dif39 <- data$intel_asian - data$intel_hispanic

data$dif40 <- data$violent_white - data$violent_asian
data$dif41 <- data$violent_black - data$violent_asian
data$dif42 <- data$violent_hispanic - data$violent_asian

data$dif43 <- data$trust_asian - data$trust_white
data$dif44 <- data$trust_asian - data$trust_black
data$dif45 <- data$trust_asian - data$trust_hispanic

data$dif46 <- data$lazy_asian - data$lazy_white 
data$dif47 <- data$lazy_asian - data$lazy_black
data$dif48 <- data$lazy_asian - data$lazy_hispanic

data$dif_asian <- (1/12)*(data$dif37 + data$dif38 + data$dif39 + data$dif40 + data$dif41 + data$dif42 + data$dif43 + data$dif44 + data$dif45 + data$dif46 + data$dif47 + data$dif48)
data$dif_asian <- (1/6)*(data$dif_asian) # scale from -1 to 1
hist(data$dif_asian)


table(data$race)


data$ethno <- ifelse(data$race=="1", data$dif_white, 
                     ifelse(data$race=="2", data$dif_black,
                            ifelse(data$race=="3", data$dif_hispanic,
                                   ifelse(data$race=="4", data$dif_asian,NA))))

data$ethno_z <- scale(data$ethno, center=TRUE, scale=TRUE)
hist(data$ethno)

###########################################################################
### Racial Policies

# Asian American Policies
data$policy_asian_1 <- 6 - data$racialpolicy_1 # limiting AA students to top colleges
data$policy_asian_2 <- 6 - data$racialpolicy_2 # limiting Asian immigrants

data$policy_asian <- (data$policy_asian_1 + data$policy_asian_2)*(1/2)

# Black policies
data$policy_black_1 <- data$racialpolicy_4 # Opposing affirmative action in colleges
data$policy_black_2 <- data$racialpolicy_5 # Opposing federal assist to blacks

data$policy_black <- (data$policy_black_1 + data$policy_black_2)*(1/2)

# Hispanic policies
data$policy_hispanic_1 <- 6 - data$racialpolicy_3 # enforcing strict U.S.-Mexico border control
data$policy_hispanic_2 <- 6 - data$racialpolicy_6 # cracking down on gangs and drugs

data$policy_hispanic <- (data$policy_hispanic_1 + data$policy_hispanic_2)*(1/2)

###########################################################################
### China Foreign Policies

table(data$china_1)

data$china_1r <-ifelse(data$china_1==8,6,
                       ifelse(data$china_1==9,7, data$china_1))
table(data$china_1r)


table(data$china_2)
data$china_2r <-ifelse(data$china_2==8,6,
                       ifelse(data$china_2==9,7, data$china_2))
table(data$china_2r)

table(data$china_3)
data$china_3r <-ifelse(data$china_3==8,6,
                       ifelse(data$china_3==9,7, data$china_3))
table(data$china_3r)

table(data$china_4)
data$china_4r <-ifelse(data$china_4==8,6,
                       ifelse(data$china_4==9,7, data$china_4))
table(data$china_4r)

data$china <- (data$china_1r + data$china_2r + data$china_3r + data$china_4r)*(1/4)
table(data$china)

### Placebo Foreign Policies

table(data$placebo_1) # Increase spending on fighting ISIS
data$placebo_1r <-ifelse(data$placebo_1==1,1,
                         ifelse(data$placebo_1==2,2,
                                ifelse(data$placebo_1==3,3,
                                       ifelse(data$placebo_1==5,4,
                                              ifelse(data$placebo_1==6,5,
                                                     ifelse(data$placebo_1==7,6,
                                                           ifelse(data$placebo_1==8,7, data$placebo_1)))))))

table(data$placebo_1r)

table(data$placebo_2) # Against abiding by international agreements on global climate change
data$placebo_2r <-ifelse(data$placebo_2==1,1,
                         ifelse(data$placebo_2==2,2,
                                ifelse(data$placebo_2==3,3,
                                       ifelse(data$placebo_2==5,4,
                                              ifelse(data$placebo_2==6,5,
                                                     ifelse(data$placebo_2==7,6,
                                                            ifelse(data$placebo_2==8,7, data$placebo_2)))))))
data$placebo_2r <- 8 - data$placebo_2r
table(data$placebo_2r)

table(data$placebo_3) # Economic and military sanction against Iran
data$placebo_3r <-ifelse(data$placebo_3==1,1,
                         ifelse(data$placebo_3==2,2,
                                ifelse(data$placebo_3==3,3,
                                       ifelse(data$placebo_3==5,4,
                                              ifelse(data$placebo_3==6,5,
                                                     ifelse(data$placebo_3==7,6,
                                                            ifelse(data$placebo_3==8,7, data$placebo_3)))))))
table(data$placebo_3r)

table(data$placebo_4) # Against increasing aids to Africa
data$placebo_4r <-ifelse(data$placebo_4==1,1,
                         ifelse(data$placebo_4==2,2,
                                ifelse(data$placebo_4==3,3,
                                       ifelse(data$placebo_4==5,4,
                                              ifelse(data$placebo_4==6,5,
                                                     ifelse(data$placebo_4==7,6,
                                                            ifelse(data$placebo_4==8,7, data$placebo_4)))))))
data$placebo_4r <- 8 - data$placebo_4r
table(data$placebo_4r)

data$placebo <- (data$placebo_1r + data$placebo_2r + data$placebo_3r + data$placebo_4r)*(1/4)

###########################################################################
### Demographics

# Military Assertiveness (MA)

data$ma <- ((6-data$ma_1) + data$ma_2 + (6-data$ma_3))*(1/3)
data$ma_z <- scale(data$ma, center = TRUE, scale = TRUE)

MA <-with(data, data.frame(ma_1, (6-ma_2), ma_3))
cronbach(MA) # Alpha = 0.51

# Essentialism

data$esse <- ((6-data$esse_1) + (6-data$esse_2))*(1/2)
hist(data$esse)

ESSE <-with(data, data.frame(esse_1, esse_2))
cronbach(ESSE) # Alpha = 0.62

data$esse_high[data$esse>4] <- 1
data$esse_high[data$esse<=4] <- 0
data$esse_high <- factor(data$esse_high)
table(data$esse_high)

# Ideology
table(data$ideo) # higher values = conservative
data$ideo_z <- scale(data$ideo, center = TRUE, scale = TRUE)

# PID
table(data$pid)
data$rep <- 0
data$rep[data$pid==1] <- 1
data$rep <- factor(data$rep)

data$democrat <- 0
data$democrat[data$pid==2] <- 1
data$democrat <- factor(data$democrat)

data$independent<- 0
data$independent[data$pid==3] <- 1
data$independent <- factor(data$independent)

data$pid2[data$pid==2] <- 0 # Democrats as baseline
data$pid2[data$pid==1] <- 1 # 1 = Republicans
data$pid2[data$pid==3] <- 2 # 2 = Independents
data$pid2 <- factor(data$pid2)

table(data$pid2)

# Employment status
table(data$employ)
data$unemploy <- 0
data$unemploy[data$employ==3 | data$employ==11] <- 1
data$unemploy <- factor(data$unemploy)
table(data$unemploy)

# Gender
data$male <- 0
data$male[data$gen==1] <- 1
data$male <- factor(data$male)

data$female <- 0
data$female[data$gen==2] <- 1
data$female <- factor(data$female)

# Race
data$race2[data$race==1] <- 0
data$race2[data$race==2] <- 1
data$race2[data$race==3] <- 2
data$race2 <- factor(data$race2)
table(data$race2)
table(data$race)

data$white <- 0
data$white[data$race==1] <- 1
data$white <- factor(data$white)

data$black <- 0
data$black[data$race==2] <- 1
data$black <- factor(data$black)

data$hispanic <- 0
data$hispanic[data$race==3] <- 1
data$hispanic <- factor(data$hispanic)

data$asian <- 0
data$asian[data$race==4] <- 1
data$asian <- factor(data$asian)

# Income
table(data$inc) # continuous

# Age
table(data$age) # continuous

# Education
table(data$edu)

data$college <- 0
data$college[data$edu >4] <- 1
data$college <- factor(data$college)

# Political knowledgeledge
table(data$know)
data$knowledge <- 0 
data$knowledge[data$know==3] <- 1
data$knowledge <- factor(data$knowledge)


###########################################################################
### Post-hoc check items

# Where do you think COVID-19 originated from? binary scale (data$origin)

data$origin <- 0
data$origin[data$check_1==2] <- 1

data$origin <- factor(data$origin)

# How much do you blame China for COVID-19? (data$blame) scale 0-100

data$blame <- data$check_2_1

# Have you experienced financial hardship due to COVID-19? scale 1-5 (data$hardship)

data$hardship <-ifelse(data$vul_2==44,5,
                       ifelse(data$vul_2==45,4,
                              ifelse(data$vul_2==46,3,
                                  ifelse(data$vul_2==47,2,
                                      ifelse(data$vul_2==48,1, data$vul_2)))))



# Have you lost your job/business due to COVID-19? binary scale (data$loss)

data$loss <- 0 
data$loss[data$vul_1 == 2] <- 1

data$loss <- factor(data$loss)

# Do you know anyone close to you who con? binary scale (data$close)

data$close <- 0 
data$close[data$vul_3 == 2] <- 1

data$close <- factor(data$close)

###########################################################################
# vote for Trump

data$trump_16 <- 0
data$trump_16[data$trump_1==2] <- 1

data$trump_20 <- data$trump_2_1

###########################################################################
# Main variables recoded 0-1

data$aar_r <- (data$aar-1)*(1/4)
data$favorability_asian <- (data$feeling_4)*(1/100)
data$favorability_chinese <- (data$feeling_8)*(1/100)
data$stereo_aa_r <- ((((8-data$intel_asian) + (8-data$trust_asian) + (8-data$lazy_asian) + data$violent_asian)*(1/4))-1)*(1/6)
data$stereo_ca_r <- ((((8-data$intel_chinese) + (8-data$trust_chinese) + (8-data$lazy_chinese) + data$violent_chinese)*(1/4))-1)*(1/6)
data$policy_asian_r <- (data$policy_asian-1)*(1/4)
data$policy_asian_1_r <- (data$policy_asian_1-1)*(1/4)
data$policy_asian_2_r <- (data$policy_asian_2-1)*(1/4)

data$sr_r <- (data$sr-1)*(1/4)
data$latino_r <- (data$latino-1)*(1/4)
data$favorability_black <- (data$feeling_2)*(1/100)
data$favorability_hispanic <- (data$feeling_3)*(1/100)
data$stereo_black_r <- ((((8-data$intel_black) + (8-data$trust_black) + (8-data$lazy_black) + data$violent_black)*(1/4))-1)*(1/6)
data$stereo_hispanic_r <- ((((8-data$intel_hispanic) + (8-data$trust_hispanic) + (8-data$lazy_hispanic) + data$violent_hispanic)*(1/4))-1)*(1/6)
data$policy_black_1 <- (data$policy_black_1 -1)*(1/4) # Opposing affirmative action in colleges
data$policy_black_2 <- (data$policy_black_2 -1)*(1/4) # Opposing federal assist to blacks
data$policy_hispanic_1 <- (data$policy_hispanic_1 -1)*(1/4) # enforcing strict U.S.-Mexico border control
data$policy_hispanic_2 <- (data$policy_hispanic_2 -1)*(1/4)# cracking down on gangs and drugs
data$china_r <- (data$china-1)*(1/6)
data$china_1r <- (data$china_1r-1)*(1/6)
data$china_2r <- (data$china_2r-1)*(1/6)
data$china_3r <- (data$china_3r-1)*(1/6)
data$china_4r <- (data$china_4r-1)*(1/6)

###########################################################################
# Figure 2 in the main paper

library(dotwhisker)

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)

dwplot(list(q1, q2, q3, q4, q5, q6, q7), vline = geom_vline(xintercept = 0, colour = "grey20", linetype = 2),
       vars_order=c("treat3", "treat2"),
       model_order=c("Model 7", "Model 6", "Model 5", "Model 4", "Model 3", "Model 2", "Model 1"),
       dot_args = list(aes(shape = model), lwd=4),
       whisker_args = list(aes(linetype = model), lwd=1)) %>%
  relabel_predictors(c(treat3 = "Chinese origin condition", treat2 = "Coronavirus condition")) +
  theme_classic() + xlab("Coefficient estimate") + ylab("") +
  ggtitle("") +
  theme(title = element_text(size=25),
        axis.title.x = element_text(size = 2), 
        axis.title.y = element_text(size = 23),
        axis.text.y = element_text(color="black", size=25, angle=0),
        axis.text.x = element_text(color="black", size=25, angle=0),
        plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.spacing.y = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.position=c(.75, .65),legend.text=element_text(size=22)) +
  scale_colour_grey(start =.1, end= .1, name="Model", breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"), labels=c("Anti-AA sentiment","Favorability: AA", "Favorability: CA", "Stereotype: AA", "Stereotype: CA", "Limit AA students", "Limit Asian immigrants")) +
  scale_shape_discrete(name="Model",  breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"), labels=c("Anti-AA sentiment","Favorability: AA", "Favorability: CA", "Stereotype: AA", "Stereotype: CA", "Limit AA students", "Limit Asian immigrants")) +
  guides(shape=guide_legend("Model"), colour=guide_legend("Model")) + 
  coord_flip() + xlim(c(-.07, .1))


stargazer(q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

###########################################################################
# Figure 3 in the main paper

q8 <- lm(aar_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q9 <- lm(favorability_asian ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q10 <- lm(favorability_chinese ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q11 <- lm(stereo_aa_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q12 <- lm(stereo_ca_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q13 <- lm(policy_asian_1_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q14 <- lm(policy_asian_2_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)

dwplot(list(q8, q9, q10, q11, q12, q13, q14), vline = geom_vline(xintercept = 0, colour = "grey20", linetype = 2),
       vars_order=c("treat3:pid21", "treat2:pid21"),
       model_order=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"),
       dot_args = list(aes(shape = model), lwd=4),
       whisker_args = list(aes(linetype = model), lwd=1)) %>%
  relabel_predictors(c("treat2:pid21" = "Coronavirus\n* Republican", "treat3:pid21" = "Chinese origin\n* Republican")) +
  theme_classic() + xlab("Coefficient estimate") + ylab("") +
  ggtitle("") +
  theme(title = element_text(size=25),
        axis.title.x = element_text(size = 2), 
        axis.title.y = element_text(size = 23),
        axis.text.y = element_text(color="black", size=25, angle=0),
        axis.text.x = element_text(color="black", size=25, angle=0),
        plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.spacing.y = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.position="none",legend.text=element_text(size=22)) +
  scale_colour_grey(start =.1, end= .1, name="Model", breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"), labels=c("Anti-AA sentiment","Favorability: AA", "Favorability: CA", "Stereotype: AA", "Stereotype: CA", "Limit AA students", "Limit Asian immigrants")) +
  scale_shape_discrete(name="Model",  breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"), labels=c("Anti-AA sentiment","Favorability: AA", "Favorability: CA", "Stereotype: AA", "Stereotype: CA", "Limit AA students", "Limit Asian immigrants")) +
  guides(shape=guide_legend("Model"), colour=guide_legend("Model")) + 
  xlim(c(-.15, .15))


q15 <- lm(aar_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q16 <- lm(favorability_asian ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q17 <- lm(favorability_chinese ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q18 <- lm(stereo_aa_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q19 <- lm(stereo_ca_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q20 <- lm(policy_asian_1_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q21 <- lm(policy_asian_2_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)


dwplot(list(q15, q16, q17, q18, q19, q20, q21), vline = geom_vline(xintercept = 0, colour = "grey20", linetype = 2),
       vars_order=c("treat3:esse", "treat2:esse"),
       model_order=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"),
       dot_args = list(aes(shape = model), lwd=4),
       whisker_args = list(aes(linetype = model), lwd=1)) %>%
  relabel_predictors(c("treat2:esse" = "Coronavirus\n* Essentialism", "treat3:esse" = "Chinese origin\n* Essentialism")) +
  theme_classic() + xlab("Coefficient estimate") + ylab("") +
  ggtitle("") +
  theme(title = element_text(size=25),
        axis.title.x = element_text(size = 2), 
        axis.title.y = element_text(size = 23),
        axis.text.y = element_text(color="black", size=25, angle=0),
        axis.text.x = element_text(color="black", size=25, angle=0),
        plot.title = element_text(face="bold"),
        legend.justification = c(0, 0), 
        legend.spacing.y = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.position=, legend.text=element_text(size=22)) +
  scale_colour_grey(start =.1, end= .1, name="Model", breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"), labels=c("Anti-AA sentiment","Favorability: AA", "Favorability: CA", "Stereotype: AA", "Stereotype: CA", "Limit AA students", "Limit Asian immigrants")) +
  scale_shape_discrete(name="Model",  breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7"), labels=c("Anti-AA sentiment","Favorability: AA", "Favorability: CA", "Stereotype: AA", "Stereotype: CA", "Limit AA students", "Limit Asian immigrants")) +
  guides(shape=guide_legend("Model"), colour=guide_legend("Model")) + 
  xlim(c(-.07, .07))
###########################################################################
# Supplementary Analyses (Appendix 4)

## Appendix 4.1
### Table 3. 

mean(data$aar_r[data$treat==1]) # Mean for Anti-AA & Control
sd(data$aar_r[data$treat==1]) # SD for Anti-AA & Control 

mean(data$aar_r[data$treat==2]) # Mean for Anti-AA & Coronavirus
sd(data$aar_r[data$treat==2]) # SD for Anti-AA & Coronavirus

mean(data$aar_r[data$treat==3]) # Mean for Anti-AA & Chinese origin
sd(data$aar_r[data$treat==3]) # SD for Anti-AA & Chinese origin

mean(data$favorability_asian[data$treat==1]) # Mean for Favor:AA & Control
sd(data$favorability_asian[data$treat==1]) # SD for Favor:AA & Control

mean(data$favorability_asian[data$treat==2]) # Mean for Favor:AA & Coronavirus
sd(data$favorability_asian[data$treat==2]) # SD for Favor:AA & Coronavirus

mean(data$favorability_asian[data$treat==3]) # Mean for Favor:AA & Chinese origin
sd(data$favorability_asian[data$treat==3]) # SD for Favor:AA & Chinese origin

mean(data$favorability_chinese[data$treat==1]) # Mean for Favor:CA & Control
sd(data$favorability_chinese[data$treat==1]) # SD for Favor:CA & Control

mean(data$favorability_chinese[data$treat==2]) # Mean for Favor:CA & Coronavirus
sd(data$favorability_chinese[data$treat==2]) # SD for Favor:CA & Coronavirus

mean(data$favorability_chinese[data$treat==3]) # Mean for Favor:CA & Chinese origin
sd(data$favorability_chinese[data$treat==3]) # SD for Favor:CA & Chinese origin

mean(data$stereo_aa_r[data$treat==1]) # Mean for Stereo:AA & Control
sd(data$stereo_aa_r[data$treat==1]) # SD for Stereo:AA & Control

mean(data$stereo_aa_r[data$treat==2]) # Mean for Stereo:AA & Coronavirus
sd(data$stereo_aa_r[data$treat==2]) # SD for Stereo:AA & Coronavirus

mean(data$stereo_aa_r[data$treat==3]) # Mean for Stereo:AA & Chinese origin
sd(data$stereo_aa_r[data$treat==3]) # SD for Stereo:AA & Chinese origin

mean(data$stereo_ca_r[data$treat==1]) # Mean for Stereo:CA & Control
sd(data$stereo_ca_r[data$treat==1]) # SD for Stereo:CA & Control

mean(data$stereo_ca_r[data$treat==2]) # Mean for Stereo:CA & Coronavirus
sd(data$stereo_ca_r[data$treat==2]) # SD for Stereo:CA & Coronavirus

mean(data$stereo_ca_r[data$treat==3]) # Mean for Stereo:CA & Chinese origin
sd(data$stereo_ca_r[data$treat==3]) # SD for Stereo:CA & Chinese origin

mean(data$policy_asian_1_r[data$treat==1]) # Mean for Policy 1 & Control
sd(data$policy_asian_1_r[data$treat==1]) # SD for Policy 1 & Control

mean(data$policy_asian_1_r[data$treat==2]) # Mean for Policy 1 & Coronavirus
sd(data$policy_asian_1_r[data$treat==2]) # SD for Policy 1 & Coronavirus

mean(data$policy_asian_1_r[data$treat==3]) # Mean for Policy 1 & Chinese origin
sd(data$policy_asian_1_r[data$treat==3]) # SD for Policy 1 & Chinese origin

mean(data$policy_asian_2_r[data$treat==1]) # Mean for Policy 2 & Control
sd(data$policy_asian_2_r[data$treat==1]) # SD for Policy 2 & Control

mean(data$policy_asian_2_r[data$treat==2]) # Mean for Policy 2 & Coronavirus
sd(data$policy_asian_2_r[data$treat==2]) # SD for Policy 2 & Coronavirus

mean(data$policy_asian_2_r[data$treat==3]) # Mean for Policy 2 & Chinese origin
sd(data$policy_asian_2_r[data$treat==3]) # SD for Policy 2 & Chinese origin


### Table 4. 

mean(data$sr_r[data$treat==1]) # Mean for Anti-black & Control
sd(data$sr_r[data$treat==1]) # SD for Anti-black & Control

mean(data$sr_r[data$treat==2]) # Mean for Anti-black & Coronavirus
sd(data$sr_r[data$treat==2]) # SD for Anti-black & Coronavirus

mean(data$sr_r[data$treat==3]) # Mean for Anti-black & Chinese origin
sd(data$sr_r[data$treat==3]) # SD for Anti-black & Chinese origin

mean(data$favorability_black[data$treat==1]) # Mean for Favor:black & Control
sd(data$favorability_black[data$treat==1]) # SD for Favor:black & Control

mean(data$favorability_black[data$treat==2]) # Mean for Favor:black & Coronavirus
sd(data$favorability_black[data$treat==2]) # SD for Favor:black & Coronavirus

mean(data$favorability_black[data$treat==3]) # Mean for Favor:black & Chinese origin
sd(data$favorability_black[data$treat==3]) # SD for Favor:black & Chinese origin

mean(data$stereo_black_r[data$treat==1]) # Mean for Stereo:black & Control
sd(data$stereo_black_r[data$treat==1]) # SD for Stereo:black & Control

mean(data$stereo_black_r[data$treat==2]) # Mean for Stereo:black & Coronavirus
sd(data$stereo_black_r[data$treat==2]) # SD for Stereo:black & Coronavirus

mean(data$stereo_black_r[data$treat==3]) # Mean for Stereo:black & Chinese origin
sd(data$stereo_black_r[data$treat==3]) # SD for Stereo:black & Chinese origin

mean(data$policy_black_1[data$treat==1]) # Mean for Policy 1 & Control
sd(data$policy_black_1[data$treat==1]) # SD for Policy 1 & Control

mean(data$policy_black_1[data$treat==2]) # Mean for Policy 1 & Coronavirus
sd(data$policy_black_1[data$treat==2]) # SD for Policy 1 & Coronavirus

mean(data$policy_black_1[data$treat==3]) # Mean for Policy 1 & Chinese origin
sd(data$policy_black_1[data$treat==3]) # SD for Policy 1 & Chinese origin

mean(data$policy_black_2[data$treat==1]) # Mean for Policy 2 & Control
sd(data$policy_black_2[data$treat==1]) # Mean for Policy 2 & Control

mean(data$policy_black_2[data$treat==2]) # Mean for Policy 2 & Coronavirus
sd(data$policy_black_2[data$treat==2]) # Mean for Policy 2 & Coronavirus

mean(data$policy_black_2[data$treat==3]) # Mean for Policy 2 & Chinese origin
sd(data$policy_black_2[data$treat==3]) # Mean for Policy 2 & Chinese origin


### Table 5.

mean(data$latino_r[data$treat==1]) # Mean for Anti-Latino & Control
sd(data$latino_r[data$treat==1]) # SD for Anti-Latino & Control

mean(data$latino_r[data$treat==2]) # Mean for Anti-Latino & Coronavirus
sd(data$latino_r[data$treat==2]) # SD for Anti-Latino & Coronavirus

mean(data$latino_r[data$treat==3]) # Mean for Anti-Latino & Chinese origin
sd(data$latino_r[data$treat==3]) # SD for Anti-Latino & Chinese origin

mean(data$favorability_hispanic[data$treat==1]) # Mean for Favor:Latino & Control
sd(data$favorability_hispanic[data$treat==1]) # SD for Favor:Latino & Control

mean(data$favorability_hispanic[data$treat==2]) # Mean for Favor:Latino & Coronavirus
sd(data$favorability_hispanic[data$treat==2]) # SD for Favor:Latino & Coronavirus

mean(data$favorability_hispanic[data$treat==3]) # Mean for Favor:Latino & Chinese origin
sd(data$favorability_hispanic[data$treat==3]) # SD for Favor:Latino & Chinese origin

mean(data$stereo_hispanic_r[data$treat==1]) # Mean for Stereo:Latino & Control
sd(data$stereo_hispanic_r[data$treat==1]) # SD for Stereo:Latino & Control

mean(data$stereo_hispanic_r[data$treat==2]) # Mean for Stereo:Latino & Coronavirus
sd(data$stereo_hispanic_r[data$treat==2]) # SD for Stereo:Latino & Coronavirus

mean(data$stereo_hispanic_r[data$treat==3]) # Mean for Stereo:Latino & Chinese origin
sd(data$stereo_hispanic_r[data$treat==3]) # SD for Stereo:Latino & Chinese origin

mean(data$policy_hispanic_1[data$treat==1]) # Mean for Policy 1 & Control
sd(data$policy_hispanic_1[data$treat==1]) # SD for Policy 1 & Control

mean(data$policy_hispanic_1[data$treat==2]) # Mean for Policy 1 & Coronavirus
sd(data$policy_hispanic_1[data$treat==2]) # SD for Policy 1 & Coronavirus

mean(data$policy_hispanic_1[data$treat==3]) # Mean for Policy 1 & Chinese origin
sd(data$policy_hispanic_1[data$treat==3]) # SD for Policy 1 & Chinese origin

mean(data$policy_hispanic_2[data$treat==1]) # Mean for Policy 2 & Control
sd(data$policy_hispanic_2[data$treat==1]) # SD for Policy 2 & Control

mean(data$policy_hispanic_2[data$treat==2]) # Mean for Policy 2 & Coronavirus
sd(data$policy_hispanic_2[data$treat==2]) # SD for Policy 2 & Coronavirus

mean(data$policy_hispanic_2[data$treat==3]) # Mean for Policy 2 & Chinese origin
sd(data$policy_hispanic_2[data$treat==3]) # SD for Policy 2 & Chinese origin


### Table 6.

mean(data$china_1r[data$treat==1]) # Mean for Tariff & Control
sd(data$china_1r[data$treat==1]) # SD for Tariff & Control

mean(data$china_1r[data$treat==2]) # Mean for Tariff & Coronavirus
sd(data$china_1r[data$treat==2]) # SD for Tariff & Coronavirus

mean(data$china_1r[data$treat==3]) # Mean for Tariff & Chinese origin
sd(data$china_1r[data$treat==3]) # SD for Tariff & Chinese origin

mean(data$china_2r[data$treat==1]) # Mean for Covert action & Control
sd(data$china_2r[data$treat==1]) # SD for Covert action & Control

mean(data$china_2r[data$treat==2]) # Mean for Covert action & Coronavirus
sd(data$china_2r[data$treat==2]) # SD for Covert action & Coronavirus

mean(data$china_2r[data$treat==3]) # Mean for Covert action & Chinese origin
sd(data$china_2r[data$treat==3]) # SD for Covert action & Chinese origin

mean(data$china_3r[data$treat==1]) # Mean for Threat force & Control
sd(data$china_3r[data$treat==1]) # SD for Threat force & Control

mean(data$china_3r[data$treat==2]) # Mean for Threat force & Coronavirus
sd(data$china_3r[data$treat==2]) # SD for Threat force & Coronavirus

mean(data$china_3r[data$treat==3]) # Mean for Threat force & Chinese origin
sd(data$china_3r[data$treat==3]) # SD for Threat force & Chinese origin

mean(data$china_4r[data$treat==1]) # Mean for Use force & Control
sd(data$china_4r[data$treat==1]) # SD for Use force & Control

mean(data$china_4r[data$treat==2]) # Mean for Use force & Coronavirus
sd(data$china_4r[data$treat==2]) # SD for Use force & Coronavirus

mean(data$china_4r[data$treat==3]) # Mean for Use force & Chinese origin
sd(data$china_4r[data$treat==3]) # SD for Use force & Chinese origin


t.test(data$aar_r[data$treat==1], data$aar_r[data$treat==3], alternative="less")
t.test(data$aar_r[data$treat==1], data$aar_r[data$treat==2], alternative="less") 
t.test(data$aar_r[data$treat==2], data$aar_r[data$treat==3], alternative="less") 

t.test(data$favorability_asian[data$treat==1], data$favorability_asian[data$treat==3], alternative="greater")
t.test(data$favorability_asian[data$treat==1], data$favorability_asian[data$treat==2], alternative="greater") 
t.test(data$favorability_asian[data$treat==2], data$favorability_asian[data$treat==3], alternative="greater") 

t.test(data$stereo_aa_r[data$treat==1], data$stereo_aa_r[data$treat==3], alternative="less")
t.test(data$stereo_aa_r[data$treat==1], data$stereo_aa_r[data$treat==2], alternative="less") 
t.test(data$stereo_aa_r[data$treat==2], data$stereo_aa_r[data$treat==3], alternative="less") # 0.05

t.test(data$stereo_ca_r[data$treat==1], data$stereo_ca_r[data$treat==3], alternative="less")
t.test(data$stereo_ca_r[data$treat==1], data$stereo_ca_r[data$treat==2], alternative="less") 
t.test(data$stereo_ca_r[data$treat==2], data$stereo_ca_r[data$treat==3], alternative="less")

t.test(data$policy_asian_1_r[data$treat==1], data$policy_asian_1_r[data$treat==3], alternative="less")
t.test(data$policy_asian_1_r[data$treat==1], data$policy_asian_1_r[data$treat==2], alternative="less") 
t.test(data$policy_asian_1_r[data$treat==2], data$policy_asian_1_r[data$treat==3], alternative="less")

t.test(data$policy_asian_2_r[data$treat==1], data$policy_asian_2_r[data$treat==3], alternative="less")
t.test(data$policy_asian_2_r[data$treat==1], data$policy_asian_2_r[data$treat==2], alternative="less") 
t.test(data$policy_asian_2_r[data$treat==2], data$policy_asian_2_r[data$treat==3], alternative="less")

t.test(data$sr_r[data$treat==1], data$sr_r[data$treat==3], alternative="less")
t.test(data$sr_r[data$treat==1], data$sr_r[data$treat==2], alternative="less") 
t.test(data$sr_r[data$treat==2], data$sr_r[data$treat==3], alternative="less") 

t.test(data$favorability_black[data$treat==1], data$favorability_black[data$treat==3], alternative="greater")
t.test(data$favorability_black[data$treat==1], data$favorability_black[data$treat==2], alternative="greater") 
t.test(data$favorability_black[data$treat==2], data$favorability_black[data$treat==3], alternative="greater")

t.test(data$stereo_black_r[data$treat==1], data$stereo_black_r[data$treat==3], alternative="less")
t.test(data$stereo_black_r[data$treat==1], data$stereo_black_r[data$treat==2], alternative="less") 
t.test(data$stereo_black_r[data$treat==2], data$stereo_black_r[data$treat==3], alternative="less")

t.test(data$policy_black_1[data$treat==1], data$policy_black_1[data$treat==3], alternative="less")
t.test(data$policy_black_1[data$treat==1], data$policy_black_1[data$treat==2], alternative="less") 
t.test(data$policy_black_1[data$treat==2], data$policy_black_1[data$treat==3], alternative="less")

t.test(data$policy_black_2[data$treat==1], data$policy_black_2[data$treat==3], alternative="less")
t.test(data$policy_black_2[data$treat==1], data$policy_black_2[data$treat==2], alternative="less") 
t.test(data$policy_black_2[data$treat==2], data$policy_black_2[data$treat==3], alternative="less")

t.test(data$latino_r[data$treat==1], data$latino_r[data$treat==3], alternative="less")
t.test(data$latino_r[data$treat==1], data$latino_r[data$treat==2], alternative="less") 
t.test(data$latino_r[data$treat==2], data$latino_r[data$treat==3], alternative="less")

t.test(data$favorability_hispanic[data$treat==1], data$favorability_hispanic[data$treat==3], alternative="greater")
t.test(data$favorability_hispanic[data$treat==1], data$favorability_hispanic[data$treat==2], alternative="greater") 
t.test(data$favorability_hispanic[data$treat==2], data$favorability_hispanic[data$treat==3], alternative="greater")

t.test(data$stereo_hispanic_r[data$treat==1], data$stereo_hispanic_r[data$treat==3], alternative="less")
t.test(data$stereo_hispanic_r[data$treat==1], data$stereo_hispanic_r[data$treat==2], alternative="less") 
t.test(data$stereo_hispanic_r[data$treat==2], data$stereo_hispanic_r[data$treat==3], alternative="less")

t.test(data$policy_hispanic_1[data$treat==1], data$policy_hispanic_1[data$treat==3], alternative="less")
t.test(data$policy_hispanic_1[data$treat==1], data$policy_hispanic_1[data$treat==2], alternative="less") 
t.test(data$policy_hispanic_1[data$treat==2], data$policy_hispanic_1[data$treat==3], alternative="less")

t.test(data$policy_hispanic_2[data$treat==1], data$policy_hispanic_2[data$treat==3], alternative="less")
t.test(data$policy_hispanic_2[data$treat==1], data$policy_hispanic_2[data$treat==2], alternative="less") 
t.test(data$policy_hispanic_2[data$treat==2], data$policy_hispanic_2[data$treat==3], alternative="less")

t.test(data$china_1r[data$treat==1], data$china_1r[data$treat==3], alternative="less")
t.test(data$china_1r[data$treat==1], data$china_1r[data$treat==2], alternative="less") 
t.test(data$china_1r[data$treat==2], data$china_1r[data$treat==3], alternative="less")

t.test(data$china_2r[data$treat==1], data$china_2r[data$treat==3], alternative="less")
t.test(data$china_2r[data$treat==1], data$china_2r[data$treat==2], alternative="less") 
t.test(data$china_2r[data$treat==2], data$china_2r[data$treat==3], alternative="less")

t.test(data$china_3r[data$treat==1], data$china_3r[data$treat==3], alternative="less")
t.test(data$china_3r[data$treat==1], data$china_3r[data$treat==2], alternative="less") 
t.test(data$china_3r[data$treat==2], data$china_3r[data$treat==3], alternative="less")

t.test(data$china_4r[data$treat==1], data$china_4r[data$treat==3], alternative="less")
t.test(data$china_4r[data$treat==1], data$china_4r[data$treat==2], alternative="less") 
t.test(data$china_4r[data$treat==2], data$china_4r[data$treat==3], alternative="less")


###########################################################################
# Appendix 4.2 Regression Tables

## Table 7.

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

## Table 8.

q8 <- lm(aar_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q9 <- lm(favorability_asian ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q10 <- lm(favorability_chinese ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q11 <- lm(stereo_aa_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q12 <- lm(stereo_ca_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q13 <- lm(policy_asian_1_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)
q14 <- lm(policy_asian_2_r ~ treat + pid2 + treat*pid2 + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data)

stargazer(type="text", q8, q9, q10, q11, q12, q13, q14,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

## Table 9.

q15 <- lm(aar_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q16 <- lm(favorability_asian ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q17 <- lm(favorability_chinese ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q18 <- lm(stereo_aa_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q19 <- lm(stereo_ca_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q20 <- lm(policy_asian_1_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)
q21 <- lm(policy_asian_2_r ~ treat + esse + treat*esse + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data)

stargazer(type="text", q15, q16, q17, q18, q19, q20, q21,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

## Table 10 (controls for total covid cases for region (number of cumulative COVID-19 cases by county as of 5/17/2020 - one day before survey))

summary(q15 <- lm(aar_r ~ treat + log(total_case) + treat*log(total_case) + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q16 <- lm(favorability_asian ~ treat + log(total_case) + treat*log(total_case) + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q17 <- lm(favorability_chinese ~ treat + log(total_case) + treat*log(total_case) + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q18 <- lm(stereo_aa_r ~ treat + log(total_case) + treat*log(total_case) + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q19 <- lm(stereo_ca_r ~ treat + log(total_case) + treat*log(total_case) + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q20 <- lm(policy_asian_1_r ~ treat + log(total_case) + treat*log(total_case) + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q21 <- lm(policy_asian_2_r ~ treat + log(total_case) + treat*log(total_case) + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))

library(multiwayvcov)
library(lmtest)
q15 <- coeftest(q15, vcov=vcovHC(q15,type="HC0",cluster="fips")) # clustered standard error
q16 <- coeftest(q16, vcov=vcovHC(q16,type="HC0",cluster="fips")) # clustered standard error
q17 <- coeftest(q17, vcov=vcovHC(q17,type="HC0",cluster="fips")) # clustered standard error
q18 <- coeftest(q18, vcov=vcovHC(q18,type="HC0",cluster="fips")) # clustered standard error
q19 <- coeftest(q19, vcov=vcovHC(q19,type="HC0",cluster="fips")) # clustered standard error
q20 <- coeftest(q20, vcov=vcovHC(q20,type="HC0",cluster="fips")) # clustered standard error
q21 <- coeftest(q21, vcov=vcovHC(q21,type="HC0",cluster="fips")) # clustered standard error

stargazer(type="text", q15, q16, q17, q18, q19, q20, q21,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

## Table 11 (controls for having COVID cases in personal networks)

summary(q15 <- lm(aar_r ~ treat + close + treat*close + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q16 <- lm(favorability_asian ~ treat + close + treat*close + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q17 <- lm(favorability_chinese ~ treat + close + treat*close + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q18 <- lm(stereo_aa_r ~ treat + close + treat*close + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q19 <- lm(stereo_ca_r ~ treat + close + treat*close + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q20 <- lm(policy_asian_1_r ~ treat + close + treat*close + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q21 <- lm(policy_asian_2_r ~ treat + close + treat*close + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))

stargazer(type="text", q15, q16, q17, q18, q19, q20, q21,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

## Table 12 (controls for having lost job due to COVID)

summary(q15 <- lm(aar_r ~ treat + loss + treat*loss + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q16 <- lm(favorability_asian ~ treat + loss + treat*loss + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q17 <- lm(favorability_chinese ~ treat + loss + treat*loss + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q18 <- lm(stereo_aa_r ~ treat + loss + treat*loss + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q19 <- lm(stereo_ca_r ~ treat + loss + treat*loss + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q20 <- lm(policy_asian_1_r ~ treat + loss + treat*loss + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))
summary(q21 <- lm(policy_asian_2_r ~ treat + loss + treat*loss + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data))

stargazer(type="text", q15, q16, q17, q18, q19, q20, q21,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

# Table 13 (Subgroup analysis for Republicans)

q1 <- lm(aar_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==1)
q2 <- lm(favorability_asian ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==1)
q3 <- lm(favorability_chinese ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==1)
q4 <- lm(stereo_aa_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==1)
q5 <- lm(stereo_ca_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==1)
q6 <- lm(policy_asian_1_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==1)
q7 <- lm(policy_asian_2_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==1)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

# Table 14 (Subgroup analysis for Democrats)

q1 <- lm(aar_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==0)
q2 <- lm(favorability_asian ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==0)
q3 <- lm(favorability_chinese ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==0)
q4 <- lm(stereo_aa_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==0)
q5 <- lm(stereo_ca_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==0)
q6 <- lm(policy_asian_1_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==0)
q7 <- lm(policy_asian_2_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==0)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

# Table 15 (Subgroup analysis for Independents)

q1 <- lm(aar_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==2)
q2 <- lm(favorability_asian ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==2)
q3 <- lm(favorability_chinese ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==2)
q4 <- lm(stereo_aa_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==2)
q5 <- lm(stereo_ca_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==2)
q6 <- lm(policy_asian_1_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==2)
q7 <- lm(policy_asian_2_r ~ treat + ideo + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=pid2==2)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

# Table 16 (Subgroup analysis for high essentialism group)

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==1)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==1)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==1)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==1)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==1)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==1)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==1)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

# Table 17 (Subgroup analysis for low essentialism group)

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==0)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==0)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==0)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==0)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==0)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==0)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=esse_high==0)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

# Table 18 (White Americans only)

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==0)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==0)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==0)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))


# Table 19 (African Americans only)

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==1)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==1)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==1)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==1)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==1)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==1)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==1)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))


# Table 20 (Hispanic Americans only)

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==2)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==2)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + age + inc + college + knowledge, data=data, subset=race2==2)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==2)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==2)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==2)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==2)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))


# Table 21 (manipulation check passers only)

data$passer[data$fmc_control==1 | data$fmc_corona==1 | data$fmc_origin==1] <- 0
data$passer[data$fmc_control==2 | data$fmc_corona==2 | data$fmc_origin==2] <- 1
prop.table(table(data$passer))

t.test(data$passer[data$treat==1], data$passer[data$treat==2])
t.test(data$passer[data$treat==1], data$passer[data$treat==3])

q1 <- lm(aar_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=passer==1)
q2 <- lm(favorability_asian ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=passer==1)
q3 <- lm(favorability_chinese ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=passer==1)
q4 <- lm(stereo_aa_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=passer==1)
q5 <- lm(stereo_ca_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=passer==1)
q6 <- lm(policy_asian_1_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=passer==1)
q7 <- lm(policy_asian_2_r ~ treat + ideo + pid2 + factor(male) + race2 + age + inc + college + knowledge, data=data, subset=passer==1)

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))


# Table 22 (Main models with other racial attitude DVs (whites only))

summary(q1 <- lm(sr_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q2 <- lm(latino_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q3 <- lm(favorability_black ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q4 <- lm(favorability_hispanic ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q5 <- lm(stereo_black_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q6 <- lm(stereo_hispanic_r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q7 <- lm(policy_black_1 ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q8 <- lm(policy_black_2 ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q9 <- lm(policy_hispanic_1 ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))
summary(q10 <- lm(policy_hispanic_2 ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data, subset=race2==0))

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7, q8, q9, q10,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

# Table 23 (Main models with China policy DVs)

summary(q1 <- lm(china_1r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data))
summary(q2 <- lm(china_2r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data))
summary(q3 <- lm(china_3r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data))
summary(q4 <- lm(china_4r ~ treat + ideo + pid2 + factor(male)  + age + inc + college + knowledge, data=data))

stargazer(type="text", q1, q2, q3, q4,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))


# Table 24 (Associations between vulnerability to COVID-19 and views toward Asian and Chinese Americans)

summary(q1 <- lm(aar_r ~ hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data)) # ***
summary(q2 <- lm(favorability_asian ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data))
summary(q3 <- lm(favorability_chinese ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data))
summary(q4 <- lm(stereo_aa_r ~  hardship + close + log(total_case)  + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data))
summary(q5 <- lm(stereo_ca_r ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data))
summary(q6 <- lm(policy_asian_1_r ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data)) # ***
summary(q7 <- lm(policy_asian_2_r ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data)) # ***

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

pvalues <- c(summary(q1)$coefficients[2,4])
pvalues
p.adjust(pvalues,method="bonferroni", n=21)

# Table 25

summary(q1 <- lm(sr_r ~ hardship + close + log(total_case)  + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))
summary(q2 <- lm(latino_r ~ hardship + close + log(total_case)  + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))
summary(q3 <- lm(favorability_black ~ hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))
summary(q4 <- lm(favorability_hispanic ~ hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))
summary(q5 <- lm(stereo_black_r ~ hardship + close + log(total_case)  + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))
summary(q6 <- lm(stereo_hispanic_r ~ hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))
summary(q7 <- lm(policy_black_1 ~ hardship + close + log(total_case)  + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0)) 
summary(q8 <- lm(policy_black_2 ~  hardship + close + log(total_case)  + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0)) 
summary(q9 <- lm(policy_hispanic_1 ~  hardship + close + log(total_case)  + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))
summary(q10 <- lm(policy_hispanic_2 ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat, data=data, subset=race2==0))

stargazer(type="text", q1, q2, q3, q4, q5, q6, q7, q8, q9, q10,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

pvalues <- c(summary(q8)$coefficients[2,4])
pvalues
p.adjust(pvalues,method="bonferroni", n=21)

# Table 26

summary(q1 <- lm(china_1r ~ hardship + close + log(total_case) +  ideo + pid2 + factor(male) + age + inc + college + knowledge + treat + ma, data=data)) 
summary(q2 <- lm(china_2r ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat + ma, data=data)) # ***
summary(q3 <- lm(china_3r ~  hardship + close + log(total_case) + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat + ma, data=data)) # ***
summary(q4 <- lm(china_4r ~  hardship + close + log(total_case)  + ideo + pid2 + factor(male) + age + inc + college + knowledge + treat + ma, data=data)) # ***

stargazer(type="text", q1, q2, q3, q4,
          title="",
          dep.var.labels =c(),
          keep.stat = c("n", "adj.rsq"), 
          digits=2, star.cutoffs = c(0.05, 0.01, 0.001))

pvalues <- c(summary(q4)$coefficients[2,4])
pvalues
p.adjust(pvalues,method="bonferroni", n=21)


################################################################################
# Power Analysis (Figure 8)

library(pwr)
library(gridExtra)
library(stargazer)
# Assumed mean for control group: 3.2; treatment group: 4
# instead of means for each group, we can enter a mean of zero for Group 1 and 10 for the mean of Group 2, 
# so that the difference in means will be 10. Next, we need to specify the pooled standard deviation, 
# which is the square root of the average of the two standard deviations. In this case, it is sqrt((0.8^2 + 0.84^2)/2) = 0.82. 
# The default significance level (alpha level) is .05. For this example we will set the power to be at .8.

pwr.t.test(d=(0-0.8)/0.82,n=333,sig.level=.05,type="two.sample",alternative="two.sided")

# Calculating power when effect size = 0.8 (large), n=333 in each group
pwr.t.test(n = 600, d = 0.2, sig.level = 0.05)


## Use this power size to make a plot
par(mfrow=c(1,2))

# 1) How large sample do we need based on effect sizes?
ptab<-cbind(NULL, NULL)       # initalize ptab

for (i in c(.2, .3, .4, .5, .6, .7, .8, .9, 1, 1.1, 1.2)){
  pwrt<-pwr.t.test(d=i,power=0.9,sig.level=.05,type="two.sample",alternative="two.sided")
  ptab<-rbind(ptab, cbind(pwrt$d, pwrt$n))
}

ptab

plot(ptab[,1],ptab[,2],type="b",xlab="effect size",ylab="sample size")

# 2) How much power do we have based on sample sizes when effect size = 0.4?

pwrt<-pwr.t.test(d=.2,n=c(50,100,150,200,250,300,350,400,450,500),sig.level=.05,type="two.sample",alternative="two.sided")

plot(pwrt$n,pwrt$power,type="b",xlab="sample size",ylab="power")



# Meerkat offpsring survival to one year model

# last modified 03/11/2023

#using R version 4.3.1

# clear R of all objects
rm(list=ls())

# Need to decide whether other variable ssuch as early life group size or rainfall should be included - and over what timescale is most appropriate 

# Load data and libraries ####
#load librarys
library(dplyr)
library(sjPlot)
library(tidyverse)
library(rms)
library(car)
library(DHARMa)
library(glmmTMB)
library(lme4)


# import and rename data file
ALLpups<-read_csv("MeerkatSurvival.csv") #n = 123

#Remove unsexed pups (13) and 3 pups where we were not sure if they were from a control or subordinate dam
pup_Survival<- ALLpups %>% filter(Sex != "P")%>% droplevels() %>% filter(Treatment != "C/S")%>% droplevels() #N = 107

pup_Survival$Cohort<-as.factor(pup_Survival$Cohort)
pup_Survival$Litter<-as.factor(pup_Survival$Litter)
pup_Survival$Sex<-as.factor(pup_Survival$Sex)
pup_Survival$Treatment<-as.factor(pup_Survival$Treatment)
pup_Survival$Group_size<-as.numeric(pup_Survival$Group_size)
pup_Survival$Total_monthly_Rainfall<-as.numeric(pup_Survival$Total_monthly_Rainfall)
pup_Survival$Survival_Status_glmm_factor<-as.factor(pup_Survival$Survival_Status_glmm)

#re-order treatment order

pup_Survival$Treatment_b <- factor(pup_Survival$Treatment, levels=c('S', 'C', 'F'))

str(pup_Survival)


# This year was a drought so few individuals survived and had no individuals from flutamide litters
pup_Survival_reduced<- pup_Survival %>% filter(Cohort != "2")%>% droplevels()  #N = 92

pup_Survival_reduced$Treatment_b <- factor(pup_Survival_reduced$Treatment, levels=c('S', 'C', 'F'))


# Binomial analysis using glmm instead of survival analysis

#Run the full model

Survival_binomial<-glmmTMB(Survival_Status_glmm~Treatment_b +Sex +Total_monthly_Rainfall+Group_size+(1|Litter),data=pup_Survival_reduced,family=binomial)
summary(Survival_binomial)
Anova(Survival_binomial)
plot_model(Survival_binomial)
plot_model(Survival_binomial,type = "re")

# check model assumptions
resp = simulateResiduals(Survival_binomial)
plot(resp, rank = T)
simulationOutput <- simulateResiduals(fittedModel = Survival_binomial, n = 1000)
testDispersion(simulationOutput = simulationOutput, alternative ="less")
testDispersion(simulationOutput = simulationOutput, alternative ="greater")
testUniformity(simulationOutput = simulationOutput)

# check VIF - all under 2 apart from age
vif(glmer(Survival_Status_glmm~Treatment_b +Sex +Total_monthly_Rainfall+Group_size+(1|Litter),data=pup_Survival_reduced,family=binomial))

# survival graph

Survival_binomialplot_MAT<-ggplot(pup_Survival_reduced, aes(x=Treatment_b, y=(Survival_Status_glmm*100),    color= Treatment))+
stat_summary(fun.data=mean_se, geom="pointrange",size=1, aes(color=paste("mean",Treatment),group=Treatment)) +
stat_summary(fun.y=mean, geom="point",size=1, aes(color=paste("mean",Treatment),group=Treatment)) +
scale_color_manual(values=c("#3a004d","#ffca47","#3a004d","#ffca47","#278f98","#278f98"))+
geom_point(aes(x=Treatment, y=(Survival_Status_glmm*100),color= Treatment),size=1,alpha=0.2, position=position_jitter(height=0.001, width=0.2)) +
theme_classic()+
labs(x="Maternal treatment", y="% Offspring survived to 1st year") +
theme(axis.title.x=element_text(size=20, color="black"),axis.text.x =element_text(size=16, color="black"), legend.text = element_text(size=16), legend.title = element_text(size=20)) +
theme(axis.title.y=element_text(color="black", size=20, margin = margin(-5,10,0,0)),axis.text.y=element_text(size=16, color="black"), axis.line = element_line(colour = "black",size = 1, linetype = "solid")) +  theme(legend.position = 'none')+ylim(-1,101)+
scale_x_discrete(labels=c("SC","DC", "DT"))
  Survival_binomialplot_MAT

Survival_binomialplot_SEX<-ggplot(pup_Survival_reduced, aes(x=Sex, y=(Survival_Status_glmm*100),    color= Sex))+
  stat_summary(fun.data=mean_se, geom="pointrange",size=1, aes(color=paste("mean",Sex),group=Sex)) +
  stat_summary(fun.y=mean, geom="point",size=1, aes(color=paste("mean",Sex),group=Sex)) +
  scale_color_manual(values=c("#C80815","#00CCFF","#C80815","#00CCFF"))+
  geom_point(aes(x=Sex, y=(Survival_Status_glmm*100),color= Sex),size=1,alpha=0.2, position=position_jitter(height=0.001, width=0.2)) +
  theme_classic()+
  labs(x="Sex", y="% Offspring survived to 1st year") +
  theme(axis.title.x=element_text(size=20, color="black"),axis.text.x =element_text(size=16, color="black"), legend.text = element_text(size=16), legend.title = element_text(size=20)) +
  theme(axis.title.y=element_text(color="black", size=20, margin = margin(-5,10,0,0)),axis.text.y=element_text(size=16, color="black"), axis.line = element_line(colour = "black",size = 1, linetype = "solid")) +  theme(legend.position = 'none')+ylim(-1,101)+
  scale_x_discrete(labels=c("Female","Male"))
Survival_binomialplot_SEX

# save graphs as PDF then further edit to add sample size etc save as 8 X 6.5

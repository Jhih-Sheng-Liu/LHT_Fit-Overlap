library(tidybayes)
library(bayesplot)
library(rstan)
library(pander)
library(cowplot)
library("tidyverse")
library("ggplot2")
library("BayesGrowth")
library("tidybayes")
library("AquaticLifeHistory")
library(rstan)


setwd("Your Working Directory/BayesGrowth_fitting")

SMALKInput <- read.csv("BayesArrange_SMALK_sp.csv")
dim(SMALKInput)


Standard.spList <- c("Clupea harengus","Gadus morhua","Glyptocephalus cynoglossus","Melanogrammus aeglefinus","Merlangius merlangus",
                     "Pleuronectes platessa","Pollachius virens","Scomber scombrus","Sprattus sprattus","Trisopterus esmarkii")

SMALK_Standard.sp <- SMALKInput %>% filter(Species %in% Standard.spList)
dim(SMALK_Standard.sp)

#########  combine all sex here

SMALK_Standard_2Sex <- SMALK_Standard.sp %>% select(-Sex)
dim(SMALK_Standard_2Sex)
#
SMALK_Standard_2Sex_meanLenAtAge <- SMALK_Standard_2Sex %>% group_by(Species,Year,Age) %>% summarise(MeanLen=mean(LngtClass))
SMALK_Standard_2Sex_meanLenAtAge.1 <- SMALK_Standard_2Sex_meanLenAtAge %>% ungroup() %>% rename("Length"="MeanLen") %>% select(Species,Age,Length) %>% filter()
view(SMALK_Standard_2Sex_meanLenAtAge.1)
#write.csv(SMALK_10Standard_2Sex_meanLenAtAge.1,file = "SMALK_10Standard_CombinedSex_meanLenAtAge.csv")

#####################################################################################################################################

Extract_YearlyMaxMinLen <- SMALK_Standard_2Sex %>% group_by(Species,Year) %>% summarise(YearlyMax=max(LngtClass),YearlyMin=min(LngtClass))
Extract_SurveyMeanMaxMin.Se <- Extract_YearlyMaxMinLen %>% ungroup() %>% group_by(Species) %>% summarise(MeanMaxLen=mean(YearlyMax),
                                                                                                         MeanMinLen=mean(YearlyMin),
                                                                                                         SeMaxLen=sd(YearlyMax),
                                                                                                         seMinLen=sd(YearlyMin)) %>% ungroup()
write.csv(Extract_SurveyMeanMaxMin.Se,file="SMALK_10Standard_CombinedSex_SurveyMeanMaxMinSe.csv")


SMALK_10Standard_2Sex_meanLenAtAge.1 <- read.csv("SMALK_10Standard_CombinedSex_meanLenAtAge.csv")



max_size.10sp <- Extract_SurveyMeanMaxMin.Se$MeanMaxLen
max_size_se.10sp <- Extract_SurveyMeanMaxMin.Se$SeMaxLen
birth_size.10sp <- Extract_SurveyMeanMaxMin.Se$MeanMinLen
birth_size_se.10sp <- Extract_SurveyMeanMaxMin.Se$seMinLen # an se cannot be zero
#####





Model <- character()
elpd_diff <- numeric()
se_diff <- numeric() 
elpd_loo <- numeric()
se_elpd_loo <- numeric()
p_loo <- numeric()
se_p_loo <- numeric()
looic <- numeric()
se_looic <- numeric()
looic_Weight <- numeric()

ModelSelection_10sp <- data.frame(Model,elpd_diff,se_diff,elpd_loo,se_elpd_loo,p_loo,se_p_loo,looic,se_looic,looic_Weight)


for (i in 1:length(Standard.spList)) {
  
  
  
  temp_sp_tb <- SMALK_10Standard_2Sex_meanLenAtAge.1 %>% filter(Species==Standard.spList[i]) %>% select(-Species) 
  
  
  LOOIC_results <- Compare_Growth_Models(data = data.frame(temp_sp_tb),
                                         stats = "LooIC",
                                         iter = 10000, 
                                         n_cores = 3,
                                         n.chains = 4,
                                         BurnIn = 5000,
                                         thin = 10, 
                                         Linf = max_size.10sp[i],
                                         Linf.se = max_size_se.10sp[i],
                                         L0 = birth_size.10sp[i],
                                         L0.se = birth_size_se.10sp[i],
                                         verbose = T,
                                         sigma.max = 100,
                                         k.max = 3,
                                         controls = list(adapt_delta=0.95))
  
  
  
  ModelSelection_10sp <- ModelSelection_10sp %>% add_row(LOOIC_results)
  print(paste("Species: ",Standard.spList[i],",finished"))
  
}


Finished_10Standard.spList <- rep(c("Clupea harengus","Gadus morhua","Glyptocephalus cynoglossus","Melanogrammus aeglefinus","Merlangius merlangus",
                                    "Pleuronectes platessa","Pollachius virens","Scomber scombrus","Sprattus sprattus","Trisopterus esmarkii"),each=3)

######################################################
#Only the first 9 species are successfully fitted
ModelSelection_10sp_2Sex <- ModelSelection_10sp
ModelSelection_10sp_2Sex.1 <- tibble(ModelSelection_10sp_2Sex) %>% add_column(Species=Finished_10Standard.spList, .before = "Model")

write.csv(ModelSelection_10sp_2Sex.1,"10sp_CombinedSex_ModelSelect_YearlyMeanAtAge.csv")















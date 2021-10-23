install.packages("dietr")
library("dietr")
library("rfishbase")

setwd("Your Working Directory/Trophic")


#Access 10 std sp's trophic level and cal diet breadth
fishname <- c("Clupea harengus","Gadus morhua","Glyptocephalus cynoglossus","Melanogrammus aeglefinus","Merlangius merlangus",
              "Pleuronectes platessa","Pollachius virens","Scomber scombrus","Sprattus sprattus","Trisopterus esmarkii")


converted.diet <- ConvertFishbaseDiet(ExcludeStage=NULL)




################################################################################################################################
#(!)# estimation based on code from dietr 


common_names <- c("Atlantic herring","Atlantic cod","Witch flounder","Haddock","Whiting",
                  "European plaice","Saithe","Atlantic mackerel","European sprat","Norway pout")
data_length <- numeric()
Trophic_Level <- numeric()
SE_Trophic <- numeric()

#element for create boxplot or geom_pointrange (plot: mean + sd)
fish.Spe <- character()
common.Spe <- character()
Indi.TL <- numeric()
Spe.TL <- numeric()
Spe.TL.sd <- numeric()
#element for create boxplot or geom_pointrange (plot: mean + sd)



for (i in 1:length(fishname)) {
  
  
  
  fish_diets <- converted.diet$DietItems %>% filter(Species==fishname[i]) %>% filter(Stage %in% c("juv./adults","adults"))
  fish_taxonomy <- converted.diet$Taxonomy %>% filter(Species==fishname[i]) %>% filter(Individual %in% unique(fish_diets$Individual))
  
  fish.TL <- DietTroph(DietItems = fish_diets,PreyValues = FishBasePreyVals, Taxonomy =
                         fish_taxonomy, PreyClass=c("FoodI","FoodII","FoodIII","Stage"))
  Trophic_Level <- c(Trophic_Level,unique(fish.TL$Species$TrophicLevel))
  SE_Trophic <- c(SE_Trophic,fish.TL$Species$SE)
  data_length <- c(data_length,length(fish_taxonomy$Individual))
  
  #element for create boxplot or geom_pointrange (plot: mean + sd)
  fish.Spe <- c(fish.Spe,rep(fishname[i],each=nrow(fish.TL$Individual)))
  common.Spe <- c(common.Spe,rep(common_names[i],each=nrow(fish.TL$Individual)))
  Indi.TL <- c(Indi.TL,fish.TL$Individual$TrophicLevel)
  Spe.TL <- c(Spe.TL,rep(fish.TL$Species$TrophicLevel,each=nrow(fish.TL$Individual)))
  Spe.TL.sd <- c(Spe.TL.sd,rep(fish.TL$Species$SE,each=nrow(fish.TL$Individual)))
  #element for create boxplot or geom_pointrange (plot: mean + sd)
}


all_fish_Trophic <- data.frame(fishname,Trophic_Level,SE_Trophic,data_length)

juv.adu_df <- data.frame(fish.Spe,common.Spe,Indi.TL,Spe.TL,Spe.TL.sd) %>% as.tibble()

#################################################################################################################################33
#boxplot------------------------------------------------------------------------------------------------------------------------
juv.adu_df %>% ggplot(mapping = aes(x = reorder(common.Spe,-Spe.TL), y = Indi.TL,fill=common.Spe))+
  geom_boxplot()+ #remove outlier
  geom_jitter(alpha = .12, colour = "black", fill = "white")+
  theme(axis.title = element_text(face = "bold",color = "gray20", size = 24), 
        axis.text = element_text(face = "bold",color = "black", size = 18),
        legend.text = element_text(face = "plain",color = "black",size = 16),
        legend.title = element_text(face = "bold",color = "black",size = 16),
        title =element_text(size=16, face='bold'))+
  theme(axis.text.x = element_text(face = "bold",color = "gray20", size = 14,angle = 65, vjust = 0.6))+
  xlab("Species")+ggtitle("                Trophic level (10 standard species in NS,all adults)")

ggsave("all.Indi_TL.tiff",width = 12,height = 10)






############################################################################################################################
#geom_pointrange (plot: mean + sd)-----------------------------------------------------------------------------

juv.adu_df 

ggplot(juv.adu_df , aes(x =Spe.TL, y = common.Spe, xmin = Spe.TL-Spe.TL.sd, xmax = Spe.TL+Spe.TL.sd)) +
  geom_pointrange(size=.8)+
  geom_point(aes(x =Indi.TL, y = common.Spe),alpha = .18, colour = "black") +
  geom_errorbarh(height=.2)+
  theme(axis.title = element_text(face = "bold",color = "gray20", size = 24), 
        axis.text = element_text(face = "bold",color = "black", size = 18),
        legend.text = element_text(face = "plain",color = "black",size = 16),
        legend.title = element_text(face = "bold",color = "black",size = 16),
        title =element_text(size=16, face='bold'))+
  theme(axis.text.x = element_text(face = "bold",color = "gray20", size = 14))+
  xlab("Trophic Level")+ylab("Common Name")+ggtitle("                Trophic level (10 standard species in NS,all adults)")
ggsave("juv.adu_TL_with_mean_sd.tiff",width = 12,height = 10)



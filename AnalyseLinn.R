
source("RanunculusData.R")
library("lme4")
library("broom")
library("nlme")


##################################
######## Biomasse analyser #######
 
#Antar dataene er normalfordelte. Her har vi en lm uten random effects
Biomass
hist(log(Biomass$Seed_mass), breaks = 20)
Model <- lm(log(Seed_mass) ~ Biomass*Stage, data = Biomass) 
summary(Model)  

#Siden modellen er log transformert bruker vi exp for å få tilbake dataene til vanlig skala slik at vi kan tolke dataene
tidy(Model) %>%
  mutate(estimate = exp(estimate))


#####Grafer med biomasse som variabel, og AIC test#######

############################################
#Graf med biomasse og frøvekt. Ser på hvert år hver for seg
BiomassAndSeedmass <- ggplot(Biomass, aes(x = Biomass, y = log(Seed_mass), color = Stage))+ 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ Year)
ggsave(BiomassAndSeedmass, filename = "Figurer/BiomassAndSeedmass.jpeg", height = 6, width = 8)

#Model med random effects
ModelBiomass0 <- lmer(log(Seed_mass) ~ 1 + (1|BlockID), data = Biomass %>% filter(Year == 2016), REML = FALSE) 
ModelBiomass1 <- lmer(log(Seed_mass) ~ Biomass + (1|BlockID), data = Biomass %>% filter(Year == 2016), REML = FALSE)
ModelBiomass2 <- lmer(log(Seed_mass) ~ Stage + (1|BlockID), data = Biomass %>% filter(Year == 2016), REML = FALSE)
ModelBiomass3 <- lmer(log(Seed_mass) ~ Biomass+Stage + (1|BlockID), data = Biomass %>% filter(Year == 2016), REML = FALSE)
ModelBiomass4 <- lmer(log(Seed_mass) ~ Biomass*Stage + (1|BlockID), data = Biomass %>% filter(Year == 2016), REML = FALSE)
summary(ModelBiomass3)

#Gjør AIC test
AIC(ModelBiomass0, ModelBiomass1, ModelBiomass2, ModelBiomass3, ModelBiomass4)

#ModelBiomass 3 har lavest AIC verdi for 2017 og 2016 dataene. .

########################################
#Graf med biomasse og antall ovuler
BiomassAndOvulenumber <- ggplot(Biomass, aes(x = Biomass, y = Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(BiomassAndOvulenumber, filename = "Figurer/BiomassAndOvulenumber.jpeg", height = 6, width = 8)

#Model med random effects. Endret kode fra biomasse x frøvekt. Kan kun bruke lme om dataene er normalfordelt, men må bruke glmer her. Legger inn family = poisson siden det er telledata. Også endret form på random effekts, fordi "random = ~ 1 | BlockID" ikke fungerer med "glmer".
ModelOvule0 <- glmer(Ovule_number ~ 1 + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016))
ModelOvule1 <- glmer(Ovule_number ~ Biomass + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelOvule2 <- glmer(Ovule_number ~ Stage + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelOvule3 <- glmer(Ovule_number ~ Biomass+Stage + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelOvule4 <- glmer(Ovule_number ~ Biomass*Stage + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
summary(ModelOvule3)

#AIC test
AIC(ModelOvule0, ModelOvule1, ModelOvule2, ModelOvule3, ModelOvule4)
#Modell 3 har lavest verdi

#######################################################
# Graf med biomasse og antall frø. Ser på år hver for seg
BiomassAndSeednumber <- ggplot(Biomass, aes(x = Biomass, y = Seed_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(BiomassAndSeednumber, filename = "Figurer/BiomassAndSeednumber.jpeg", height = 6, width = 8)


#Kan gjøre AIC test her for å se om modellen vi valgte over er den beste (?)
ModelSeed0 <- glmer(Seed_number ~ 1 + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeed1 <- glmer(Seed_number ~ Biomass + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeed2 <- glmer(Seed_number ~ Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeed3 <- glmer(Seed_number ~ Biomass+Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeed4 <- glmer(Seed_number ~ Biomass*Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
summary(ModelSeed3)

AIC(ModelSeed0, ModelSeed1, ModelSeed2, ModelSeed3, ModelSeed4)
#Model nr 3 er beste modellen



##################################################
#Graf med antall frø + antall ovule og hvordan biomasse påvirker her
BiomassAndTotalachenes <- ggplot(Biomass, aes(x = Biomass, y = Seed_number + Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(BiomassAndTotalachenes, filename = "Figurer/BiomassAndTotalachenes.jpeg", height = 6, width = 8)

#Modeller med random effects
ModelSeedOvule0<- glmer(Seed_number + Ovule_number ~ 1 + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeedOvule1 <- glmer(Seed_number + Ovule_number ~ Biomass + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeedOvule2 <- glmer(Seed_number + Ovule_number ~ Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeedOvule3 <- glmer(Seed_number + Ovule_number ~ Biomass+Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeedOvule4 <- glmer(Seed_number + Ovule_number ~ Biomass*Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
summary(ModelSeedOvule3)

#AIC test
AIC(ModelSeedOvule0, ModelSeedOvule1, ModelSeedOvule2, ModelSeedOvule3, ModelSeedOvule4)
#Model 3 har lavest AIC verdi og derfor den beste modellen. 

##################################################
#Graf med Seed_potential = antall frø/(antall frø + antall ovule) og hvordan biomasse påvirker her.

BiomassAndSeedpotential <- ggplot(Biomass1, aes(x = Biomass, y = Seed_potential, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(BiomassAndSeedpotential, filename = "Figurer/BiomassAndSeedpotential.jpeg", height = 6, width = 8)

#Hvilken model med random effects passer best
ModelSeedset0 <- glmer(Seed_potential ~ 1 + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
ModelSeedset1 <- glmer(Seed_potential ~ Biomass + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
ModelSeedset2 <- glmer(Seed_potential ~ Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
ModelSeedset3 <- glmer(Seed_potential ~ Biomass+Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
ModelSeedset4 <- glmer(Seed_potential ~ Biomass*Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
summary(ModelSeedset3)


AIC(ModelSeedset0, ModelSeedset1, ModelSeedset2, ModelSeedset3, ModelSeedset4)
#Modell 3 har lavest verdi. 

###################################################
#Korrelasjonstester, går det ann å bare bruke seedmass, eller har antall frø og ovuler noe å si?

cor.test(Biomass$Seed_mass, Biomass$Seed_number)

plot(Biomass$Seed_mass, Biomass$Seed_number)
plot(Biomass$Seed_mass, Biomass$Ovule_number)
plot(Biomass$Seed_mass, Biomass$Tot_Ovule) 
#Virker som antall frø og frømasse er greit korrelert (?), men ikke når vi ser på antall ovuler og frømasse, eller tot ovule


#####################################################################

####### Besøksraten og frøvekt ####
#Pollination 2 = besøksraten regnet ut fra RanunculusData.R
MeanVisitRate <- pollination2 %>% 
  select(day.poll, year.poll, stage, site, tot.flowers, std.fly) %>% 
  group_by(year.poll, stage, site) %>% 
  summarise(mean.visit.rate = mean(std.fly), mean.tot.flowers = mean(tot.flowers)) %>% 
  rename(Year = year.poll, Stage = stage, Site = site) %>% 
  left_join(Biomass, by = c("Year", "Stage", "Site" )) %>% 
  filter(mean.visit.rate != Inf, !is.na (Seed_mass))

# Lage plot med seed mass x visitation rate ()
#Antar dataene er normalfordelte. Her har vi en lm uten random effects
MeanVisitRate
hist(log(mean.visit.rate$Seed_mass), breaks = 20)
Model2 <- lm(log(Seed_mass) ~ mean.visit.rate*Stage, data = MeanVisitRate) 
summary(Model2)  

#Siden modellen er log transformert bruker vi exp for å få tilbake dataene til vanlig skala slik at vi kan tolke dataene
tidy(Model2) %>%
  mutate(estimate = exp(estimate))

#Data med mean.visist.rate x seed_mass
MeanvisitationrateAndSeedmass <- MeanVisitRate %>% 
  ggplot(aes(x = mean.visit.rate, y = Seed_mass, color = Stage)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(~ Year, scales ="free")
ggsave(MeanvisitationrateAndSeedmass, filename = "Figurer/MeanvisitationrateAndSeedmass.jpeg", height = 6, width = 8)


ModelSeedmassvisit0 <- lmer(log(Seed_mass) ~ 1 + (1|BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
ModelSeedmassvisit1 <- lmer(log(Seed_mass) ~ mean.visit.rate + (1|BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
ModelSeedmassvisit2 <- lmer(log(Seed_mass) ~ Stage + (1|BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
ModelSeedmassvisit3 <- lmer(log(Seed_mass) ~ mean.visit.rate+Stage + (1|BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
ModelSeedmassvisit4 <- lmer(log(Seed_mass) ~ mean.visit.rate*Stage + (1|BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
summary(ModelSeedmassvisit2)

AIC(ModelSeedmassvisit0, ModelSeedmassvisit1, ModelSeedmassvisit2, ModelSeedmassvisit3, ModelSeedmassvisit4)
#Seedmassvisit 2 er den beste modellen i 2017 og 2016. 

# Graf med visitation rate og antall frø. Ser på år hver for seg
MeanvisitationrateAndSeednumber <- ggplot(MeanVisitRate, aes(x = mean.visit.rate, y = Seed_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year) 
ggsave(MeanvisitationrateAndSeednumber, filename = "Figurer/MeanvisitationrateAndSeednumber.jpeg", height = 6, width = 8)


ModelSeednumbervisit0 <- glmer(Seed_number ~ 1 + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelSeednumbervisit1 <- glmer(Seed_number ~ mean.visit.rate + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelSeednumbervisit2 <- glmer(Seed_number ~ Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelSeednumbervisit3 <- glmer(Seed_number ~ mean.visit.rate+Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelSeednumbervisit4 <- glmer(Seed_number ~ mean.visit.rate*Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
summary(ModelSeednumbervisit2)

AIC(ModelSeednumbervisit0, ModelSeednumbervisit1, ModelSeednumbervisit2, ModelSeednumbervisit3, ModelSeednumbervisit4)

# Graf med visitation rate og antall ovuler. Ser på år hver for seg
MeanvisitationrateAndOvulenumber <- ggplot(MeanVisitRate, aes(x = mean.visit.rate, y = Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year) 
ggsave(MeanvisitationrateAndOvulenumber, filename = "Figurer/MeanvisitationrateAndOvulenumber.jpeg", height = 6, width = 8)


#Kan gjøre AIC test her for å se om modellen vi valgte over er den beste (?)
ModelOvulenumbervisit0 <- glmer(Ovule_number ~ 1 + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelOvulenumbervisit1 <- glmer(Ovule_number ~ mean.visit.rate + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelOvulenumbervisit2 <- glmer(Ovule_number ~ Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelOvulenumbervisit3 <- glmer(Ovule_number ~ mean.visit.rate+Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelOvulenumbervisit4 <- glmer(Ovule_number ~ mean.visit.rate*Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
summary(ModelSeednumbervisit3)

AIC(ModelOvulenumbervisit0, ModelOvulenumbervisit1, ModelOvulenumbervisit2, ModelOvulenumbervisit3, ModelOvulenumbervisit4)
#Mulig å bruke dette?

# Graf med visitation rate og antall frø + ovuler. Ser på år hver for seg
MeanvisitationrateAndAchenenumber <- ggplot(MeanVisitRate, aes(x = mean.visit.rate, y = Tot_Ovule, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year) 
ggsave(MeanvisitationrateAndAchenenumber, filename = "Figurer/MeanvisitationrateAndAchenenumber.jpeg", height = 6, width = 8)


#Kan gjøre AIC test her for å se om modellen vi valgte over er den beste (?)
ModelAchenenumbervisit0 <- glmer(Tot_Ovule ~ 1 + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelAchenenumbervisit1 <- glmer(Tot_Ovule ~ mean.visit.rate + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelAchenenumbervisit2 <- glmer(Tot_Ovule ~ Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelAchenenumbervisit3 <- glmer(Tot_Ovule ~ mean.visit.rate+Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
ModelAchenenumbervisit4 <- glmer(Tot_Ovule ~ mean.visit.rate*Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
summary(ModelAchenenumbervisit1)

AIC(ModelAchenenumbervisit0, ModelAchenenumbervisit1, ModelAchenenumbervisit2, ModelAchenenumbervisit3, ModelAchenenumbervisit4)
#Mulig å bruke dette????

# Graf med visitation rate og antall frø + ovuler. Ser på år hver for seg
MeanvisitationrateAndSeedPotential <- ggplot(MeanVisitRate, aes(x = mean.visit.rate, y = Seed_potential, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year) 
ggsave(MeanvisitationrateAndSeedPotential, filename = "Figurer/MeanvisitationrateAndSeedPotential.jpeg", height = 6, width = 8)


#Kan gjøre AIC test her for å se om modellen vi valgte over er den beste (?)
ModelSeedpotvisit0 <- glmer(Seed_potential ~ 1 + (1 | BlockID) + offset(log(Tot_Ovule)), family = "binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule)
ModelSeedpotvisit1 <- glmer(Seed_potential ~ mean.visit.rate + (1 | BlockID) + offset(log(Tot_Ovule)), family = "binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule)
ModelSeedpotvisit2 <- glmer(Seed_potential ~ Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family = "binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule)
ModelSeedpotvisit3 <- glmer(Seed_potential ~ mean.visit.rate+Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family = "binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule)
ModelSeedpotvisit4 <- glmer(Seed_potential ~ mean.visit.rate*Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family = "binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule)
summary(ModelSeedpotvisit3)

AIC(ModelSeedpotvisit0, ModelSeedpotvisit1, ModelSeedpotvisit2, ModelSeedpotvisit3, ModelSeedpotvisit4)
#Model 3 har lavest AIC

##################################################
## Fenologi
#Plot med antall blomster x frø, for å se om det er konkurranse ller fasilitering
#Kjøre for mean.visit.rate og mean.tot.flowers. Se på begge år, og om 2017 året virkelig har en negativ trend for F og E. Trenger ikke log transformere her.

#Fenologi og seed mass

PhenologyAndSeedmass <- ggplot(MeanVisitRate, aes(x = mean.tot.flowers, y = Seed_mass, color = Stage)) + 
  geom_point() +
  geom_smooth(method = "lm") +
    facet_wrap(~ Year)
ggsave(PhenologyAndSeedmass, filename = "Figurer/PhenologyAndSeedmass.jpeg", height = 6, width = 8)

PhenologySeedmass0 <- lmer(log(Seed_mass) ~ 1 + (1 | BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
PhenologySeedmass1 <- lmer(log(Seed_mass) ~ mean.tot.flowers + (1 | BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
PhenologySeedmass2 <- lmer(log(Seed_mass) ~ Stage + (1 | BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
PhenologySeedmass3 <- lmer(log(Seed_mass) ~ mean.tot.flowers+Stage + (1 | BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
PhenologySeedmass4 <- lmer(log(Seed_mass) ~ mean.tot.flowers*Stage + (1 | BlockID), data = MeanVisitRate %>% filter(Year == 2016), REML = FALSE)
summary(PhenologySeedmass4)

AIC(PhenologySeedmass0, PhenologySeedmass1, PhenologySeedmass2, PhenologySeedmass3, PhenologySeedmass4)
#modell 4 for 2017 og 2016

#Fenologi og antall ovuler
PhenologyAndOvulenumber <- ggplot(MeanVisitRate, aes(x = mean.tot.flowers, y = Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(PhenologyAndOvulenumber, filename = "Figurer/PhenologyAndOvulenumber.jpeg", height = 6, width = 8)

PhenologyOvule0 <- glmer(Ovule_number ~ 1 + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016))
PhenologyOvule1 <- glmer(Ovule_number ~ mean.tot.flowers + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
PhenologyOvule2 <- glmer(Ovule_number ~ Stage + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
PhenologyOvule3 <- glmer(Ovule_number ~ mean.tot.flowers+Stage + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
PhenologyOvule4 <- glmer(Ovule_number ~ mean.tot.flowers*Stage + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
summary(PhenologyOvule3)

#AIC test
AIC(PhenologyOvule0, PhenologyOvule1, PhenologyOvule2, PhenologyOvule3, PhenologyOvule4)
#Modell 3 har lavest verdi

#######################################################
#Fenolgi og antall frø
PhenologyAndSeedNumber <- ggplot(MeanVisitRate, aes(x = mean.tot.flowers, y = Seed_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(PhenologyAndSeedNumber, filename = "Figurer/PhenologyAndSeedNumber.jpeg", height = 6, width = 8)

PhenologySeed0 <- glmer(Seed_number ~ 1 + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016))
PhenologySeed1 <- glmer(Seed_number ~ mean.tot.flowers + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
PhenologySeed2 <- glmer(Seed_number ~ Stage + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
PhenologySeed3 <- glmer(Seed_number ~ mean.tot.flowers+Stage + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
PhenologySeed4 <- glmer(Seed_number ~ mean.tot.flowers*Stage + (1 | BlockID), family="poisson", data = MeanVisitRate %>% filter(Year == 2016)) 
summary(PhenologyOvule4)

AIC(PhenologySeed0, PhenologySeed1, PhenologySeed2, PhenologySeed3, PhenologySeed4)
#Model nr 4 er beste modellen



##################################################
#Fenologi og totalt antall achener
PhenologyAndTotalachenes <- ggplot(MeanVisitRate, aes(x = mean.tot.flowers, y = Seed_number + Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(PhenologyAndTotalachenes, filename = "Figurer/PhenologyAndTotalachenes.jpeg", height = 6, width = 8)

#Modeller med random effects
PhenologyAchene0<- glmer(Seed_number + Ovule_number ~ 1 + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
PhenologyAchene1 <- glmer(Seed_number + Ovule_number ~ mean.tot.flowers + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
PhenologyAchene2 <- glmer(Seed_number + Ovule_number ~ Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
PhenologyAchene3 <- glmer(Seed_number + Ovule_number ~ mean.tot.flowers+Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
PhenologyAchene4 <- glmer(Seed_number + Ovule_number ~ mean.tot.flowers*Stage + (1 | BlockID), family = "poisson", data = MeanVisitRate %>% filter(Year == 2016))
summary(PhenologyAchene1)

#AIC test
AIC(PhenologyAchene0, PhenologyAchene1, PhenologyAchene2, PhenologyAchene3, PhenologyAchene4)
#Model 3 har lavest AIC verdi og derfor den beste modellen. 

##################################################
#Frøpotensiale og fenologi

PhenologyAndSeedpotential <- ggplot(MeanVisitRate, aes(x = mean.tot.flowers, y = Seed_potential, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)
ggsave(PhenologyAndSeedpotential, filename = "Figurer/PhenologyAndSeedpotential.jpeg", height = 6, width = 8)

#Hvilken model med random effects passer best
PhenologySeedpot0 <- glmer(Seed_potential ~ 1 + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule) 
PhenologySeedpot1 <- glmer(Seed_potential ~ mean.tot.flowers + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule) 
PhenologySeedpot2 <- glmer(Seed_potential ~ Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule) 
PhenologySeedpot3 <- glmer(Seed_potential ~ mean.tot.flowers+Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule) 
PhenologySeedpot4 <- glmer(Seed_potential ~ mean.tot.flowers*Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = MeanVisitRate %>% filter(Year == 2016), weights = Tot_Ovule) 
summary(PhenologySeedpot3)


AIC(PhenologySeedpot0, PhenologySeedpot1, PhenologySeedpot2, PhenologySeedpot3, PhenologySeedpot4)
#Modell 3 har lavest verdi.
  
##############################################3
## Pollen limitation? seedset x pollen treatment. Hvor seedset er enten seed potential eller seed mass, og pollentreatment er HP eller C.
  
#Seedpotential og PL

PLAndSeedPot <- ggplot(Biomass, aes(x = Treatment, y = Seed_potential, color = Stage)) +
  geom_boxplot() +
  facet_wrap(~ Stage)
ggsave(PLAndSeedPot, filename = "Figurer/PLAndSeedPot.jpeg", height = 6, width = 8)


#Hvilken model med random effects passer best
PLSeedPot0 <- glmer(Seed_potential ~ 1 + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
PLSeedPot1 <- glmer(Seed_potential ~ Stage + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
PLSeedPot2 <- glmer(Seed_potential ~ Treatment + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
PLSeedPot3 <- glmer(Seed_potential ~ Stage+Treatment + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
#PLSeedPot4 <- glmer(Seed_potential ~ Stage*Treatment + (1 | BlockID) + offset(log(Tot_Ovule)), family="binomial", data = Biomass %>% filter(Year == 2016), weights = Tot_Ovule) 
# Klarer ikke kjøre koden over, variabler ikke mulige å bruke her?
summary(PLSeedPot3)

#AIC
AIC(PLSeedPot0, PLSeedPot1, PLSeedPot2, PLSeedPot3) #PLSeedPot4)



#Seed mass og PL
PLAndSeedmass <- ggplot(Biomass, aes(x = Treatment, y = Seed_mass, color = Stage)) +
  geom_boxplot() +
  facet_wrap(Year ~ Stage)
ggsave(PLAndSeedmass, filename = "Figurer/PLAndSeedmass.jpeg", height = 6, width = 8)

PLSeedmass0 <- lmer(log(Seed_mass) ~ 1 + (1 | BlockID), data = Biomass %>% filter(Year == 2017), REML = FALSE)
PLSeedmass1 <- lmer(log(Seed_mass) ~ Treatment + (1 | BlockID), data = Biomass %>% filter(Year == 2017), REML = FALSE)
PLSeedmass2 <- lmer(log(Seed_mass) ~ Stage + (1 | BlockID), data = Biomass %>% filter(Year == 2017), REML = FALSE)
PLSeedmass3 <- lmer(log(Seed_mass) ~ Treatment+Stage + (1 | BlockID), data = Biomass %>% filter(Year == 2017), REML = FALSE)
PLSeedmass4 <- lmer(log(Seed_mass) ~ Treatment*Stage + (1 | BlockID), data = Biomass %>% filter(Year == 2017), REML = FALSE)
summary(PLSeedmass2)

AIC(PLSeedmass0, PLSeedmass1, PLSeedmass2, PLSeedmass3, PLSeedmass4)
#Model 2 i 2016 og 2017

#Look at differences in seed number and seed weight.
SeednumberAndSeedweight <- ggplot(Biomass, aes(x = Seed_number, y = Seed_mass, color = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ Stage)
ggsave(SeednumberAndSeedweight, filename = "Figurer/SeednumberAndSeedweight.jpeg", height = 6, width = 8)
 # filter(Stage != "F") få inn stage koden

 


#Hvilken model med random effects passer best. ENDRE KODE!
ModelSNSM0 <- lmer(log(Seed_mass) ~ 1 + (1 | BlockID), data = Biomass %>% filter(Year == 2016))
ModelSNSM1 <- lmer(log(Seed_mass) ~ Treatment + (1 | BlockID), data = Biomass %>% filter(Year == 2016))
ModelSNSM2 <- lmer(log(Seed_mass) ~ Seed_number + (1 | BlockID), data = Biomass1 %>% filter(Year == 2016))
ModelSNSM3 <- lmer(log(Seed_mass) ~ Treatment+Seed_number + (1 | BlockID), data = Biomass %>% filter(Year == 2016))
ModelSNSM4 <- lmer(log(Seed_mass) ~ Treatment*Seed_number + (1 | BlockID), data = Biomass %>% filter(Year == 2016))
summary(ModelSNSM2)

#AIC
AIC(ModelSNSM0, ModelSNSM1, ModelSNSM2, ModelSNSM3, ModelSNSM4)

##############################
#Temperatur og frødata

#Graf med temperatur og frøvekt. Ser på hvert år hver for seg
ggplot(WeatherAndBiomass, aes(x = CumTemp, y = log(Seed_mass), color = Stage))+ 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ Year)
ggsave(Plot, filename = "Figurer/Plot.jpeg", height = 6, width = 8)

#Model med random effects
ModelCumTempSM0 <- lme(log(Seed_mass) ~ 1, random = ~ 1 | BlockID, data = WeatherAndBiomass %>% filter(Year == 2017)) #ser på 2016 dataene alene (gjøre egen for 2017)
ModelCumTempSM1 <- lme(log(Seed_mass) ~ CumTemp, random = ~ 1 | BlockID, data = WeatherAndBiomass %>% filter(Year == 2017))
ModelCumTempSM2 <- lme(log(Seed_mass) ~ Stage, random = ~ 1 | BlockID, data = WeatherAndBiomass %>% filter(Year == 2017))
ModelCumTempSM3 <- lme(log(Seed_mass) ~ CumTemp+Stage, random = ~ 1 | BlockID, data = WeatherAndBiomass %>% filter(Year == 2017))
ModelCumTempSM4 <- lme(log(Seed_mass) ~ CumTemp*Stage, random = ~ 1 | BlockID, data = WeatherAndBiomass %>% filter(Year == 2017))
summary(ModelCumTempSM2)

#Gjør AIC test
AIC(ModelCumTempSM0, ModelCumTempSM1, ModelCumTempSM2, ModelCumTempSM3, ModelCumTempSM4)

# Graf med temperatur og antall frø. Ser på år hver for seg
ggplot(WeatherAndBiomass, aes(x = CumTemp, y = Seed_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)



#Kan gjøre AIC test her for å se om modellen vi valgte over er den beste (?)
ModelCumTempSN0 <- glmer(Seed_number ~ 1 + (1 | BlockID), family = "poisson", data = WeatherAndBiomass %>% filter(Year == 2016))
ModelCumTempSN1 <- glmer(Seed_number ~ CumTemp.cen + (1 | BlockID), family = "poisson", data = WeatherAndBiomass %>% filter(Year == 2016))
ModelCumTempSN2 <- glmer(Seed_number ~ Stage + (1 | BlockID), family = "poisson", data = WeatherAndBiomass %>% filter(Year == 2016))
ModelCumTempSN3 <- glmer(Seed_number ~ CumTemp.cen+Stage + (1 | BlockID), family = "poisson", data = WeatherAndBiomass %>% filter(Year == 2016))
ModelCumTempSN4 <- glmer(Seed_number ~ CumTemp.cen*Stage + (1 | BlockID), family = "poisson", data = WeatherAndBiomass %>% filter(Year == 2016))
summary(ModelCumTempSN4)

AIC(ModelCumTempSN0, ModelCumTempSN1, ModelCumTempSN2, ModelCumTempSN3, ModelCumTempSN4)


#Graf med temperatur og antall ovuler
ggplot(WeatherAndBiomass, aes(x = CumTemp, y = Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)


ModelCumTempON0 <- glmer(Ovule_number ~ 1 + (1 | BlockID), family="poisson", data = WeatherAndBiomass %>% filter(Year == 2016))
ModelCumTempON1 <- glmer(Ovule_number ~ CumTemp + (1 | BlockID), family="poisson", data = WeatherAndBiomass %>% filter(Year == 2016)) 
ModelCumTempON2 <- glmer(Ovule_number ~ Stage + (1 | BlockID), family="poisson", data = WeatherAndBiomass %>% filter(Year == 2016)) 
ModelCumTempON3 <- glmer(Ovule_number ~ CumTemp+Stage + (1 | BlockID), family="poisson", data = WeatherAndBiomass %>% filter(Year == 2016)) 
ModelCumTempONe4 <- glmer(Ovule_number ~ CumTemp*Stage + (1 | BlockID), family="poisson", data = WeatherAndBiomass %>% filter(Year == 2016)) 
summary(ModelOvule3)

#Kan gjøre AIC test her for å se hvilken modell som er den beste
AIC(ModelOvule0, ModelOvule1, ModelOvule2, ModelOvule3, ModelOvule4)

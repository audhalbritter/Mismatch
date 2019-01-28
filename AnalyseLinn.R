
source("RanunculusData.R")
library("lme4")
library("broom")
library("nlme")
library("dplyr")


##################################
######## Biomasse analyser #######
Biomass <- read.csv("Biomass1617.csv", sep=";") %>% 
  mutate(BlockID = as.factor(paste(Stage, Site, Block))) %>% #gir block en mer presis id
  filter(!is.na(Seed_mass)) %>%  #fjerner alle NA 
  filter(!is.na(Biomass)) %>%
  #filter(!is.na(Tot_ovule)) #fjerner alle NA
 
  

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
ggplot(Biomass, aes(x = Biomass, y = log(Seed_mass), color = Stage))+ 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ Year)

#Model med random effects
ModelBiomass <- lme(log(Seed_mass) ~ Biomass*Stage, random = ~ 1 | BlockID, data = Biomass %>% filter(Year == 2016)) #ser på 2016 dataene alene (gjøre egen for 2017)
summary(ModelBiomass)

#Seed mass x Biomass forteller oss at det er sammenheng mellom biomasse og frøvekt. Jo mer biomasse jo flere frø.

########################################
#Graf med biomasse og antall ovuler
ggplot(Biomass, aes(x = Biomass, y = Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)

#Model med random effects. Endret kode fra biomasse x frøvekt. Kan kun bruke lme om dataene er normalfordelt, men må bruke glme her. Legger inn family = poisson siden det er telledata. Også endret form på random effekts, fordi "random = ~ 1 | BlockID" ikke fungerer med "glmer".
ModelOvule0 <- glmer(Ovule_number ~ Biomass + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelOvule1 <- glmer(Ovule_number ~ Stage + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelOvule2 <- glmer(Ovule_number ~ Biomass+Stage + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelOvule3 <- glmer(Ovule_number ~ Biomass*Stage + (1 | BlockID), family="poisson", data = Biomass %>% filter(Year == 2016)) 
summary(ModelOvule0)

#Kan gjøre AIC test her for å se hvilken modell som er den beste
AIC(ModelOvule0, ModelOvule1, ModelOvule2, ModelOvule3)
#Modell 0 har lavest verdi og er den som forklarer mest, altså biomassen sier mer om antall ovuler enn stage og intraksjon mellom biomasse og stage (?). Både antall ovule og biomasse er signifikante

#######################################################
# Graf med biomasse og antall frø. Ser på år hver for seg
ggplot(Biomass, aes(x = Biomass, y = Seed_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year) 

#Kan gjøre AIC test her for å se om modellen vi valgte over er den beste (?)
ModelSeed0 <- glmer(Seed_number ~ Biomass + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeed1 <- glmer(Seed_number ~ Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeed2 <- glmer(Seed_number ~ Biomass+Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeed3 <- glmer(Seed_number ~ Biomass*Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
summary(ModelSeed3)

AIC(ModelSeed0, ModelSeed1, ModelSeed2, ModelSeed3)
#Model nr 3 er den som har lavest verdi, slik at interaksjonen mellom biomasse og stage forklarer best resultatet vi ser (?).

##################################################
#Graf med antall frø + antall ovule og hvordan biomasse påvirker her
ggplot(Biomass, aes(x = Biomass, y = Seed_number + Ovule_number, color = Stage)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Year)

#Modeller med random effects
ModelSeedOvule0 <- glmer(Seed_number + Ovule_number ~ Biomass + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeedOvule1 <- glmer(Seed_number + Ovule_number ~ Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeedOvule2 <- glmer(Seed_number + Ovule_number ~ Biomass+Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
ModelSeedOvule3 <- glmer(Seed_number + Ovule_number ~ Biomass*Stage + (1 | BlockID), family = "poisson", data = Biomass %>% filter(Year == 2016))
summary(ModelSeedOvule2)

#AIC test
AIC(ModelSeedOvule0, ModelSeedOvule1, ModelSeedOvule2, ModelSeedOvule3)
#Model 2 har lavest AIC verdi og derfor den beste modellen. Den forteller oss at stage L har høyest verdi av frø+ovule. 

##################################################
#Graf med antall frø/(antall frø + antall ovule) og hvordan biomasse påvirker her. Hvordan lage denne grafen?
#ggplot(Biomass, aes(x = Biomass, y = Tot_ovule, color = Stage)) +
  #geom_point() +
  #geom_smooth(method = "lm") +
  #facet_wrap(~ Year)

#Hvilken model med random effects passer best
ModelSeedset0 <- glmer(Seed_number ~ Biomass + (1 | BlockID) + offset(log(Tot_ovule)), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelSeedset1 <- glmer(Seed_number ~ Stage + (1 | BlockID) + offset(log(Tot_ovule)), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelSeedset2 <- glmer(Seed_number ~ Biomass+Stage + (1 | BlockID) + offset(log(Tot_ovule)), family="poisson", data = Biomass %>% filter(Year == 2016)) 
ModelSeedset3 <- glmer(Seed_number ~ Biomass*Stage + (1 | BlockID) + offset(log(Tot_ovule)), family="poisson", data = Biomass %>% filter(Year == 2016)) 
summary(ModelSeedset1)

# OBS! får ikke helt til å fjerne NA fra Tot_ovule i linje 15, må fikse dette for å kjøre formel

AIC(ModelSeedset0, ModelSeedset1, ModelSeedset2, ModelSeedset3)
#Modell 1 har lavest verdi. Resultatene viser at stage M har flest frø produsert ut i fra mulig utgangspunkt, og stage L har lavest mengde produsert frø. Både stage E og L er signifikante.

###################################################
#Korrelasjonstester, går det ann å bare bruke seedmass, eller har antall frø og ovuler noe å si?

cor.test(Biomass$Seed_mass, Biomass$Seed_number)

plot(Biomass$Seed_mass, Biomass$Seed_number)
plot(Biomass$Seed_mass, Biomass$Ovule_number)
#plot(Biomass$Seed_mass, Biomass$Tot_ovule) får ikke helt til å fjerne NA fra Tot_ovule i linje 15, må fikse dette.
#Virker som antall frø og frømasse er greit korrelert (?), men ikke når vi ser på antall ovuler og frømasse.

################################################################################
# Funket ikke helt, ser på senere
BiomassResult <- Biomass %>% 
  group_by(Year) %>% 
  do(fit = lme(Seed_mass ~ Biomass*Stage + (1 | BlockID), data = .))
tidy(BiomassResult, fit) %>% 
  mutate(estimate = exp(estimate))


####### Besøksraten og frøvekt ####
#Pollination 2 = besøksraten regnet ut fra RanunculusData.R
MeanVisitRate <- pollination2 %>% 
  select(day.poll, year.poll, stage, site, tot.flowers, std.fly) %>% 
  group_by(year.poll, stage, site) %>% 
  summarise(mean.visit.rate = mean(std.fly), mean.tot.flowers = mean(tot.flowers)) %>% 
  rename(Year = year.poll, Stage = stage, Site = site) %>% 
  left_join(Biomass, by = c("Year", "Stage", "Site" )) %>% 
  filter(mean.visit.rate != Inf)


hist(log(Biomass$Seed_mass), breaks = 20)
Model <- lm(log(Seed_mass) ~ mean.visit.rate*Stage, data = MeanVisitRate %>% filter(Year == 2017)) 
summary(Model) 

# Lage plot med seed mass x visitation rate
ggplot(MeanVisitRate, aes(x = mean.visit.rate, y = log(Seed_mass), color = Stage)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ Year)




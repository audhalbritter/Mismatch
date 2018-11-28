
source("RanunculusData.R")
library("lme4")
library("broom")
library("nlme")

######## Biomasse analyse #######
Biomass <- Biomass %>% 
  mutate(BlockID = as.factor(paste(Stage, Site, Block))) %>% #gir block en mer presis id
  filter(!is.na(Seed_mass)) %>%  #fjerner alle NA 
  filter(!is.na(Biomass)) #fjerner alle NA

#Antar dataene er normalfordelte, lm er uten random effects
Biomass
hist(log(Biomass$Seed_mass), breaks = 20)
Model <- lm(log(Seed_mass) ~ Biomass*Stage, data = Biomass) 
summary(Model)  

#Siden modellen er log transformert bruker vi exp for å få tilbake dataene til vanlig skala slik at vi kan tolke dataene
tidy(Model) %>%
  mutate(estimate = exp(estimate))

#Lager graf med biomasse og frøvekt, og ser på år hver for seg
ggplot(Biomass, aes(x = Biomass, y = log(Seed_mass), color = Stage)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ Year)

#Model med random effects
ModelBiomass <- lme(log(Seed_mass) ~ Biomass*Stage, random = ~ 1 | BlockID, data = Biomass %>% filter(Year == 2016)) #ser på 2016 dataene alene (gjøre egen for 2017)
summary(ModelBiomass)




# Funket ikke helt, ser på senere
BiomassResult <- Biomass %>% 
  group_by(Year) %>% 
  do(fit = lme(log(Seed_mass) ~ Biomass*Stage, random = ~ 1 | BlockID, data = .))
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

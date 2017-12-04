source("RanunculusData.R")

##### PEAK FLOWER/ PEAK POLLINATORS ################################################
AllPred %>% 
  select(stage, siteID, peak.fl, peak.poll)

summary(lm(peak.fl~peak.poll, AllPred))
anova(lm(peak.fl~peak.poll, AllPred))

##### MISMATCH ~ DAY OF SNOWMELT ###################################################
snowmelt_mismatch <- AllPred %>% 
  select(stage, siteID, peak.diff) %>%
  left_join(Date_snowmelt, by=c("stage"="stage", "siteID"="siteID"))

fit1 <- lm(peak.diff ~ doy, data = snowmelt_mismatch)
fit2 <- lm(peak.diff ~ doy + I(doy^2), data = snowmelt_mismatch)
fit3 <- lm(peak.diff~doy + I(doy^2) + I(doy^3), data = snowmelt_mismatch)
AIC(fit1, fit2, fit3) # using model 1 from here

summary(fit1)
anova(fit1)

##### REPROD.OUTPUT ~ TREATMENT ####################################################

summary(lm(Seed_mass ~ Plant_type*Stage, Biomass17))
summary(lm(Seed_mass ~ Plant_type, Biomass17))


##### REPROD.OUTPUT ~ MISMATCH #####################################################
output_mismatch <- Biomass17 %>% 
  left_join(AllPred, by=c("Site"="siteID", "Stage"="stage")) %>% 
  filter(Plant_type == "C") %>%
  select(Stage, Site, Plant_type, Seed_mass, peak.diff) 

fitA <- lm(Seed_mass ~ peak.diff, output_mismatch)
fitB <- lm(Seed_mass ~ peak.diff + I(peak.diff^2), output_mismatch)
fitC <- lm(Seed_mass ~ peak.diff + I(peak.diff^2) + I(peak.diff^3), output_mismatch)

AIC(fitA, fitB, fitC)

summary(fitA)
anova(fitA)

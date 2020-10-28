##### REPROD.OUTPUT ~ MISMATCH #####################################################

# 2016
output_mismatch16 <- Biomass %>% 
  left_join(AllPred, by=c("Year"="year", "Stage"="stage", "Site"="site", "siteID" )) %>% 
  filter(Year == 2016, Treatment == "Control", Stage != "L")

fitA <- lm(Seed_mass ~ peak.diff, output_mismatch16)
fitB <- lm(Seed_mass ~ peak.diff + I(peak.diff^2), output_mismatch16)
fitC <- lm(Seed_mass ~ peak.diff + I(peak.diff^2) + I(peak.diff^3), output_mismatch16)

AIC(fitA, fitB, fitC)

summary(fitA)
anova(fitA)

# 2017
output_mismatch17 <- Biomass %>% 
  left_join(AllPred, by=c("Year"="year", "Stage"="stage", "Site"="site", "siteID")) %>% 
  filter(Year == 2017, Treatment == "Control")

modA <- lm(Seed_mass ~ peak.diff, output_mismatch17)
modB <- lm(Seed_mass ~ peak.diff + I(peak.diff^2), output_mismatch17)
modC <- lm(Seed_mass ~ peak.diff + I(peak.diff^2) + I(peak.diff^3), output_mismatch17)

AIC(modA, modB, modC)

summary(modA)
anova(modA)

##### REPROD.OUTPUT ~ OVERLAP #####################################################

# 2016


# 2017
overlap17 <- Biomass %>% 
  filter(Year == 2017) %>% 
  left_join(Overlap_data, by=c("Stage"="stage", "Site"="site", "siteID")) %>% 
  filter(Treatment == "Control")

mod1 <- lm(Seed_mass ~ overlap, overlap17)
mod2 <- lm(Seed_mass ~ overlap + I(overlap^2), overlap17)
mod3<- lm(Seed_mass ~ overlap + I(overlap^2) + I(overlap^3), overlap17)

AIC(mod1, mod2, mod3) # -> mod3 er lavest = -3490.203

summary(mod1)
anova(mod?)

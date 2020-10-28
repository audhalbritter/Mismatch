source("R/1_Import_RanunculusData.R")

##### PEAK FLOWER/ PEAK POLLINATORS ################################################
# 2016
peaks.16 <- AllPred %>% 
  filter(year == 2016)

summary(lm(peak.fl~peak.poll, peaks.16))
anova(lm(peak.fl~peak.poll, peaks.16))

# 2017
peaks.17 <- AllPred %>% 
  filter(year == 2017)

summary(lm(peak.fl~peak.poll, peaks.17))
summary(lm(peak.fl~peak.poll*stage, peaks.17))

##### MISMATCH ~ STAGE #############################################################
# 2016
stage_mismatch16 <- AllPred %>% 
  filter(year == 2016, stage != "L")
  
summary(lm(peak.diff ~ stage, stage_mismatch16))

# 2017
stage_mismatch17 <- AllPred %>% 
  filter(year == 2017) %>% 
  select(year, stage, siteID, peak.diff)

summary(lm(peak.diff ~ stage, stage_mismatch17))


##### REPROD.OUTPUT####################################################
# 2016
# Output of treatments
treat16 <- Biomass %>% 
  filter(Year==2016, Stage != "L")
summary(lm(Seed_mass ~ Treatment, treat16))

# Output of treatments and stage
treat16 <- Biomass %>% 
  filter(Year==2016, Stage != "L")
summary(lm(Seed_mass ~ Treatment*Stage, treat16))

# Output of controls per stage
contr16 <- Biomass %>% 
  filter(Year==2016, Stage != "L", Treatment == "Control")
summary(lm(Seed_mass ~ Stage, contr16))


# 2017
# Output of treatments
treat17 <- Biomass %>% 
  filter(Year == 2017)
summary(lm(Seed_mass ~ Treatment, treat17))

# Output of treatments and stage
treat17 <- Biomass %>% 
  filter(Year == 2017)
summary(lm(Seed_mass ~ Treatment*Stage, treat17))

# Output of controls per stage
contr17 <- Biomass %>% 
  filter(Year == 2017, Treatment=="Control")
summary(lm(Seed_mass ~ Stage, contr17))
anova(lm(Seed_mass ~ Stage, contr17))



# treatment*stage 2016


# treatment*stage 2017


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


##### POLLINATION VISITATION RATE ##################################################

# 2016
rate16 <- pollination2 %>% 
  filter(std.fly != "Inf", year.poll == 2016, stage != "L")

summary(lm(std.fly ~ doy, rate16))
summary(lm(std.fly ~ stage, rate16))
summary(lm(std.fly ~ doy + stage, rate16))

# 2017
rate17 <- pollination2 %>% 
  filter(std.fly != "Inf") %>% 
    filter(year.poll == 2017)

summary(lm(std.fly ~ doy, rate17))
summary(lm(std.fly ~ stage, rate17))
summary(lm(std.fly ~ doy + stage, rate17))


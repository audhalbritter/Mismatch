source("RanunculusData.R")

##### PEAK FLOWER/ PEAK POLLINATORS ################################################
summary(lm(peak.fl~peak.poll, AllPred))
anova(lm(peak.fl~peak.poll, AllPred))


##### MISMATCH ~ STAGE #############################################################
# 2016
stage_mismatch16 <- AllPred %>% 
  filter(year == 2016) %>% 
  select(year, stage, siteID, peak.diff)
  
summary(lm(peak.diff ~ stage, stage_mismatch16))

# 2017
stage_mismatch17 <- AllPred %>% 
  filter(year == 2017) %>% 
  select(year, stage, siteID, peak.diff)

summary(lm(peak.diff ~ stage, stage_mismatch17))

##### MISMATCH ~ DAY OF SNOWMELT ###################################################

# 2017
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
#2016
summary(lm(Seed_mass ~ Treatment*Stage, biomass16))
summary(lm(Seed_mass ~ Treatment, biomass16))

#2017
summary(lm(Seed_mass ~ Treatment*Stage, Biomass17))
summary(lm(Seed_mass ~ Treatment, Biomass17))


##### REPROD.OUTPUT ~ MISMATCH/OVERLAP#####################################################
# Mismatch, 2016 #Problem with 2016 Biomass data
output_mismatch16 <- Biomass %>% 
  left_join(AllPred, by=c("Stage"="stage", "Year"="year")) %>% 
  filter(Year == 2016, Treatment == "Control") %>%
  select(Stage, Treatment, Seed_mass, peak.diff) 

fitA <- lm(Seed_mass ~ peak.diff, output_mismatch16)
fitB <- lm(Seed_mass ~ peak.diff + I(peak.diff^2), output_mismatch16)
fitC <- lm(Seed_mass ~ peak.diff + I(peak.diff^2) + I(peak.diff^3), output_mismatch16)

AIC(fitA, fitB, fitC)

summary(fitA)
anova(fitA)

# Mismatch, 2017
output_mismatch17 <- Biomass %>% 
  left_join(AllPred, by=c("Stage"="stage", "Year"="year")) %>% 
  filter(Year == 2017, Treatment == "Control") %>%
  select(Stage, Treatment, Seed_mass, peak.diff) 

modA <- lm(Seed_mass ~ peak.diff, output_mismatch17)
modB <- lm(Seed_mass ~ peak.diff + I(peak.diff^2), output_mismatch17)
modC <- lm(Seed_mass ~ peak.diff + I(peak.diff^2) + I(peak.diff^3), output_mismatch17)

AIC(modA, modB, modC)

summary(modA)
anova(modA)


# Days of overlap, 2016


# Days of overlap, 2017
overlap17 <- Biomass %>% 
  filter(Year == 2017) %>% 
  left_join(Overlap_data, by=c("Stage"="stage", "siteID")) %>% 
  mutate(Stage = as.character(Stage), siteID = as.character(siteID))

mod1 <- lm(Seed_mass ~ overlap, overlap17)
mod2 <- lm(Seed_mass ~ overlap + I(overlap^2), overlap17)
mod3<- lm(Seed_mass ~ overlap + I(overlap^2) + I(overlap^3), overlap17)

AIC(mod1, mod2, mod3) # -> mod3 er lavest = -3490.203

summary(mod?)
anova(mod?)

##### REPROD.OUTPUT ~ SNOWMELT #####################################################
summary(lm(Seed_mass ~ ))

##### POLLINATION VISITATION RATE ##################################################
vis.rate <- pollination2 %>% 
  filter(std.fly != "Inf") %>% 
  mutate(siteID = as.factor(paste(stage, site, sep = " "))) %>%
  filter(year.poll == 2017) %>%
  left_join(Date_snowmelt, by = c("siteID", "stage")) %>%
  ungroup() %>%
  mutate(stage = factor(stage), doy = yday(date))

summary(lm(std.fly ~ doy, vis.rate))
summary(lm(std.fly ~ doy + Snowmelt_date, vis.rate))

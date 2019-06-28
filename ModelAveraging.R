source("RanunculusData.R")

library("MuMIn")
library("lme4")

#  change the default "na.omit" to prevent models from being fitted to different datasets in case of missing values.
options(na.action = "na.fail") # can also be put in the model
options(na.action = "na.omit") # change back


# 2016 data
# Seed potential
dat2016 <- WeatherAndBiomass %>% filter(Year == 2016,
                          MeanVisit != Inf) %>% 
  droplevels() %>% 
  mutate(Biomass.cen = scale(Biomass, scale = FALSE))

d1 <- as_tibble(x = scale(dat2016$CumTemp))
d2 <- as_tibble(x = scale(dat2016$CumPrec))
d3 <- as_tibble(x = scale(dat2016$MeanFlowers))
d4 <- as_tibble(x = scale(dat2016$MeanVisit))

dat2016 <- dat2016 %>% 
  select(-CumTemp.cen) %>% 
  bind_cols(d1, d2, d3, d4) %>% 
  rename(CumTemp.cen = V1, CumPrec.cen = V11, MeanFlower.cen = V12, MeanVisit.cen = V13)

ModelSeedPotential2016 <- glmer(Seed_potential ~ Biomass + Stage + Treatment + MeanVisit.cen + MeanFlower.cen + CumTemp.cen + (1 | BlockID) + offset(log(Tot_Ovule)), family = "binomial", data = dat2016, weights = Tot_Ovule) 

plot(ModelSeedPotential2016)

model.set <- dredge(ModelSeedPotential2016, rank = "AICc", extra = "R^2")
mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm95 <- mm %>% filter(cumsum < 0.95)
averaged.model <- model.avg(model.set, cumsum(weight) <= 0.95)
res <- data.frame(summary(averaged.model)$coefmat.full)


# Seed mass
ModelSeedMass2016 <- lm(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanVisit + MeanFlower.cen + CumTemp.cen, data = dat2016)

plot(ModelSeedMass2016)
model.set <- dredge(ModelSeedMass2016, rank = "AICc", extra = "R^2")
mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm95 <- mm %>% filter(cumsum < 0.95)
averaged.model <- model.avg(model.set, cumsum(weight) <= 0.95)
res <- data.frame(summary(averaged.model)$coefmat.full)
summary(ModelSeedMass2016)


# 2017 data
dat2017 <- WeatherAndBiomass %>% filter(Year == 2017,
                                        MeanVisit != Inf) %>% 
  droplevels() %>% 
  mutate(MeanVisit.cen = scale(MeanVisit, scale = FALSE),
         Biomass.cen = scale(Biomass, scale = FALSE))

d1 <- as_tibble(x = scale(dat2017$CumTemp))
d2 <- as_tibble(x = scale(dat2017$CumPrec))
d3 <- as_tibble(x = scale(dat2017$MeanFlowers))

dat2017 <- dat2017 %>% 
  select(-CumTemp.cen) %>% 
  bind_cols(d1, d2, d3) %>% 
  rename(CumTemp.cen = V1, CumPrec.cen = V11, MeanFlower.cen = V12)

ModelSeedMass2017 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanVisit + MeanFlower.cen + CumTemp.cen + (1| BlockID), data = dat2017, REML = FALSE)

plot(ModelSeedMass2017)

model.set <- dredge(ModelSeedMass2017, rank = "AICc", extra = "R^2")
mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm95 <- mm %>% filter(cumsum < 0.95)
averaged.model <- model.avg(model.set, cumsum(weight) <= 0.95)
res <- data.frame(summary(averaged.model)$coefmat.full)


WeatherAndBiomass %>% 
  select(Year, BlockID, Seed_potential, Biomass, Stage, Treatment, MeanVisit, MeanFlowers, CumTemp) %>% 
  filter(Year == "2016") %>% 
  gather(key = Variable, value = Value, -Year, -Seed_potential, -Stage, -BlockID, -Treatment) %>% 
  ggplot(aes(y = Seed_potential, x = Value, color = Stage, shape = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = Treatment, color = Stage)) +
  scale_shape_manual(values = c(1, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  facet_grid(~ Variable, scales = "free")

WeatherAndBiomass %>% 
  select(Year, BlockID, Seed_mass, Biomass, Stage, Treatment, MeanVisit, MeanFlowers, CumTemp) %>% 
  gather(key = Variable, value = Value, -Year, -Seed_mass, -Stage, -BlockID, -Treatment) %>% 
  ggplot(aes(y = Seed_mass, x = Value, color = Stage, shape = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = Treatment, color = Stage)) +
  scale_shape_manual(values = c(1, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  facet_grid(Year ~ Variable, scales = "free")

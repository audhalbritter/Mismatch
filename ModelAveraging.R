source("RanunculusData.R")

library("MuMIn")
library("lme4")

# Function to check model assumptions
fix.check <- function(mod){		#function to produce model-checking plots for the fixed effects of an lmer model
  par(mfrow = c(2,2))
  plot(fitted(mod),resid(mod))	#should have no pattern
  abline(h=0)
  print(anova(lm(fitted(mod)~resid(mod))))	#should be non-significant
  qqnorm(resid(mod), ylab="Residuals")		#should be approximately straight line
  qqline(resid(mod))
  plot(density(resid(mod)))					#should be roughly normally distributed
  rug(resid(mod))}


#  change the default "na.omit" to prevent models from being fitted to different datasets in case of missing values.
options(na.action = "na.fail") # can also be put in the model
#options(na.action = "na.omit") # change back


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
  #select(-CumTemp.cen) %>% 
  bind_cols(d1, d2, d3, d4) %>% 
  rename(CumTemp.cen = V1, CumPrec.cen = V11, MeanFlower.cen = V12, MeanVisit.cen = V13)

# Define full model with all variables
ModelSeedPotential2016 <- glmer(Seed_potential ~ Biomass + Stage + Treatment + MeanVisit.cen + MeanFlower.cen + CumTemp.cen + (1 | BlockID), family = "binomial", data = dat2016) #removed weights = Tot_Ovule because we have a binomial distribution 


# check model assumptions
# This is a function that produces a few plots to check if the model is fine. First plots is fitted values against residuals. The points should be distributed nicely on both sides of the line. Second plot is a QQ plot and points should be on a line. Last plot should show a normal(-ish) distribution. Stats should not be significant.
fix.check(ModelSeedPotential2016)

# The "drege" function runs all possible combinations of the full model. And produces a table wit R^2, AIC and wieghts. Intercept and offset is kept in all the models (= fixed).
model.set <- dredge(ModelSeedPotential2016, rank = "AICc", extra = "R^2")
#removed fixed = offset(log(Tot_Ovule))
# R squares are high (c. 0.8), this is good. It means the models are describing the data very well.

mm <- data.frame(model.set) # making a data frame
mm$cumsum <- cumsum(mm$weight) # calculate the cumulative sum of the weights of all models
mm
# Look at the cumsum. Many models are needed to sum up to 0.95. This means that all these models are important and we will keep c. 20 models for the next step. So we are not selecting one best model, but a bunch of models which are good.

# select 95% confident set of models
mm95 <- mm %>% filter(cumsum < 0.95)
mm95
# Now you can see there are 20 models kept.

# The "model.avg" function does a model averaging based on AIC values
averaged.model <- model.avg(model.set, cumsum(weight) <= 0.95)
averaged.model
# Now the different variables have been weighed and you can see the "weighed coefficients". There are 2 different ways this can be calculated. We will use the full method. It is described in teh Gruber et al. 2011 paper. But not so important to understand it.

# getting results. This table is what you can present in your results section. In my paper (Halbritter et al 2018) I plotted these variables (see Fig 3). See Table S4 and S6 for how I reported that results in table.
res <- data.frame(summary(averaged.model)$coefmat.full) 

res1 <- res %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "CumTemp.cen", "MeanVisit.cen", "Biomass", "MeanFlower.cen", "StageM", "StageL", "TreatmentPollinated"), c("Stage E", "Cumulative temperature", "Mean visitation rate", "Biomass", "Phenology", "Stage M", "Stage L", "Treatment: hand pollinated"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
   mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI, Zvalue, Pvalue)
res1




#Try to make a plot like the one Aud has in her paper, and also divide it into stages, not slope and absolute slope?

#Prøvd å lage ggplot med errorbars, men får det ikke til med CI (confidence interval), kun med SE
ggplot(res1, aes(x = Variable, y = Estimate)) + 
  geom_errorbar(aes(ymin = Estimate-CI, ymax = Estimate+CI, width = .1)) + #errorbar fungerer ikke ved å legge inn CI  
  geom_point() +
  geom_line() 
  


### Now you can try for Seed mass :-)

# Seed mass (done with lm, check if it can be done with lmer if we change treatment?)
#uses the same code from line 25-33.

#Define the model
ModelSeedMass2016 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanVisit + MeanFlower.cen + CumTemp.cen + (1|BlockID), data = dat2016, REML = FALSE)

#Plot
fix.check(ModelSeedMass2016)
plot(ModelSeedMass2016)

#Apply dredge function
model.set <- dredge(ModelSeedMass2016, rank = "AICc", extra = "R^2")

#Make a new dataframe
mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm

#Sekect the 95% confidence interval
mm95 <- mm %>% filter(cumsum < 0.95)
mm95 

#Model averaging based on AIC values
averaged.model <- model.avg(model.set, cumsum(weight) <= 0.95)
averaged.model

#Getting the table to present
res <- data.frame(summary(averaged.model)$coefmat.full)
res

summary(ModelSeedMass2016)

res2 <- res %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "CumTemp.cen", "MeanVisit.cen", "Biomass", "MeanFlower.cen", "StageM", "StageL", "TreatmentPollinated"), c("Stage E", "Cumulative temperature", "Mean visitation rate", "Biomass", "Phenology", "Stage M", "Stage L", "Treatment: hand pollinated"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
  mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI, Zvalue, Pvalue)
res2



# 2017 data
dat2017 <- WeatherAndBiomass %>% filter(Year == 2017,
                                        MeanVisit != Inf) %>% 
  droplevels() %>% 
  mutate(MeanVisit.cen = scale(MeanVisit, scale = FALSE),
         Biomass.cen = scale(Biomass, scale = FALSE))

d1 <- as_tibble(x = scale(dat2017$CumTemp))
d2 <- as_tibble(x = scale(dat2017$CumPrec))
d3 <- as_tibble(x = scale(dat2017$MeanFlowers))
d4 <- as_tibble(x = scale(dat2017$MeanFlowers)) #added this, since its added above, correct?

dat2017 <- dat2017 %>% 
  #select(-CumTemp.cen) %>% 
  bind_cols(d1, d2, d3, d4) %>%  #also added d4 here, see comment above
  rename(CumTemp.cen = V1, CumPrec.cen = V11, MeanFlower.cen = V12)

#Model
ModelSeedMass2017 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanVisit + MeanFlower.cen + CumTemp.cen + (1| BlockID), data = dat2017, REML = FALSE)

#Check the different plots (only get one type of plot here, correct?)
fix.check(ModelSeedMass2017)
plot(ModelSeedMass2017)

#Dredge function
model.set <- dredge(ModelSeedMass2017, rank = "AICc", extra = "R^2")

#Making a dataframe
mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm

#Choosing the 95 % confidence set of model
mm95 <- mm %>% filter(cumsum < 0.95)
mm95
#Results: biomass + temp + mean flowers in the top model

#Model averaging
averaged.model <- model.avg(model.set, cumsum(weight) <= 0.95)
averaged.model

#Results to present
res <- data.frame(summary(averaged.model)$coefmat.full)
res

res3 <- res %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "CumTemp.cen", "MeanVisit.cen", "Biomass", "MeanFlower.cen", "StageM", "StageL", "TreatmentPollinated"), c("Stage E", "Cumulative temperature", "Mean visitation rate", "Biomass", "Phenology", "Stage M", "Stage L", "Treatment: hand pollinated"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
  mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI, Zvalue, Pvalue)
res3

# Plots
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

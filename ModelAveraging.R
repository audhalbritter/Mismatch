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
ModelSeedPotential2016 <- glmer(Seed_potential ~ Biomass + Stage + Treatment + MeanFlower.cen + CumTemp.cen + CumPrec.cen + (1 | BlockID), family = "binomial", data = dat2016) #removed weights = Tot_Ovule because we have a binomial distribution 


# check model assumptions
# This is a function that produces a few plots to check if the model is fine. First plots is fitted values against residuals. The points should be distributed nicely on both sides of the line. Second plot is a QQ plot and points should be on a line. Last plot should show a normal(-ish) distribution. Stats should not be significant.
fix.check(ModelSeedPotential2016)

# The "drege" function runs all possible combinations of the full model. And produces a table wit R^2, AIC and wieghts. Intercept and offset is kept in all the models (= fixed).
model.setSP <- dredge(ModelSeedPotential2016, rank = "AICc", extra = "R^2")
#removed fixed = offset(log(Tot_Ovule))
# R squares are high (c. 0.8), this is good. It means the models are describing the data very well.

mmSP <- data.frame(model.setSP) # making a data frame
mmSP$cumsum <- cumsum(mmSP$weight) # calculate the cumulative sum of the weights of all models
mmSP
# Look at the cumsum. Many models are needed to sum up to 0.95. This means that all these models are important and we will keep c. 20 models for the next step. So we are not selecting one best model, but a bunch of models which are good.

# select 95% confident set of models
mmSP95 <- mmSP %>% filter(cumsum < 0.95)
mmSP95
# Now you can see there are 20 models kept. New comment: now there are 38 models, with precipitation included and mean visit excluded in the model, and offset removed


# The "model.avg" function does a model averaging based on AIC values
averaged.modelSP <- model.avg(model.setSP, cumsum(weight) <= 0.95)
averaged.modelSP
# Now the different variables have been weighed and you can see the "weighed coefficients". There are 2 different ways this can be calculated. We will use the full method. It is described in teh Gruber et al. 2011 paper. But not so important to understand it.

# getting results. This table is what you can present in your results section. In my paper (Halbritter et al 2018) I plotted these variables (see Fig 3). See Table S4 and S6 for how I reported that results in table.
resSP <- data.frame(summary(averaged.modelSP)$coefmat.full) 
resSP

resSP1 <- resSP %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Category = plyr::mapvalues(Category, c("Intercept", "CumTemp.cen", "Biomass", "MeanFlower.cen", "CumPrec.cen", "StageM", "StageL", "TreatmentPollinated"), c("Stage: early and Treatment: control", "Cumulative temperature", "Biomass", "Phenology", "Cumulative precipitation", "Stage: mid", "Stage: late", "Treatment: hand pollinated"))) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "CumTemp.cen", "Biomass", "MeanFlower.cen", "CumPrec.cen", "StageM", "StageL", "TreatmentPollinated"), c("Stage: early and Treatment: control", "Cumulative temperature", "Biomass", "Phenology", "Cumulative precipitation", "Stage: mid", "Stage: late", "Treatment: hand pollinated"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
   #mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI.low, CI.high, Zvalue, Pvalue)
resSP1

#nothing is significant



#Plot, ikke endret navn på intercept da jeg ikke vet om det er stage E eller treatment: control
Seedpotential2016 <- ggplot(resSP1, aes(x = Estimate, y = Variable)) + 
  geom_errorbarh(aes(xmin = CI.low, xmax = CI.high, height = .0)) +   
  geom_point() +
  geom_line() +
  ggtitle("Seed potential 2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Parameter estimates", y="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey")
ggsave(Seedpotential2016, filename = "Figurer/Seedpotential2016.jpeg", height = 6, width = 8)
  


### Now you can try for Seed mass :-)

# Seed mass (done with lm, check if it can be done with lmer if we change treatment?)
#uses the same code from line 25-33.

#Define the model
ModelSeedMass2016 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanFlower.cen + CumTemp.cen + CumPrec.cen + (1|BlockID), data = dat2016, REML = FALSE)

#Plot
fix.check(ModelSeedMass2016)
plot(ModelSeedMass2016)

#Apply dredge function
model.setSM16 <- dredge(ModelSeedMass2016, rank = "AICc", extra = "R^2")

#Make a new dataframe
mmSM16 <- data.frame(model.setSM16)
mmSM16$cumsum <- cumsum(mmSM16$weight)
mmSM16

#Sekect the 95% confidence interval
mmSM1695 <- mmSM16 %>% filter(cumsum < 0.95)
mmSM1695 
#Gives us 17 models

#Model averaging based on AIC values
averaged.modelSM16 <- model.avg(model.setSM16, cumsum(weight) <= 0.95)
averaged.modelSM16

#Getting the table to present
resSM16 <- data.frame(summary(averaged.modelSM16)$coefmat.full)
resSM16

summary(ModelSeedMass2016)

resSM162 <- resSM16 %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Category = plyr::mapvalues(Category, c("Intercept", "Biomass", "StageM", "StageL", "CumTemp.cen", "TreatmentPollinated", "MeanFlower.cen", "CumPrec.cen"), c("Stage E", "Biomass", "Stage: mid", "Stage: late", "Cumulative temperature", "Treatment: hand pollinated", "Phenology", "Cumulative precipitation"))) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "Biomass", "StageM", "StageL", "CumTemp.cen", "TreatmentPollinated", "MeanFlower.cen", "CumPrec.cen"), c("Stage E", "Biomass", "Stage: mid", "Stage: late", "Cumulative temperature", "Treatment: hand pollinated", "Phenology", "Cumulative precipitation"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
  #mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI.low, CI.high, Zvalue, Pvalue)
resSM162

#biomass and intercept is significant

#Plot, har ikke endret navn på intercept da jeg ikke vet om det er stage E eller treatment: control
Seedmass2016 <- ggplot(resSM162, aes(x = Estimate, y = Variable)) + 
  geom_errorbarh(aes(xmin = CI.low, xmax = CI.high, height = .0)) +   
  geom_point() +
  geom_line() +
  ggtitle("Seed mass 2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Parameter estimates", y="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey")
ggsave(Seedmass2016, filename = "Figurer/Seedmass2016.jpeg", height = 6, width = 8)



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
ModelSeedMass2017 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanFlower.cen + CumTemp.cen + CumPrec.cen + (1| BlockID), data = dat2017, REML = FALSE)

#Check the different plots (only get one type of plot here, correct?)
fix.check(ModelSeedMass2017)
plot(ModelSeedMass2017)

#Dredge function
model.setSM17 <- dredge(ModelSeedMass2017, rank = "AICc", extra = "R^2")

#Making a dataframe
mmSM17 <- data.frame(model.setSM17)
mmSM17$cumsum <- cumsum(mmSM17$weight)
mmSM17

#Choosing the 95 % confidence set of model
mmSM1795 <- mmSM17 %>% filter(cumsum < 0.95)
mmSM1795
#Gives us 9 models

#Model averaging
averaged.modelSM17 <- model.avg(model.setSM17, cumsum(weight) <= 0.95)
averaged.modelSM17

#Results to present
resSM17 <- data.frame(summary(averaged.modelSM17)$coefmat.full)
resSM17

resSM173 <- resSM17 %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Category = plyr::mapvalues(Category, c("Intercept", "Biomass", "CumTemp.cen", "MeanFlower.cen", "StageE", "StageM", "TreatmentPollinated", "CumPrec.cen"), c("Stage E", "Biomass", "Cumulative temperature", "Phenology", "Stage: mid", "Stage: late", "Treatment: hand pollinated", "Cumulative precipitation"))) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "Biomass", "CumTemp.cen", "MeanFlower.cen", "StageE", "StageM", "TreatmentPollinated", "CumPrec.cen"), c("Stage E", "Biomass", "Cumulative temperature", "Phenology", "Stage: mid", "Stage: late", "Treatment: hand pollinated", "Cumulative precipitation"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
  #mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI.low, CI.high, Zvalue, Pvalue)
resSM173

#biomass, intercept and phenology is significant

#Plot, ikke endret navn på intercept da jeg ikke vet om det er stage E eller treatment: control

Seedmass2017 <- ggplot(resSM173, aes(x = Estimate, y = Variable)) + 
  geom_errorbarh(aes(xmin = CI.low, xmax = CI.high, height = .0)) +   
  geom_point() +
  geom_line() +
  ggtitle("Seed mass 2017") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Parameter estimates", y="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey")
ggsave(Seedmass2017, filename = "Figurer/Seedmass2017.jpeg", height = 6, width = 8)

###############################
#Model with interaction????
#Define the model
Model2SeedMass2016 <- lmer(log(Seed_mass) ~ Biomass * Stage + Treatment + MeanFlower.cen + CumTemp.cen + CumPrec.cen + (1|BlockID), data = dat2016, REML = FALSE)

#Plot
fix.check(Model2SeedMass2016)
plot(Model2SeedMass2016)

#Apply dredge function
model2.setSM16 <- dredge(Model2SeedMass2016, rank = "AICc", extra = "R^2")

#Make a new dataframe
mm2SM16 <- data.frame(model2.setSM16)
mm2SM16$cumsum <- cumsum(mm2SM16$weight)
mm2SM16

#Sekect the 95% confidence interval
mm2SM1695 <- mm2SM16 %>% filter(cumsum < 0.95)
mm2SM1695 
#Gives us 28 models

#Model averaging based on AIC values
averaged.model2SM16 <- model.avg(model2.setSM16, cumsum(weight) <= 0.95)
averaged.model2SM16

#Getting the table to present
res2SM16 <- data.frame(summary(averaged.model2SM16)$coefmat.full)
res2SM16

summary(ModelSeedMass2016)






# Plots
WeatherAndBiomass %>% 
  select(Year, BlockID, Seed_potential, Biomass, Stage, Treatment, MeanVisit, MeanFlowers, CumTemp, CumPrec) %>% #added cumprec
  filter(Year == "2016") %>% 
  gather(key = Variable, value = Value, -Year, -Seed_potential, -Stage, -BlockID, -Treatment) %>% 
  ggplot(aes(y = Seed_potential, x = Value, color = Stage, shape = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = Treatment, color = Stage)) +
  scale_shape_manual(values = c(1, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  facet_grid(~ Variable, scales = "free")

WeatherAndBiomass %>% 
  select(Year, BlockID, Seed_mass, Biomass, Stage, Treatment, MeanVisit, MeanFlowers, CumTemp, CumPrec) %>% #added cumprec
  gather(key = Variable, value = Value, -Year, -Seed_mass, -Stage, -BlockID, -Treatment) %>% 
  ggplot(aes(y = Seed_mass, x = Value, color = Stage, shape = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = Treatment, color = Stage)) +
  scale_shape_manual(values = c(1, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  facet_grid(Year ~ Variable, scales = "free")


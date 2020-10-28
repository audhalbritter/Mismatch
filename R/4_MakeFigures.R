##### DATA ANALYSIS CODE ######
# This is the code to analyse the data for Silje Andrea Hjortland Östman's master thesis (University of Bergen), 2018, Plant-pollinator interactions in the alpine: Landscape heterogeneity acts as a potential buffer against climate-change induced mismatch in the pollinator-generalist Ranunculus acris

# This is also the code to analyse the data for the plant-pollinator interaction part from Roos et al. 20XX.

# import data and calculate day of peak flowering and pollinaton
source("R/1_Import_RanunculusData.R")
source("R/2_PeakPredicted.R")

### LIBRARIES
library("nlme")


##### PEAK FLOWER/ PEAK POLLINATORS ################################################

peaks <- AllPred %>% 
  filter(stage != "L") %>% 
  rename(gradient = site) %>% 
  group_by(year) %>% 
  do(fit = lm(peak.poll ~ peak.fl, data = .),
     fit2 = lm(peak.poll ~ peak.fl * stage, data = .))

tidy(peaks, fit)
tidy(peaks, fit2)

summary(lm(peak.fl~peak.poll, peaks.16))
anova(lm(peak.fl~peak.poll, peaks.16))


peaks.17 <- AllPred %>% 
  filter(year == 2017)

summary(lm(peak.fl~peak.poll, peaks.17))
summary(lm(peak.fl~peak.poll*stage, peaks.17))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


Peak_Flower_Poll_Figure <- AllPred %>%
  filter(stage != "L") %>% 
  ggplot(aes(x = peak.fl, y = peak.poll)) +
  labs(y = "Day of peak pollinator visitation", x = "Day of peak flowering", color = "Snowmelt stage") +
  geom_smooth(method = lm, se = FALSE, size = 1, colour = "grey30") +
  geom_point(aes(colour = factor(stage), shape = factor(stage)), size = 2) +
  scale_color_manual(labels = c ("early","mid", "late"), values = cbbPalette[c(3,7,4)], name = "snowmelt stage") +
  scale_shape_manual(labels = c ("early","mid", "late"), values = c(15,16,17), name = "snowmelt stage") +
  geom_abline(slope = 1, color = "grey80", linetype = "dashed") +
  theme_light(base_size = 16) +
  facet_wrap(~year) +
  theme(legend.position = "bottom", 
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12))

ggsave(Peak_Flower_Poll_Figure, filename = "Peak_Flower_Poll_Figure.jpeg", dpi = 300)


##### POLLINATION VISITATION RATE ##################################################

# 2016
dfVisitRate <- pollination2 %>% 
  ungroup() %>% 
  filter(std.fly != "Inf", stage != "L") %>% 
  mutate(stage = recode(stage, "F"="Early", "E" = "Mid", "M" = "Late")) %>%
  rename(year = year.poll, gradient = site) %>% 
  group_by(year) %>% 
  do(fit = lm(std.fly ~ doy + I(doy^2), data = .),
     fit2 = lme(std.fly ~ doy, random =~ 1|gradient, data = .))

tidy(dfVisitRate, fit)
tidy(dfVisitRate, fit2, effects = "fixed")


pollination2 %>% 
  ungroup() %>% 
  filter(std.fly != "Inf", stage != "L") %>% 
  mutate(stage = recode(stage, "F"="Early", "E" = "Mid", "M" = "Late")) %>% 
  select(year.poll, stage, std.fly, doy) %>% 
  ggplot(aes(x = doy, y = std.fly)) +
  geom_point(size = 1.5) +
  geom_smooth(method = lm, formula = "y ~ x + I(x^2)", se = FALSE, color = "black") +
  labs(y = "Visitation rate", color="", x="Day of the year") +
  facet_grid(stage ~ year.poll, scales = "free_y") +
  theme_light(base_size = 18)



#### REPRODUCTIVE OUTPUT ##########################################################

## Reproductive output by treatment
Biomass %>% 
  filter(Stage != "L") %>%
  ggplot(aes(y=Seed_mass, x=Treatment, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(Year~Stage, labeller = labeller(Stage = as_labeller(c("F" = "early", "E" = "mid", "M" = "late")))) +
  theme_light(base_size = 16) +
  labs(y="Reproductive output", x="", fill="") +
  scale_fill_manual(values=cbbPalette[c(7,3)])
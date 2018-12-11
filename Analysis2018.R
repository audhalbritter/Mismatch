source("RanunculusData.R")

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


AllPred %>%
  filter(stage != "L") %>% 
  ggplot(aes(x = peak.fl, y = peak.poll, colour = stage)) +
  labs(y = "Peak pollinator visitation (day of the year)", x = "Peak flowering (day of the year)", color = "Snowmelt stage") +
  geom_smooth(method = lm, se = FALSE, size = 1) +
  geom_point(aes(colour = factor(stage), shape = factor(stage)), size = 2) +
  scale_color_manual(labels = c ("early","mid", "late"), values=cbbPalette[c(7,3,5)], name = "snowmelt stage") +
  scale_shape_manual(labels = c ("early","mid", "late"), values = c(15,16,17), name = "snowmelt stage") +
  # geom_abline(slope = 1, color = "black", size = 20, alpha = 0.1) +
  geom_abline(slope = 1, color = "black", linetype = "dashed") +
  theme_light(base_size = 16) +
  facet_wrap(~year) +
  theme(legend.position = "bottom", legend.title=element_text(size=12), legend.text=element_text(size=12))



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


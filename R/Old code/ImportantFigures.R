#### Important Figures

#### Those figures made for the first report for Yan,VV
# Figures Presentation

## ----loadPhenology
#load libraries
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")


# load data
load("PhenologyPollination.RData")
load("Phenology.RData")
load("Pollinator.RData")
load("TemperatureiButton.RData")
load("Pollen.RData")

# set the theme
th <- theme()

##############################################################################
####  FIGURES
##############################################################################

## ----PhenologyAllStages
pheno2 %>%
  group_by(stage, date, site) %>%
  summarise(n = n(), nrflower = sum(flowering)) %>% 
  #filter(stage != "L") %>% 
  ggplot() +
  labs(x = "", y = "Sum of flowers") +
  geom_line(aes(x = date, y = nrflower, color = stage)) +
  facet_wrap(~ site)


## ----PollinationAllStages
pollinator %>%
  mutate(day = ymd(day)) %>%
  group_by(stage, day, site) %>%
  summarise(n = n(), nrvisit = mean(fly)) %>% 
  #filter(stage != "L") %>% 
  ggplot() +
  labs(x = "", y = "Mean number of visits per day") +
  geom_point(aes(x = day, y = nrvisit, color = stage)) +
  scale_x_date() +
  facet_wrap(~ site)

## ----PollinationSummary
pollinator %>% 
  group_by(stage, day) %>% 
  summarise(n = n(), meanvisit = mean(fly), se = sd(fly)/sqrt(n)) %>% 
  mutate(day = ymd(day)) %>% 
  ggplot(aes(x = day, y = meanvisit, ymin = meanvisit - se, ymax = meanvisit + se)) + 
  geom_point() + 
  labs(x = "", y = "Mean number of visits per day") +
  geom_errorbar() +
  scale_x_date() +
  facet_wrap(~ stage)


## ----MismatchPlot
dat %>%
  filter(stage != "L") %>% 
  group_by(site, stage) %>% 
  mutate(peak.flower = day[which.max(nrflower)], peak.fly = day[which.max(nrvisit)]) %>% 
  mutate(diff = yday(peak.fly) - yday(peak.flower)) %>% 
  ggplot() +
  geom_point(aes(x = stage, y = diff, colour = stage)) +
  geom_hline(yintercept = 0, color = "grey") +
  labs(x = "", y = "Peak flower - peak visit in days") +
  facet_wrap(~ site)


## ----TemperatureData
Temperature %>% 
  filter(!is.na(temperature)) %>%
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(date, stage) %>%
  summarise(n = n(), value = mean(temperature)) %>%
  filter(n > 140) %>%
  select(-n) %>% 
  ggplot(aes(x = date, y = value, colour = stage)) +
  labs(x = "", y = "Temperature in °C") +
  geom_line()


## ----NrRipeSeed
pollen %>% 
  fill(Plot) %>% 
  mutate(stage = factor(substring(Plot, 1,1))) %>% 
  mutate(site = factor(substring(Plot, 2,3))) %>% 
  mutate(plot = factor(substring(Plot, 4,4))) %>% 
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>%
  mutate(plant = ifelse(Plant %in% c("C1", "C2"), "Control", "Pollinated")) %>% 
  select(-Plot, -Plant) %>% 
  group_by(plant, site, stage) %>% 
  ggplot(aes(x = plant, y = NumberOvule, fill = plant)) +
  geom_boxplot() +
  labs(x = "", y = "Number of ripe seeds") +
  facet_wrap(~ stage)

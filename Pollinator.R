### LOAD POLLINATOR DATA

#### LIBRARIES
install.packages("tidyr"); install.packages("dplyr"); install.packages("lubridate"); install.packages("ggplot2")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
### LOAD TEMPERATURE DATA
load("TemperatureiButton.RData", verbose = TRUE)
Temperature <- Temperature %>%
  mutate(stage = plyr::mapvalues(stage, c("early", "mid", "late"), c("E", "M", "L"))) %>%
  mutate(date = update(date, seconds = 0)) %>%
  mutate(minutes = (floor(minute(date)/10)*10)) %>%
  mutate(date = ymd_hm(paste0(format(date, "%Y-%m-%d %H:"), minutes)))



# Mean Temperature
Temperature %>% 
  group_by(stage) %>% 
  summarize(mean = mean(temperature))


# POLLINATOR OBSERVATIONS
setwd("C:/Users/Signe/Dropbox/Mismatch/Data")
polli <- read.csv("RanunculusPollinator.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)
str(polli)
head(polli)
pollinator <- polli %>%
  filter(!Tid == "") %>% # slette alle koloner med Na
  mutate(date = dmy_hm(paste(Dato, Tid))) %>%# lime sammen dato å tid
  mutate(fly = as.numeric(Fluer)) %>%
  mutate(stage = substring(Site, 1,1), site = substring(Site, 2,3)) %>%# lage to nye variabler, stage å site
  select(-Tid, -Fluer, -Site) %>% # sletter her koloner ikke rekker, - betyr ta vekk
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>%  # bestemme rekkefölgen for en faktor
  mutate(minutes = (floor(minute(date)/10)*10)) %>%
  mutate(date = ymd_hm(paste0(format(date, "%Y-%m-%d %H:"), minutes))) %>%
  left_join(Temperature, by = c("date" = "date", "stage" = "stage", "site" = "site"))
head(pollinator)


# Calculate and plot mean nr of visits per site
pol <- pollinator %>%
  filter(stage == "E") %>%
  filter(site == "07") %>%
  group_by(stage, date, site) %>%
  #summarise(n = n(), nrvisit = mean(fly)) %>%
  ggplot() +
  geom_point(aes(x = date, y = fly, color = site)) +
  facet_wrap(~site)


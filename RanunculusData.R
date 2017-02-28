##### PHENOLOGY ######

#### LIBRARIES
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")

#### READ IN DATA ####


# PHENOLOGY
pheno <- read.csv("RANfenologi.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
pheno <- as.data.frame(t(pheno), stringsAsFactors = FALSE) # transpose data
names(pheno) <- pheno[1,] # first column = name
pheno <- pheno[-1,] # remove first column
pheno$date <- dmy_hm(paste(pheno$Dato, pheno$Tid))
pheno$Dato <- NULL
pheno$Tid <- NULL
pheno2 <- gather(pheno, key = "plot", value = "flowering", -date, -Vaer, -Hvem) %>% 
  mutate(flowering = as.numeric(flowering)) %>% 
  mutate(stage = factor(substring(plot, 1,1))) %>% 
  mutate(site = factor(substring(plot, 2,3))) %>% 
  mutate(plot = factor(substring(plot, 4,4))) %>% 
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>% 
  mutate(day = format(as.Date(date,format="%Y-%m-%d"))) %>%
  filter(!is.na(flowering)) # remove NA


# POLLINATOR OBSERVATIONS
polli <- read.csv("RanunculusPollinator.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)
pollinator <- polli %>%
  filter(!Tid == "") %>% # slette alle koloner med Na
  mutate(date = dmy_hm(paste(Dato, Tid))) %>%# lime sammen dato å tid
  mutate(fly = as.numeric(Fluer)) %>%
  mutate(stage = substring(Site, 1,1), site = substring(Site, 2,3)) %>%# lage to nye variabler, stage å site
  select(-Tid, -Fluer, -Site, -Dato) %>% # sletter her koloner ikke rekker, - betyr ta vekk
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>%  # bestemme rekkefölgen for en faktor
  mutate(minutes = (floor(minute(date)/10)*10)) %>%
  mutate(date = ymd_hm(paste0(format(date, "%Y-%m-%d %H:"), minutes))) %>% # making 10 minutes steps
  left_join(Temperature, by = c("date" = "date", "stage" = "stage", "site" = "site")) %>% 
  mutate(sol.og.sky = plyr::mapvalues(sol.og.sky, c("overskyet","overskyet_littsol","sol_littsky","sol"), c("overcast","cloudy","cloud_sun","sun"))) %>% 
  mutate(sol.og.sky = factor(sol.og.sky, levels = c("overcast","cloudy","cloud_sun","sun"))) %>% 
  mutate(site = factor(site)) %>% 
  mutate(day = format(as.Date(date,format="%Y-%m-%d"))) %>%
  select(-minutes.x, -minutes.y)



# Calculate and plot mean nr of flowers per site
fl <- pheno2 %>%
  group_by(stage, day, site) %>%
  summarise(n = n(), nrflower = sum(flowering)) %>% 
  select(-n)

dat <- pollinator %>%
  group_by(stage, day, site) %>%
  summarise(n = n(), nrvisit = mean(fly)) %>%
  select(-n) %>% 
  left_join(fl, by = c("day" = "day", "stage" = "stage", "site" = "site"))


save(dat, file = "PhenologyPollination.RData")
load("PhenologyPollination.RData")
head(dat)

save(pheno2, file = "Phenology.RData")
load("Phenology.RData")

save(pollinator, file = "Pollinator.RData")
load("Pollinator.RData")

# Plot flowering and visits together


# Mismatch: differnece in peak flowering - peak visit
dat %>%
  filter(stage != "L") %>% 
  group_by(site, stage) %>% 
  mutate(peak.flower = day[which.max(nrflower)], peak.fly = day[which.max(nrvisit)]) %>% 
  mutate(diff = yday(peak.fly) - yday(peak.flower)) %>% 
  ggplot() +
  geom_point(aes(x = stage, y = diff, colour = stage)) +
  geom_hline(yintercept = 0, color = "grey") +
  labs(x = "Site", y = "Difference in days: peak flower - peak visit") +
  facet_wrap(~ site)

# Mid
dat %>%
  filter(stage == "M") %>% 
  ggplot() +
  geom_point(aes(x = day, y = nrflower), color = "red") +
  geom_point(aes(x = day, y = nrvisit)) +
  facet_wrap(~ site)

# Flowering E and M
pheno2 %>%
  group_by(stage, date, site) %>%
  summarise(n = n(), nrflower = sum(flowering)) %>% 
  filter(stage != "L") %>% 
  ggplot() +
  geom_line(aes(x = date, y = nrflower, color = stage)) +
  facet_wrap(~ site)

# Polli E and M
pollinator %>%
  group_by(stage, day, site) %>%
  summarise(n = n(), nrvisit = mean(fly)) %>% 
  filter(stage != "L") %>% 
  ggplot() +
  geom_point(aes(x = day, y = nrvisit, color = stage)) +
  facet_wrap(~ site)

pollinator %>% 
  group_by(stage, day) %>% 
  summarise(n = n(), meanvisit = mean(fly), se = sd(fly)/sqrt(n)) %>% 
  mutate(day = ymd(day)) %>% 
  ggplot(aes(x = day, y = meanvisit, ymin = meanvisit - se, ymax = meanvisit + se)) + 
  geom_point() + 
  geom_errorbar() +
  scale_x_date() +
  facet_wrap(~ stage)


### Pollinaiton
pollen <- read_excel("RanunculusPollination.xlsx", col_names = TRUE)
head(pollen)

pollen %>% 
  select(Plot, Plant, Date1, Date2, Date3, DateCollection) %>% 
  gather(key = Collection, value = Date, - Plot, -Plant)

pollen <- read_excel("BiomassAndSeed.xlsx", col_names = TRUE)
head(pollen)

save(pollen, file = "Pollen.RData")
load("Pollen.RData")

pollen %>% 
  fill(Plot) %>% 
  mutate(stage = factor(substring(Plot, 1,1))) %>% 
  mutate(site = factor(substring(Plot, 2,3))) %>% 
  mutate(plot = factor(substring(Plot, 4,4))) %>% 
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>%
  mutate(plant = ifelse(Plant %in% c("C1", "C2"), "Control", "Pollinated")) %>% 
  select(-Plot, -Plant) %>% 
  group_by(plant, site, stage) %>% 
  ggplot(aes(x = plant, y = NumberOvule)) +
  geom_boxplot() +
  facet_wrap(~ stage)




### NUMBER OF OBSERVATIONS  
pheno2 %>% 
  filter(flowering > 0) %>% 
  mutate(day = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(stage, site, day) %>% 
  summarise(flower = sum(flowering)) %>% 
  summarise(n = n()) %>% 
  spread(key = stage, value = n)
  

pollinator %>% 
  filter(fly > 0) %>% 
  mutate(day = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(stage, site, day) %>% 
  summarise(flower = mean(fly)) %>% 
  summarise(n = n()) %>% 
  spread(key = stage, value = n)


##### PHENOLOGY ######

#### LIBRARIES
library("tidyverse")
library("lubridate")
library("readxl")

pn <- . %>% print(n = Inf)

########################################################################
#### READ IN DATA 2016 ####

# PHENOLOGY
pheno16 <- read.csv("Data/2016/RANfenologi.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
pheno16 <- as.data.frame(t(pheno16), stringsAsFactors = FALSE) # transpose data
names(pheno16) <- pheno16[1,] # first column = name
head(pheno16)

pheno16 <- pheno16 %>% 
  slice(-1) %>% # remove first column
  gather(key = site, value = flowering, -Dato, -Tid, -Vaer, -Hvem) %>% 
  as_tibble() %>% 
  filter(flowering != "") %>% 
  mutate(date = dmy(Dato)) %>% # do we need time?
  select(-Dato, -Tid) %>% 
  mutate(flowering = as.numeric(flowering)) %>% 
  mutate(stage = factor(substring(site, 1,1))) %>% 
  mutate(plot = factor(substring(site, 4,4))) %>% 
  mutate(site = factor(substring(site, 2,3))) %>% 
  mutate(day = as.Date(date,format="%Y-%m-%d"), year = year(date)) %>% 
  rename(weather = Vaer, name = Hvem)
  

# POLLINATOR OBSERVATIONS
pollination16 <- read.csv("Data/2016/RanunculusPollinator.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)
pollination16 <- pollination16 %>%
  as_tibble() %>% 
  filter(!Tid == "") %>% # slette alle koloner med Na
  # Fix date variables
  mutate(date = dmy_hm(paste(Dato, Tid))) %>%# lime sammen dato å tid
  mutate(minutes = (floor(minute(date)/10)*10)) %>%
  mutate(date = ymd_hm(paste0(format(date, "%Y-%m-%d %H:"), minutes))) %>% # making 10 minutes steps
  mutate(year = year (date), day = as.Date(date,format="%Y-%m-%d")) %>%
  # Fix  other variables
  mutate(stage = substring(Site, 1,1), site = substring(Site, 2,3)) %>% # lage to nye variabler, stage og site
  mutate(stage = factor(stage, levels = c("E", "M", "L")), site = factor(site)) %>%  # bestemme rekkefölgen for stage
  mutate(fly = as.numeric(Fluer), other = as.numeric(andre)) %>% # make variables numeric
  mutate(weather = plyr::mapvalues(sol.og.sky, c("overskyet","overskyet_littsol","sol_littsky","sol", "sol "), c("cloudy","cloudy_sun","sun_cloud","sun", "sun"))) %>% 
  mutate(wind = as.factor(vind)) %>% 
  mutate(remark = paste(regn, sommerfugler)) %>% 
  select(-Tid, -Fluer, -Site, -Dato, -minutes, -sol.og.sky, -vind, -andre, -regn, -sommerfugler) # sletter her koloner ikke rekker, - betyr ta vekk

########################################################################

#### READ IN DATA 2017 ####

# PHENOLOGY
pheno17 <- read.csv2("Data/2017/17-10-06_Phenology.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
pheno17 <- pheno17[-c(155:186),] # remove F09 and F10
pheno17 <- as_data_frame(t(pheno17)) # transpose data
names(pheno17) <- pheno17[1,] # first column = name

pheno17 <- pheno17 %>% 
  slice(-1) %>% # remove first column
  gather(key = site, value = flowering, -Date, -Time, -Weather, -Name) %>% 
  filter(flowering != "") %>% 
  #mutate(Time = substr(Time, 1, 5)) %>% # do we need time?
  mutate(date = dmy(Date)) %>% 
  select(-Date, -Time) %>% 
  mutate(flowering = as.numeric(flowering)) %>% 
  mutate(stage = factor(substring(site, 1,1))) %>% 
  mutate(plot = factor(substring(site, 4,4))) %>% 
  mutate(site = factor(substring(site, 2,3))) %>% 
  mutate(day = as.Date(date,format="%Y-%m-%d"), year = year(date)) %>%
  rename(weather = Weather, name = Name)


# POLLINATOR OBSERVATIONS
pollination17 <- read.csv("Data/2017/17-10-06_Pollinatorobservations.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)

pollination17 <- pollination17 %>%
  select(-X, -wind.categories., -X.1, -X.2, -X.3, -X.4) %>% 
  as_tibble() %>% 
  filter(!Time == "") %>% # slette alle koloner med Na
  # Fix date variables
  mutate(date = dmy_hm(paste(Date, Time))) %>%# lime sammen dato å tid
  mutate(minutes = (floor(minute(date)/10)*10)) %>% # round all the dates to 10 minutes
  mutate(date = ymd_hm(paste0(format(date, "%Y-%m-%d %H:"), minutes))) %>% # making 10 minutes steps
  mutate(year = year (date), day = as.Date(date, format="%Y-%m-%d")) %>%
  # Fix  other variables
  mutate(stage = substring(Site, 1,1), site = substring(Site, 2,3)) %>%# lage to nye variabler, stage å site
  mutate(stage = factor(stage, levels = c("F", "E", "M")), site = factor(site)) %>%  # bestemme rekkefölgen for stage
  mutate(fly = as.numeric(Flies), other = as.numeric(Other)) %>% # make variables numeric
  mutate(weather = plyr::mapvalues(Weather, c("cloud", "sun ","sun","sun_cloud","cloud_sun"), c("cloud", "sun","sun","sun_cloud","cloud_sun"))) %>% 
  mutate(wind = as.factor(Wind)) %>% 
  select(-Time, -Flies, -Site, -Date, -minutes, -Weather, -Wind, -Other) # sletter her koloner ikke rekker, - betyr ta vekk


########################################################################

### IMPORT SITE AND CLIMATE DATA ###
sites <- read_excel("Data/Sites.xlsx")
sites <- sites %>% 
  filter(!is.na(stage)) %>% # remove empty columns
  mutate(area = width * length) %>% 
  mutate(site = factor(site)) %>% 
  select(-width, -length)

########################################################################


### JOIN 2016 and 2017 DATA
### PHENOLOGY
phenology <- pheno16 %>% 
  bind_rows(pheno17) %>% 
  # fix weather !!!
  mutate(stage = factor(stage, levels = c("F", "E", "M", "L"))) %>% 
  group_by(day, stage, site) %>% 
  summarise(flowering = mean(flowering))


### POLLINATION
pollination <- pollination16 %>% 
  bind_rows(pollination17) %>% 
  left_join(sites, by = c("stage", "site")) %>%  # add area of each site
  # add climate data
  #left_join(Temperature, by = c("date" = "date", "stage" = "stage", "site" = "site"))
  mutate(stage = factor(stage, levels = c("F", "E","M", "L"))) %>%
  mutate(weather = factor(weather, levels = c("sun", "sun_cloud","cloud_sun", "cloud")))


### JOIN PHENOLOGY AND POLLINATION ####

# Find closest phenology observation to each pollination observation
pollination %>% 
  full_join(phenology, by = c("site", "stage"), suffix = c(".poll",".fl")) %>% 
  select(-weather, -wind, -remark, -area) %>% 
  mutate(diff = day.poll - day.fl, abs.diff = abs(diff)) %>% 
  mutate(abs.diff.mult = if_else(diff > 0, abs.diff * 1.9, abs.diff)) %>% 
  group_by(day.poll, stage, site) %>% 
  slice(which.min(abs.diff.mult)) %>% 
  mutate(flowering = ifelse(abs.diff > 3, NA, flowering)) # could check how much different flowers are


# calculate first flower and peak flower and insect obsesrvations
phenology %>% 
  group_by(year, stage, site) %>%  # group by year, stage and site to calculate first and peak
  mutate(doy = yday(date)) %>% 
  #mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  #group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  summarize(first = first(doy), peak = doy[which.max(flowering)]) %>% pn



########################################################################

### READ IN HAND-POLLINATION, BIOMASS AND REPRODUCTIVE OUTPUT ###

### 2016
pollen16 <- read_excel("Data/2016/RanunculusPollination.xlsx", col_names = TRUE, col_types = c("text", "text", "text", "date", "text", "date", "text", "date", "text", "text", "text"))


pollen16 %>% 
  select(Plot, Plant, Date1, Date2, Date3, DateCollection) %>% 
  gather(key = Collection, value = Date, - Plot, -Plant)

biomass16 <- read_excel("Data/2016/BiomassAndSeed.xlsx", col_names = TRUE)
head(biomass16)
### SOME PROBLEM WITH 2 PLANTS WHERE THERE ARE 2 PLANTS!!!


pollen16 %>% 
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

dd <- pollen %>% 
  fill(Plot) %>% 
  mutate(stage = factor(substring(Plot, 1,1))) %>% 
  mutate(site = factor(substring(Plot, 2,3))) %>% 
  mutate(plot = factor(substring(Plot, 4,4))) %>% 
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>%
  mutate(plant = ifelse(Plant %in% c("C1", "C2", "Control", "Pollinated")))


### 2017
pollen17 <- read_excel("Data/2017/Vekt_planter_fro.xlsx", col_names = TRUE)

# loop to get Plot in all cells
LoopPlot <- function(dat){
  for (i in 2:nrow(dat)){
    if(is.na(nchar(dat$Plot[i]))){
      dat$Plot[i] <- dat$Plot[i-1]
    }
  }
  return(dat)
}

pollen17 <- LoopPlot(pollen17)

pollen17 <- pollen17 %>% 
  mutate(Stage = substring(Plot, 1,1), Subplot = substring(Plot, nchar(Plot), nchar(Plot)), Site = substring(Plot, 2, nchar(Plot)-1)) %>%
  mutate(Stage = factor(Stage, levels = c("F", "E", "M")), Site = factor(Site)) %>% 
  mutate(Treatment = plyr::mapvalues(Plant, c("C1", "C2", "HP1", "HP2"), c("Control", "Control", "Pollination", "Pollination")))
  

ggplot(pollen17, aes(x = Treatment, y = Biomass)) +
  geom_boxplot() +
  facet_grid( ~ Stage)

ggplot(pollen17, aes(x = Treatment, y = ReproductiveOutput)) +
  geom_boxplot() +
  facet_grid( ~ Stage)



### TO DO!!!
# merge pollination with phenology data
# fix day variable, should be a data
# is 10 minutes data needed? to merge with phenology? there is only one phenolog data per day anyway. Should be merged with the closest day
# should I claculate the mean daily pollinators: group_by(day, stage, site) %>% summarise(mean = mean(fly))
# import biomass and reproductive output data

phenology %>% 
  filter(year == 2017) %>% 
  group_by(day, stage, site) %>% 
  summarise(mean = mean(flowering)) %>% 
  ggplot(aes(x = day, y = mean, color = stage)) +
  geom_point() +
  facet_wrap(~ site)

pollination %>% 
  #filter(stage == "F") %>% 
  group_by(day, stage, site) %>% 
  summarise(mean = mean(fly)) %>% 
  ggplot(aes(x = day, y = mean, colour = stage)) +
  geom_point() +
  facet_wrap(~ site)


#*****************************************************************************************
# Calculate and plot mean nr of flowers per site
fl <- pheno2 %>%
  group_by(stage, day, site) %>%
  summarise(n = n(), nrflower = sum(flowering)) %>% 
  select(-n)

# join phenology and insect data sets
# find closest phenology observation to insect observation and standardize insect observation by area and number of flowers!!!
dat <- pollinator %>%
  left_join(fl, by = c("day" = "day", "stage" = "stage", "site" = "site"))
group_by(stage, day, site) %>%
  summarise(n = n(), nrvisit = mean(fly)) %>%
  select(-n)




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



hist(dd$NumberOvule)
fit <- glm(NumberOvule ~ plant*stage, dd, family = "poisson")
summary(fit)
plot(fit) 





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



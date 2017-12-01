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
  as_tibble() %>% # lage en tabel
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
pollination17 <- read.csv("Data/2017/17-10-31_Pollinatorobservations.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)

pollination17 <- pollination17 %>%
  #select(-X,-wind.categories., -X.1, -X.2, -X.3, -X.4) %>% 
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

### SNOWMELT DATA ###

#2016
snomelt16 <- data_frame(year = c(rep(2016, 3)),
                           stage = c("E", "M", "L"),
                           Snowmelt_date = c("17.06.2016", "04.07.2016", "15.07.2016"))

snowmelt16 <- snomelt16 %>% 
  mutate(Snowmelt_date = dmy(Snowmelt_date))

#2017
#importing snowmelt-dataset and joining with peak-data
Date_snowmelt <- read_excel("Data/2017/Date_snowmelt.xlsx")

Date_snowmelt <- Date_snowmelt %>% 
  mutate(doy = yday(Snowmelt_date)) %>% 
  mutate(stage = as.factor(stage), site=as.factor(site)) %>% 
  rename(siteID=site) %>% 
  mutate(doy = yday(Snowmelt_date))

########################################################################

##### WEATHER THROUGHOUT SEASON #####
weather <- read_excel("Data/2017/Finse_weather.xlsx")

Weather <- weather %>% 
  mutate(precipitation = as.numeric(precipitation)) %>%
  mutate(doy = yday(date))


########################################################################


### JOIN 2016 and 2017 DATA
### PHENOLOGY
phenology <- pheno16 %>% 
  bind_rows(pheno17) %>% 
  # fix weather !!!
  mutate(stage = factor(stage, levels = c("F", "E", "M", "L"))) %>% 
  group_by(day, stage, site, year) %>% 
  summarise(flower.sum = sum(flowering), flower.mean = mean(flowering)) %>% 
  mutate(fl.sqm = flower.mean*2)


### POLLINATION
pollination <- pollination16 %>% 
  bind_rows(pollination17) %>% 
  left_join(sites, by = c("stage", "site")) %>%  # add area of each site
  # add climate data
  #left_join(Temperature, by = c("date" = "date", "stage" = "stage", "site" = "site"))
  mutate(stage = factor(stage, levels = c("F", "E","M", "L"))) %>%
  mutate(weather = factor(weather, levels = c("sun", "sun_cloud","cloud_sun", "cloud"))) %>% 
  mutate(poll.sqm = fly/area)



# SAVE AND LOAD DATA
### INSTEAD OF USING ALL THE CODE ABOVE, YOU CAN JUST LOAD THE DATA
#save(phenology, file = "Phenology.RData")
#save(pollination, file = "Pollinaton.RData")

#load("Phenology.RData")
#load("Pollinaton.RData")





########################################################################

### READ IN HAND-POLLINATION, BIOMASS AND REPRODUCTIVE OUTPUT ###

### 2016
biomass16 <- read_excel("Data/2016/17-12-01_BiomassAndSeed.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "text", "date", "text", "date", "text", "date", "text", "date", "text"))
head(biomass16)
### SOME PROBLEM WITH 2 PLANTS WHERE THERE ARE 2 PLANTS!!!


biomass16 <- biomass16 %>% 
  fill(Plot) %>% # fills empty plot names with value above
  rename(Treatment = Plant, Biomass = `Vekt biomasse`, Seed_mass = `Vekt frø`, Seed_number = `Antall frø`, Ovule_number = `Antall ovuler`, Date1 = `Dato pollinert 1`, Date2 = `Dato pollinert 2`, Date3 = `Dato pollinert 3`, Name1 = Hvem1, Name2 = Hvem2, Name3 = Hvem3, Collected = `Dato samlet frø`, NameCollected = Hvem4) %>%
  mutate(Stage = factor(substring(Plot, 1,1))) %>% 
  mutate(Site = factor(substring(Plot, 2,3))) %>% 
  mutate(Block = factor(substring(Plot, 4,4))) %>% 
  mutate(Stage = factor(Stage, levels = c("E", "M", "L"))) %>%
  mutate(Treatment = ifelse(Treatment %in% c("C1", "C2"), "Control", "Pollinated")) %>% 
  select(-Plot) %>% 
  mutate(Year = 2016)


### 2017
#importing biomass data
Biomass17 <- read_excel("Data/2017/Biomass.xlsx", col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "date", "text", "date", "text", "date", "text", "date", "text"))

### BY SITE ###
Biomass17 <- Biomass17 %>% 
  rename(Treatment = Plant_type, Date1 = `Date  1`, Date2 = `Date 2`, Date3 = `Date 3`, Name1 = `Name 1`, Name2 = `Name 2`, Name3 = `Name 3`) %>%
  mutate(Stage = factor(Stage, levels = c("F", "E", "M"))) %>% 
  mutate(Year = 2017)

Biomass <- biomass16 %>% 
  bind_rows(Biomass17)


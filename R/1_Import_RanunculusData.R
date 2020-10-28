##### PLANT POLLINATOR DATA FINSE 2016 AND 2017 ######
# import and prepare plant-pollinator data

#### LIBRARIES
library("tidyverse")
library("lubridate")
library("readxl")

pn <- . %>% print(n = Inf)

########################################################################
#### READ IN DATA 2016 ####

# PHENOLOGY
pheno16 <- read.csv("Data_plant_pollinator_Finse_2016_2017/2016/RANfenologi.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
pheno16 <- as.data.frame(t(pheno16), stringsAsFactors = FALSE) # transpose data
names(pheno16) <- pheno16[1,] # first column = name

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
pollination16 <- read.csv("Data_plant_pollinator_Finse_2016_2017/2016/RanunculusPollinator.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)
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
pheno17 <- read.csv2("Data_plant_pollinator_Finse_2016_2017/2017/17-10-06_Phenology.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
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
pollination17 <- read.csv("Data_plant_pollinator_Finse_2016_2017/2017/17-10-31_Pollinatorobservations.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)

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
sites <- read_excel("Data_plant_pollinator_Finse_2016_2017/Sites.xlsx")
sites <- sites %>% 
  filter(!is.na(stage)) %>% # remove empty columns
  mutate(area = width * length) %>% 
  mutate(site = factor(site)) %>% 
  select(-width, -length)

########################################################################

### SNOWMELT DATA ###

#2016
snomelt16 <- tibble(year = c(rep(2016, 3)),
                           stage = c("E", "M", "L"),
                           Snowmelt_date = c("17.06.2016", "04.07.2016", "15.07.2016"))

snowmelt16 <- snomelt16 %>% 
  mutate(Snowmelt_date = dmy(Snowmelt_date)) %>% 
  mutate(doy = yday(Snowmelt_date))

#2017
#importing snowmelt-dataset and joining with peak-data
Date_snowmelt <- read_excel("Data_plant_pollinator_Finse_2016_2017/2017/Date_snowmelt.xlsx")

Date_snowmelt <- Date_snowmelt %>% 
  mutate(doy = yday(Snowmelt_date)) %>% 
  mutate(stage = as.factor(stage), site=as.factor(site)) %>% 
  rename(siteID=site) %>% 
  mutate(doy = yday(Snowmelt_date))

########################################################################

##### WEATHER THROUGHOUT SEASON #####
weather16 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Finse_weather_2016.xlsx")
colnames(weather16) <- iconv(colnames(weather16), "latin1", "ASCII", sub = "q")
  
Weather16 <- weather16 %>% 
  rename("date" = "Dato", "temperature" = "Middeltemperatur", "precipitation" = "Nedbqqr") %>% 
  mutate(doy = yday(date))


weather17 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2017/Finse_weather.xlsx")

Weather <- weather17 %>% 
  mutate(precipitation = as.numeric(precipitation)) %>%
  mutate(doy = yday(date)) %>% 
  bind_rows(Weather16) %>% 
  mutate(tempAboveZero = ifelse(temperature > 0, temperature, 0))


########################bind_rows()################################################


### JOIN 2016 and 2017 DATA
### PHENOLOGY
phenology <- pheno16 %>% 
  bind_rows(pheno17) %>% 
  # fix weather !!!
  mutate(stage = factor(stage, levels = c("F", "E", "M", "L"))) %>% 
  mutate(siteID = paste(stage, site, sep = " ")) %>% 
  group_by(day, stage, site, year, siteID) %>% 
  summarise(flower.sum = sum(flowering), flower.mean = mean(flowering)) %>% 
  mutate(fl.sqm = flower.mean*2)


### POLLINATION
pollination <- pollination16 %>% 
  bind_rows(pollination17) %>% 
  left_join(sites, by = c("stage", "site")) %>%  # add area of each site
  # add climate data
  #left_join(Temperature, by = c("date" = "date", "stage" = "stage", "site" = "site"))
  mutate(stage = factor(stage, levels = c("F", "E","M", "L"))) %>%
  mutate(siteID = paste(stage, site, sep = " ")) %>% 
  mutate(weather = factor(weather, levels = c("sun", "sun_cloud","cloud_sun", "cloud"))) %>% 
  mutate(poll.sqm = fly/area)





########################################################################

### READ IN HAND-POLLINATION, BIOMASS AND REPRODUCTIVE OUTPUT ###

### 2016
biomass16 <- read_excel("Data/2016/17-12-01_BiomassAndSeed.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "date", "text", "date", "text", "date", "text", "date", "text"))

### SOME PROBLEM WITH 2 PLANTS WHERE THERE ARE 2 PLANTS!!!


biomass16 <- biomass16 %>% 
  fill(Plot) %>% # fills empty plot names with value above
  rename(Biomass = `Vekt biomasse`, Seed_mass = `Vekt frø`, Seed_number = `Antall frø`, Ovule_number = `Antall ovuler`, Date1 = `Dato pollinert 1`, Date2 = `Dato pollinert 2`, Date3 = `Dato pollinert 3`, Name1 = Hvem1, Name2 = Hvem2, Name3 = Hvem3, Collected = `Dato samlet frø`, NameCollected = Hvem4) %>%
  mutate(Stage = gsub("^([A-Z]).*", "\\1", Plot)) %>% 
  mutate(Site = gsub(".(\\d+).", "\\1", Plot)) %>% 
  mutate(Site = ifelse(nchar(Site) < 2, paste(0, Site, sep = ""), Site)) %>% # add a zero to Site
  mutate(Block = gsub(".*([a-z])$", "\\1", Plot)) %>% 
  mutate(Treatment = ifelse(Plant %in% c("C1", "C2"), "Control", "Pollinated")) %>%
  mutate(Biomass = as.numeric(Biomass), Seed_mass = as.numeric(Seed_mass)) %>%
  select(-Plot) %>% 
  mutate(Year = 2016) %>% 
  mutate(siteID = paste(Stage, Site, sep = " "))


### 2017
#importing biomass data
Biomass17 <- read_excel("Data/2017/Biomass.xlsx", col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "date", "text", "date", "text", "date", "text", "date", "text"))

### BY SITE ###
Biomass17 <- Biomass17 %>% 
  rename(Treatment = Plant_type, Date1 = `Date  1`, Date2 = `Date 2`, Date3 = `Date 3`, Name1 = `Name 1`, Name2 = `Name 2`, Name3 = `Name 3`, siteID = `Site`) %>%
  mutate(Treatment = ifelse(Treatment %in% c("C"), "Control", "Pollinated"),
         Site = substr(siteID, 3, 4),
         Year = 2017) %>% 
  # Fix wrong dates
  mutate(Date1 = if_else(siteID == "E 01" & Block == "b" & Plant == "HP1", ymd_hms("2017-07-23", truncated = 3), Date1),
         Date2 = if_else(siteID == "E 01" & Block == "b"& Plant == "HP1", ymd_hms("2017-07-25", truncated = 3), Date2),
         Date1 = if_else(siteID == "E 01" & Block == "b" & Plant == "HP2", ymd_hms("2017-07-25", truncated = 3), Date1),
         Date2 = if_else(siteID == "E 01" & Block == "b"& Plant == "HP2", ymd_hms("2017-07-26", truncated = 3), Date2),
         Date1 = if_else(siteID == "E 03" & Block == "c" & Plant == "HP1", ymd_hms("2017-07-25", truncated = 3), Date1),
         Date1 = if_else(siteID == "E 03" & Block == "c" & Plant == "HP2", ymd_hms("2017-07-25", truncated = 3), Date1),
         Date1 = if_else(siteID == "E 07" & Block == "a" & Plant == "HP2", ymd_hms("2017-07-26", truncated = 3), Date1),
         Date1 = if_else(siteID == "M 04" & Block == "a" & Plant == "HP1", ymd_hms("2017-07-25", truncated = 3), Date1),
         Collected = if_else(siteID == "E 04" & Block == "d" & Plant == "HP2", ymd_hms("2017-08-18", truncated = 3), Collected))


Biomass <- biomass16 %>% 
  bind_rows(Biomass17) %>% 
  filter(!is.na(Stage)) %>% 
  mutate(Stage = factor(Stage, levels = c("F", "E", "M", "L")),
         Tot_Ovule = Seed_number + Ovule_number,
         Seed_potential = Seed_number / Tot_Ovule, 
         BlockID = paste(Stage, Site, Block, sep = " ")) %>% 
  # Convert dates to doy
  mutate(Date1 = yday(Date1),
         Date2 = yday(Date2),
         Date3 = yday(Date3),
         Collected = yday(Collected),
         TimeToRipe = Collected - Date1)



########################################################################
### JOIN PHENOLOGY AND POLLINATION ####

# Find closest phenology observation to each pollination observation
pollination2 <- pollination %>% 
  full_join(phenology, by = c("site", "stage", "siteID"), suffix = c(".poll",".fl")) %>% 
  select(-weather, -wind, -remark) %>% 
  mutate(diff = day.poll - day.fl, abs.diff = abs(diff)) %>% 
  mutate(abs.diff.mult = if_else(diff > 0, abs.diff * 1.9, abs.diff)) %>% 
  group_by(day.poll, stage, site) %>% 
  slice(which.min(abs.diff.mult)) %>%
  mutate(doy = yday(date)) %>% 
  mutate(flowering = ifelse(abs.diff > 3, NA, flower.sum)) %>% # could check how much different flowers are
  mutate(tot.flowers = flower.sum*2*area) %>% # added new column: total number of flowers pr. area (based on mean flowers)
  mutate(std.fly = fly/tot.flowers) # standardize insect observation by fl per area


# Mean total number of flowers and insect visits per
MeanFlyFlower <- pollination2 %>% 
  group_by(year.poll, siteID) %>% 
  summarise(MeanFlowers = mean(tot.flowers),
            MeanVisit = mean(std.fly)) %>% 
  rename(Year = year.poll)

Biomass <- Biomass %>% 
  left_join(MeanFlyFlower, by = c("Year", "siteID")) %>%
  filter(!is.na (Seed_mass))



### Cumulative temperature for pollinated plants

Weather2 <- Weather %>% 
  select(doy, temperature, tempAboveZero, precipitation)

Weather2 %>% 
  filter(doy > 193, doy < 228) %>% 
  group_by() %>% 
  summarise(sum(tempAboveZero), sum(precipitation))

Period <- Biomass %>% 
  filter(Treatment == "Pollinated") %>% 
  group_by(Year, BlockID) %>% 
  summarise(MinDate = min(Date1, na.rm = TRUE), MaxDate = max(Collected, na.rm = TRUE))
  
MASL <- read.csv("Data_plant_pollinator_Finse_2016_2017/MASL.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)

WeatherAndBiomass <- Biomass %>% 
  select(Year, Stage, siteID, BlockID, Plant, Treatment, Biomass, Seed_mass, Seed_number, Ovule_number, Tot_Ovule, Seed_potential, MeanFlowers, MeanVisit) %>% 
  left_join(Period, by = c("BlockID", "Year")) %>%
  left_join(MASL, by = c("siteID")) %>%
  crossing(Weather2) %>%
  mutate(TempAdi = temperature - Adiabatic.temp) %>% #trekt i fra den adiabatiske temperaturen, mer korrekt temp pr site
  mutate(tempAboveZeroAdi = ifelse(TempAdi > 0, TempAdi, 0)) %>%
  select(-tempAboveZero) %>%
  filter(doy > MinDate, doy < MaxDate) %>%
  group_by(Year, BlockID, Plant) %>%
  summarise(CumTemp = sum(tempAboveZeroAdi, na.rm = TRUE), CumPrec = sum(precipitation, na.rm = TRUE)) %>% 
  left_join(Biomass, by = c("Year", "BlockID", "Plant"))
  
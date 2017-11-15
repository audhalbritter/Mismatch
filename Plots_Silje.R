##### PEAK DIFFERENCE PLOT #######################################

### BY SITE ###

# Joining data and creating peak difference
peak.data <- peak.fl %>%
  left_join(peak.pol, by=c("year"="year", "stage"="stage", "site"="site")) %>% 
  select(year, stage, site, peak.fl, peak.pol) %>%
  filter(year == "2017") %>% 
  mutate(peak.diff = peak.fl-peak.pol) %>% 
  mutate(siteID = paste(stage, site))

View(peak.data)

# Making plot showing peak difference by site
ggplot(peak.data, aes(y = peak.diff, x = siteID)) +
  geom_point() +
  theme_minimal()


### BY SNOWMELT-DATE ###

#importing snowmelt-dataset and joining with peak-data
library(readxl)
Date_snowmelt <- read_excel("~/Mismatch/Data/2017/Date_snowmelt.xlsx")

peak_snowmelt <- peak.data %>% 
  full_join(Date_snowmelt, by=c("stage"="stage", "siteID"="site")) %>% 
  mutate(doy = yday(Snowmelt_date))

View(peak_snowmelt)

# Making plot showing peak difference by date of snowmelt
ggplot(peak_snowmelt, aes(y = peak.diff, x = doy)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_minimal()




##### REPRODUCTIVE OUTPUT PLOT #####################################

#importing biomass data
library(readxl)
Biomass <- read_excel("~/Mismatch/Data/2017/Biomass.xlsx")
View(Biomass)

### BY SITE ###
Biomass %>% 
  mutate(Stage = factor(Stage, levels = c("F", "E","M", "L")))

Biomass$Stage <- as.character(Biomass$Stage)
Biomass$Stage <- factor(Biomass$Stage, levels=c("F", "E", "M")) 

ggplot(Biomass, aes(y=Seed_mass, x=Plant_type, color = Plant_type)) +
  geom_boxplot() +
  facet_grid(~Stage) + 
  theme_minimal() #outliers were re-weighed: they are correct


### BY SNOWMELT DATE ###
Biomass.snowmelt <- Biomass %>% 
  left_join(Date_snowmelt, by=c("Site"="site", "Stage"="stage")) %>% 
  mutate(Stage = factor(Stage, levels = c("F", "E","M", "L"))) %>% 
  mutate(doy = yday(Snowmelt_date))

ggplot(Biomass.snowmelt, aes(y=Seed_mass, x=doy, color=Stage)) +
  geom_point() +
  theme_minimal()

ggplot(Biomass.snowmelt, aes(y=Seed_mass, x=Plant_type)) +
  geom_boxplot() +
  facet_grid(~doy)
  theme_minimal()

### PLOTTING SEEDS AGAINST BIOMASS ###

ggplot(Biomass, aes(y=Seed_mass, x=Biomass, color=Plant_type, symbol)) +
  geom_point() +
  theme_minimal()


### PLOTTING REPRODUCTIVE OUTPUT AGAINST DEGREE OF MISMATCH (PEAK DIFF)

Reprod <- Biomass %>% 
  left_join(AllPred, by=c("Site"="siteID", "Stage"="stage")) %>% 
  select(Stage, Site, Plant_type, Seed_mass, peak.diff)

Reprod %>% 
  filter(Plant_type == "C", Stage != "M") %>% 
  ggplot(aes(y=Seed_mass, x=peak.diff, color=Stage)) +
  geom_point() +
  theme_minimal()

ggplot(Reprod, aes(y=Seed_mass, x = peak.diff, color = Stage)) +
  geom_boxplot() +
  theme_minimal()

##### WEATHER THROUGHOUT SEASON #####################################
weather <- read_excel("~/Mismatch/Data/2017/Finse_weather.xlsx")
View(weather)

Weather <- weather %>% 
  mutate(precipitation = as.numeric(precipitation), temperature = as.numeric(temperature)) %>%    mutate(doy = yday(date)) %>% 
  filter(doy>151)

ggplot(Weather, aes(x = doy, y = temperature, color="Temperature")) +
  geom_point()+
  geom_line() + labs(y="Temperature(°C)", color="", x="Day of snowmelt") +
  geom_point(aes(y = precipitation, color="Precipitation")) +
  geom_line(aes(y=precipitation, color="Precipitation")) +
  scale_y_continuous(sec.axis = sec_axis(~./2), name = "Precipitation(mm)") +
  theme_minimal()

ggplot(Weather, aes(y=temperature, x=doy) +
  geom_point()+
  geom_line()

Weather %>% 
  
  ggplot(aes(x=date, y=precipitation)) +
  geom_line()

ggplot(Weather, aes(x=doy, y = temperature)) +
  geom_point()



################################
weather <- pollination %>% 
  select(year, day, weather) %>% 
  mutate(doy = yday(day)) %>% 
  filter(year == "2017")

ggplot(weather, aes(y=weather, x=doy, colour=weather)) +
  geom_point()


wind <- pollination %>%
  select(year, day, wind) %>% 
  mutate(doy = yday(day)) %>% 
  filter(year == "2017")

ggplot(wind, aes(y=wind, x=doy)) +
  geom_point()

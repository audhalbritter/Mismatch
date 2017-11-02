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

ggplot(Biomass, aes(y=Seed_mass, x=Plant_type)) +
  geom_boxplot() +
  facet_grid(~Stage) + 
  theme_minimal() #outliers were re-weighed: they are correct


### BY SNOWMELT DATE ###
Biomass.snowmelt <- Biomass %>% 
  left_join(Date_snowmelt, by=c("Site"="site", "Stage"="stage")) %>% 
  mutate(Stage = factor(Stage, levels = c("F", "E","M", "L"))) %>% 
  mutate(doy = yday(Snowmelt_date))

ggplot(Biomass.snowmelt, aes(y=Seed_mass, x=doy, color=Plant_type)) +
  geom_point() +
  theme_minimal()

ggplot(Biomass.snowmelt, aes(y=Seed_mass, x=Plant_type)) +
  geom_boxplot() +
  facet_grid(~doy)
  theme_minimal()

### PLOTTING SEEDS AGAINST BIOMASS ###

ggplot(Biomass, aes(y=Seed_mass, x=Biomass, color=Plant_type)) +
  geom_point() +
  theme_minimal()

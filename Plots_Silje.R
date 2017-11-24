##### PEAK DIFFERENCE PLOT #######################################

peak_snowmelt <- peak.data %>% 
  full_join(Date_snowmelt, by=c("stage"="stage", "siteID"="site")) %>% 
  mutate(doy = yday(Snowmelt_date))


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



# Making plot showing peak difference by date of snowmelt
ggplot(peak_snowmelt, aes(y = peak.diff, x = doy)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_minimal()

#Using predicted data
MismatchSnow <- AllPred %>% 
  select(stage, siteID, peak.diff) %>%
  left_join(Date_snowmelt, by=c("stage"="stage", "siteID"="site" )) %>% 
  mutate(stage = factor(stage, levels = c("F", "E","M")))

MismatchSnow$stage <- as.character(MismatchSnow$stage)
MismatchSnow$stage <- factor(MismatchSnow$stage, levels=c("F", "E", "M")) 

ggplot(MismatchSnow, aes(y=abs(peak.diff), x=Snowmelt_date, color=stage)) +
  #geom_point() +
  geom_jitter() +
  labs(y="Mismatch (no. days)", x="Date of snowmelt", color="Stage") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme_minimal(base_size = 18)
  
### Peak flowering vs. peak pollinators

AllPred %>%
  ggplot(aes(x = peak.poll, y = peak.fl, color = stage)) +
  geom_point() + labs(x = "Peak pollinator visitation (d.o.y)", y = "Peak flowering (d.o.y)", color = "Stage") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  geom_abline(slope = 0.6221, intercept = 75.6327, color = "blue") +
  geom_abline(slope = 1, color = "grey80", linetype = "dashed") +
  theme_minimal(base_size = 18)




stage_names <- c("F"="E", "E"="M", "M"="L")

ggplot(Biomass, aes(y=Seed_mass, x=Plant_type, fill=as.factor(Plant_type))) +
  geom_boxplot() +
  facet_grid(~Stage, labeller = as_labeller(stage_names)) + 
  theme_minimal(base_size = 18) + #outliers were re-weighed: they are correct
  labs(y="Reproductive output (g)", x="Treatment", fill="Treatment")+
  scale_fill_manual(labels = c("control (C)", "hand-pollinated (HP)"), values = c("#F8766D", "#00BA38"))


### BY SNOWMELT DATE ###
Biomass.snowmelt <- Biomass %>% 
  left_join(Date_snowmelt, by=c("Site"="site", "Stage"="stage")) %>% 
  mutate(Stage = factor(Stage, levels = c("F", "E","M", "L"))) 
  #mutate(doy = yday(Snowmelt_date))

ggplot(Biomass.snowmelt, aes(y=Seed_mass, x=Snowmelt_date, color=Stage)) +
  geom_point() +
  labs(x="Date of snowmelt", y="Reproductive output") +
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

Reprod$Stage <- as.character(Reprod$Stage)
Reprod$Stage <- factor(Reprod$Stage, levels=c("F", "E", "M")) 


Reprod %>% 
  #filter(Plant_type == "C", Stage != "M") %>% 
  ggplot(aes(y=Seed_mass, x=abs(peak.diff), color=Stage)) +
  geom_jitter() +
  labs(y="Reproductive output (g)", x="Mismatch") +
  scale_color_manual(labels = c("E", "M", "L"), values = c("#F8766D", "#00BA38", "#619CFF")) +
  theme_minimal(base_size = 18)

ggplot(Reprod, aes(y=Seed_mass, x = peak.diff, color = Stage)) +
  geom_boxplot() +
  theme_minimal()


## PLOTTING REPRODUCTIVE OUTPUT AGAINST OVERLAP
output.overlap <- Biomass %>% 
  select(Stage, Site, Block, Plant_type, Seed_mass) %>% 
  left_join(Overlap_data, by=c("Stage"="stage", "Site"="siteID")) %>% 
  select(Stage, Site, Block, Plant_type, Seed_mass, overlap)

output.overlap %>% 
  ggplot(aes(x = overlap, y = Seed_mass, color = Stage)) +
  geom_jitter() + labs(x = "Overlap in occurance (no. days)", y = "Reproductive output (g)") +
  scale_color_manual(labels = c("E", "M", "L"), values = c("#F8766D", "#00BA38", "#619CFF")) +
  theme_minimal(base_size = 18)

##### WEATHER THROUGHOUT SEASON ###############################################
weather <- read_excel("~/Mismatch/Data/2017/Finse_weather.xlsx")
View(weather)

Weather <- weather %>% 
  mutate(precipitation = as.numeric(precipitation), temperature = as.numeric(temperature)) %>%
  mutate(doy = yday(date)) %>% 
  filter(doy>151)

ggplot(Weather, aes(x = date, y = precipitation, color="Precipitation (daily avg.)")) +
  geom_point()+
  geom_line() + labs(y = "Precipitation(mm)", color="", x="") +
  geom_point(aes(y = (temperature*2), color="Temperature (°C)")) +
  geom_line(aes(y=(temperature*2), color="Temperature (°C)")) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Temperature(°C)")) +
  scale_color_manual(labels = c ("Precipitation (daily avg.)","Temperature (daily avg.)"), values=c("#619CFF", "#F8766D")) +
  theme_minimal(base_size = 18)

#*******************************************************************************
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
#************************************************************

  
  
##### DAYS FROM SNOWMELT TO FIRST FLOWER ##############################################
First <- phenology %>% 
  filter(year == 2017) %>% 
  mutate(doy = yday(day)) %>% 
  group_by(stage, site) %>% 
  summarise(first = first(doy), peak = doy[which.max(flower.sum)], last = last(doy)) %>% 
  rename(Stage=stage, Site = site) %>% 
  select(Stage, Site, first) %>%
  bind_cols(Date_snowmelt) %>% 
  select(Stage, Site, first, Snowmelt_date) %>% 
  mutate(day = as.Date(Snowmelt_date)) %>% 
  mutate(doy = yday(day)) %>% 
  mutate(lag = first-doy)

#plot
First %>% 
  ggplot(aes(x=day, y = lag, color=Stage)) +
  geom_point() +
  theme_minimal()


##### TIME OF SNOWMELT/SITES ##################################################

snowmelt <- Date_snowmelt %>% 
  mutate(day = as.Date(Snowmelt_date)) %>% 
  mutate(doy = yday(day)) %>% 
  mutate(stage = factor(stage, levels = c("F", "E","M")))  

snowmelt %>% 
  ggplot(aes(x = stage, y = doy, color = stage)) +
  geom_jitter() +
  theme_minimal()



##### POLLINATION/FLOWERING-OVERLAP #########################

Overlap_data <- AllPred %>%
  mutate(first = ifelse(first.poll > first.fl, first.poll, first.fl)) %>%
  mutate(last = ifelse(last.poll < last.fl, last.poll, last.fl)) %>%
  mutate(overlap = last - first)

Overlap_data %>% 
  ggplot(aes(x = siteID, y = overlap, color = stage)) +
  geom_point() + labs(x = "Site", y = "Overlap in occurance (no. days)") +
  theme_minimal()



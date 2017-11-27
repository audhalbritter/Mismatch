source("RanunculusData.R")


##### MISMATCH ####################################################################

## BY SITE
AllPred %>% 
  mutate(siteID = paste(stage, site)) %>% 
  ggplot(aes(y = abs(peak.diff), x = siteID, color = stage)) +
  geom_point() +
  theme_minimal()


## BY SNOWMELT
AllPred %>% 
  select(stage, siteID, peak.diff) %>%
  left_join(Date_snowmelt, by=c("stage"="stage", "siteID"="siteID")) %>% 
  mutate(stage = factor(stage, levels = c("F", "E","M"))) %>% 
  ggplot(MismatchSnow, aes(y=abs(peak.diff), x=Snowmelt_date, color=stage)) +
  #geom_point() +
  geom_jitter() +
  labs(y="Mismatch (no. days)", x="Date of snowmelt", color="Stage") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme_minimal(base_size = 18)
  

## PEAK VS. PEAK
AllPred %>%
  ggplot(aes(x = peak.poll, y = peak.fl, color = stage)) +
  geom_point() + labs(x = "Peak pollinator visitation (d.o.y)", y = "Peak flowering (d.o.y)", color = "Stage") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  geom_abline(slope = 0.6221, intercept = 75.6327, color = "blue") +
  geom_abline(slope = 1, color = "grey80", linetype = "dashed") +
  theme_minimal(base_size = 18)



#### REPRODUCTIVE OUTPUT ##########################################################

## POLLEN LIMITATION, BY SITE
stage_names <- c("F"="E", "E"="M", "M"="L")

ggplot(Biomass, aes(y=Seed_mass, x=Plant_type, fill=as.factor(Plant_type))) +
  geom_boxplot() +
  facet_grid(~Stage, labeller = as_labeller(stage_names)) + 
  theme_minimal(base_size = 18) + #outliers were re-weighed: they are correct
  labs(y="Reproductive output (g)", x="Treatment", fill="Treatment")+
  scale_fill_manual(labels = c("control (C)", "hand-pollinated (HP)"), values = c("#F8766D", "#00BA38"))


## BY SNOWMELT DATE
Biomass %>% 
  left_join(Date_snowmelt, by=c("Site"="siteID", "Stage"="stage")) %>% 
  mutate(Stage = factor(Stage, levels = c("F", "E","M", "L"))) %>% 
  ggplot(aes(y=Seed_mass, x=Snowmelt_date, color=Stage)) +
  geom_point() +
  labs(x="Date of snowmelt", y="Reproductive output") +
  theme_minimal()



#### MISMATCH AND REPRODUCTIVE OUTPUT #############################################

## OUTPUT VS. MISMATCH (PEAK DIFF)

Reprod <- Biomass %>% 
  left_join(AllPred, by=c("Site"="siteID", "Stage"="stage")) %>% 
  select(Stage, Site, Plant_type, Seed_mass, peak.diff) %>% 
  mutate(Stage = factor(Stage, levels = c("F", "E","M"))) %>% 
  ggplot(aes(y=Seed_mass, x=abs(peak.diff), color=Stage)) +
  geom_jitter() +
  labs(y="Reproductive output (g)", x="Mismatch") +
  scale_color_manual(labels = c("E", "M", "L"), values = c("#F8766D", "#00BA38", "#619CFF")) +
  theme_minimal(base_size = 18)


## OUTPUT VS. DAYS OF OVERLAP
Biomass %>% 
  select(Stage, Site, Block, Plant_type, Seed_mass) %>% 
  left_join(Overlap_data, by=c("Stage"="stage", "Site"="siteID")) %>% 
  select(Stage, Site, Block, Plant_type, Seed_mass, overlap) %>% 
  ggplot(aes(x = overlap, y = Seed_mass, color = Stage)) +
  geom_jitter() + labs(x = "Overlap in occurance (no. days)", y = "Reproductive output (g)") +
  scale_color_manual(labels = c("E", "M", "L"), values = c("#F8766D", "#00BA38", "#619CFF")) +
  theme_minimal(base_size = 18)



##### WEATHER THROUGHOUT SEASON ###################################################

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


  
##### SNOWMELT ####################################################################

## OVERLAP (LAG) VS. SNOWMELT
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
  mutate(lag = first-doy) %>% 
  ggplot(aes(x=day, y = lag, color=Stage)) +
  geom_point() +
  theme_minimal()


## SNOWMELT VS. SITES

Date_snowmelt %>% 
  mutate(day = as.Date(Snowmelt_date)) %>% 
  mutate(doy = yday(day)) %>% 
  mutate(stage = factor(stage, levels = c("F", "E","M"))) %>% 
  ggplot(aes(x = stage, y = doy, color = stage)) +
  geom_jitter() +
  theme_minimal()



##### OVERLAP VS. SITES ###########################################################

overlap <- AllPred %>%
  mutate(first = ifelse(first.poll > first.fl, first.poll, first.fl)) %>%
  mutate(last = ifelse(last.poll < last.fl, last.poll, last.fl)) %>%
  mutate(overlap = last - first)
ggplot(overlap, aes(x = siteID, y = overlap, color = stage)) +
  geom_point() + labs(x = "Site", y = "Overlap in occurance (no. days)") +
  theme_minimal()



##### SUM TEMP. AND SUM PRECIPITATION #############################################

## TEMPERATURE
# Comparing to mismatch
sumTP %>% 
  mutate(peak.diff=peak.fl-peak.poll) %>% 
  ggplot(aes(x = abs(peak.diff), y = sumT.fl, color = stage)) +
  geom_point() +  #more correct to not use abs(peak.diff) here?
  labs(x= "Degree of mismatch (no. days)", y = "Sum temperature (snowmelt to peak flowering)") +
  theme_minimal()

sumTP %>% 
  mutate(peak.diff=peak.fl-peak.poll) %>% 
  ggplot(aes(x = peak.diff, y = sumT.poll, color = stage)) +
  geom_point() + 
  labs(x= "Degree of mismatch (no. days)", y = "Sum temperature (snowmelt to peak pollinator)") +
  theme_minimal()

# Compare to overlap
overlap %>% 
  select(overlap) %>% 
  left_join(sumTP, by=c("stage")) %>% 
  ggplot(aes(x = overlap, y = sumT.fl, color = stage)) +
  geom_point()

overlap %>% 
  select(overlap) %>% 
  left_join(sumTP, by=c("stage")) %>% 
  ggplot(aes(x = overlap, y = sumT.poll, color = stage)) +
  geom_point()

# Comapring to reproductive output
Biomass17 %>% 
  select(Stage, Site, Seed_mass) %>% 
  left_join(sumTP, by = c("Stage"="stage", "Site"="siteID")) %>% 
  ggplot(aes(x = sumT.fl, y = Seed_mass, color = Stage))+
  geom_point()

Biomass17 %>% 
  select(Stage, Site, Seed_mass) %>% 
  left_join(sumTP, by = c("Stage"="stage", "Site"="siteID")) %>% 
  ggplot(aes(x = sumT.poll, y = Seed_mass, color = Stage))+
  geom_point()

## PRECIPITATION
# Comparing to mismatch
sumTP %>% 
  mutate(peak.diff=peak.fl-peak.poll) %>% 
  ggplot(aes(x = peak.diff, y = sumP.fl, color = stage)) +
  geom_point() + #more correct to not use abs(peak.diff) here?
  labs(x= "Degree of mismatch (no. days)", y = "Sum precipitation (snowmelt to peak flowering)") +
  theme_minimal()

sumTP %>% 
  mutate(peak.diff=peak.fl-peak.poll) %>% 
  ggplot(aes(x = peak.diff, y = sumP.poll, color = stage)) +
  geom_point() + 
  labs(x= "Degree of mismatch (no. days)", y = "Sum precipitation (snowmelt to peak pollinator)") +
  theme_minimal()

# Compare to overlap
overlap %>% 
  select(overlap) %>% 
  left_join(sumTP, by=c("stage")) %>% 
  ggplot(aes(x = overlap, y = sumP.fl, color = stage)) +
  geom_point()

overlap %>% 
  select(overlap) %>% 
  left_join(sumTP, by=c("stage")) %>% 
  ggplot(aes(x = overlap, y = sumP.poll, color = stage)) +
  geom_point()

# Comparing to reproductive output
Biomass17 %>% 
  select(Stage, Site, Seed_mass) %>% 
  left_join(sumTP, by = c("Stage"="stage", "Site"="siteID")) %>% 
  ggplot(aes(x = sumP.fl, y = Seed_mass, color = Stage))+
  geom_point()

Biomass17 %>% 
  select(Stage, Site, Seed_mass) %>% 
  left_join(sumTP, by = c("Stage"="stage", "Site"="siteID")) %>% 
  ggplot(aes(x = sumP.poll, y = Seed_mass, color = Stage))+
  geom_point()

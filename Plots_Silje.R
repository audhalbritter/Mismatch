source("RanunculusData.R")
source("PeakPredicted.R")

library(gridExtra)

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
  mutate(stage = factor(stage, levels = c("F", "E", "M"))) %>% 
  ggplot(aes(y=(abs(peak.diff)), x=doy, color = stage)) +
  geom_jitter() +
  #geom_smooth(method="lm", aes(group = 1)) +
  labs(y="Mismatch (no. days)", x="Day of snowmelt") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme_light(base_size = 18)


## FLOWERING + POLLINATOR VISITS *not finished*
all %>% 
  mutate(siteID = paste(stage, site, sep = " ")) %>% 
  filter(year.x == 2017, siteID == "E 04") %>% 
  ggplot(aes(x = doy, y = value, color = variable)) +
  geom_point() +
  geom_line(aes(y = pred, color = variable)) +
  labs(x = "Day of the year", y = "No. flowers") +
  scale_color_manual(values = c("yellow", "green")) +
  scale_y_continuous(sec.axis= sec_axis(~./10, name="Pollinator visitats")) +
  ggtitle("E 04") + #unique(paste(dat$year, dat$stage, dat$site, sep = " "))) +
  theme_minimal()
  

## PEAK VS. PEAK
## POINTS COLORED AS STAGE
# 2016
AllPred %>%
  filter(stage != "L") %>% 
  ggplot(aes(x = peak.poll, y = peak.fl)) +
  geom_point(aes(color = factor(stage))) +
  labs(x = "Peak pollinator visitation (d.o.y)", y = "Peak flowering (d.o.y)", color = "Snowmelt stage") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#00BA38", "darkorange", "#619CFF")) +
  #geom_abline(slope = 0.6221, intercept = 75.6327, color = "grey50", linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, colour = "red") +
  geom_abline(slope = 1, color = "grey50") +
  theme_light(base_size = 16) +
  facet_wrap(~year) +
  theme(legend.position = "bottom", legend.title=element_text(size=12), legend.text=element_text(size=12))
  #ggtitle("a) 2016")

#2017
plotB <- AllPred %>%
  filter(year == 2017) %>% 
  ggplot(aes(x = peak.poll, y = peak.fl)) +
  geom_point(aes(color = factor(stage))) +
  labs(x = "Peak pollinator visitation (d.o.y)", y = "Peak flowering (d.o.y)", color = "Snowmelt stage") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  geom_abline(slope = 0.6221, intercept = 75.6327, color = "red") +
  geom_abline(slope = 1, color = "grey80", linetype = "dashed") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  ggtitle("b) 2017")

grid.arrange(plotA, plotB)

## POINTS COLORED AS TIME OF SNOWMELT
#2017
AllPred %>% 
  select(year, stage, siteID, peak.poll, peak.fl) %>%
  filter(year == 2017) %>% 
  left_join(Date_snowmelt, by=c("stage"="stage", "siteID"="siteID")) %>%
  mutate(stage = factor(stage, levels = c("F", "E", "M"))) %>%
  ggplot(aes(x = peak.poll, y = peak.fl, colour = doy)) +
  geom_point() +
  labs(x = "Peak pollinator visitation (d.o.y)", y = "Peak flowering (d.o.y)", color = "Time of snowmelt") +
  #scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  scale_color_gradient2(midpoint = 160, mid = "grey80") +
  geom_abline(slope = 0.6221, intercept = 75.6327, color = "red") +
  geom_abline(slope = 1, color = "grey80", linetype = "dashed") +
  theme_minimal(base_size = 18) +
  ggtitle("b) 2017")


#### REPRODUCTIVE OUTPUT ##########################################################

## POLLEN LIMITATION, BY SITE *unable to change grid labels... (F=E, E=M, M=L) *

Biomass %>% 
  filter(Stage != "L") %>%
  ggplot(aes(y=Seed_mass, x=Treatment, fill=as.factor(Year))) +
  geom_boxplot() +
  facet_grid(~Stage) +
  theme_linedraw(base_size = 16) +
  labs(y="Reproductive output", x="Treatment", fill="Year") +
  scale_fill_manual(values=c("#619CFF", "darkorange"))

#2016
PlotA <- Biomass %>% 
  filter(Year == 2016, Stage != "L") %>% 
  ggplot(aes(y=Seed_mass, x=Treatment, fill = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(~Stage) + 
  theme_light(base_size = 16) +
  theme(legend.position="none") +
  ggtitle("a) 2016") +
  labs(x="Treatment", y="Reproductive output", fill="Treatment") +
  scale_fill_manual(values = c("#619CFF", "darkorange"))

#2017
stage_names <- c("F"="E", "E"="M", "M"="L")

PlotB <- Biomass %>% 
  filter(Year == 2017) %>% 
  ggplot(aes(y=Seed_mass, x=Treatment, fill = as.factor(Treatment))) +
  geom_boxplot() +
  facet_wrap(~Stage) + 
  theme_light(base_size = 16) +
  theme(legend.position="none") +
  ggtitle("b) 2017") +
  labs(y="Reproductive output", x="Treatment", fill="Treatment") +
  scale_fill_manual(values = c("#619CFF", "darkorange"))

grid.arrange(PlotA, PlotB)

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

Biomass17 %>% 
  left_join(AllPred, by=c("Site"="siteID", "Stage"="stage")) %>% 
  filter(Plant_type == "C") %>%
  select(Stage, Site, Plant_type, Seed_mass, peak.diff) %>%
  ggplot(aes(y=Seed_mass, x=abs(peak.diff))) +
  geom_jitter() +
  geom_abline(slope = 0.12155, intercept = -1764231, color = "blue") +
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
  ggplot(aes(y = peak.diff, x = sumT.fl, color = stage)) +
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



##### SUM TEMP./PRECIPITATION AND DAYS FROM SM TO PEAK #####################

# Sum temp. and sum precipitation plotted against days from SM to peak flower
sumTP %>% 
  mutate(daysSM = peak.fl-SM) %>% 
  mutate(stage = factor(stage, levels = c("F", "E","M", "L"))) %>% 
  ggplot(aes(x = daysSM, y = sumT.fl, color = stage)) +
  labs(y = "Sum temperature(°C)", x = "Days from snowmelt to peak flowering") +
  geom_jitter() +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme_light(base_size = 16)

sumTP %>% 
  mutate(daysSM = peak.fl-SM) %>%
  mutate(stage = factor(stage, levels = c("F", "E","M"))) %>% 
  ggplot(aes(x = daysSM, y = sumP.fl, color = stage)) +
  geom_jitter() +
  labs(y = "Sum precipitation", x = "Days from snowmelt to peak flowering") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme_light()

# Sum temp. and sum precipitation plotted against days from SM to peak pollinator
sumTP %>% 
  mutate(daysSM = peak.poll-SM) %>% 
  mutate(stage = factor (stage, levels = c("F", "E","M", "L"))) %>% 
  ggplot(aes(x = daysSM, y = sumT.poll, color = stage)) +
  labs(y = "Sum temperature(°C)", x = "Days from snowmelt to peak pollination") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  geom_jitter() +
  theme_light()

sumTP %>% 
  mutate(daysSM = peak.poll-SM) %>% 
  mutate(stage = factor(stage, levels = c("F", "E","M", "L"))) %>% 
  ggplot(aes(x = daysSM, y = sumP.poll, color = stage)) +
  geom_jitter() +
  labs(y = "Sum precipitation", x = "Days from snowmelt to peak pollination") +
  scale_color_manual(labels = c ("E","M", "L"), values=c("#F8766D", "#00BA38", "#619CFF")) +
  theme_light()


##### SUM TEMPERATURE AND NO. FLOWERS THROUGHOUT SEASON ###################
sumT <- Weather %>%
  mutate(temperature = ifelse(temperature<0, 0, temperature)) %>% 
  mutate(temp.sum = cumsum(temperature))

pollination2 %>% 
  mutate(doy = yday(date)) %>%
  filter(year.fl == 2017) %>% 
  left_join(sumT, by = c("doy")) %>% 
  ggplot(aes(x = temp.sum, y = flower.sum)) +
  geom_point()



##### POLLINATOR VISITATION RATE #########################################################

pollination2 %>% 
  mutate(siteID = as.factor(paste(stage, site, sep = " "))) %>%
  filter(year.poll == 2017) %>%
  left_join(Date_snowmelt, by = c("siteID", "stage")) %>%
  ungroup() %>%
  mutate(stage = factor(stage)) %>% 
  #filter(siteID == "F 06") %>% 
  ggplot(aes(x=date, y=std.fly, color = Snowmelt_date)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ stage)

pollination2 %>%
  filter(year.poll==2017) %>%
  mutate(doy=yday(date)) %>% 
  left_join(Weather, by=c("doy")) %>% 
  ggplot(aes(x=temperature, y=std.fly)) +
  geom_point()


#### FLOWERING AND VISITATION RATE ###################################################

# 2016
pollination2 %>% 
  mutate(siteID = paste(stage, site)) %>% 
  filter(year.poll == 2016, stage != "L") %>% 
  ggplot(aes(x = doy, y = fl.sqm, colour=fl.sqm)) +
  geom_point(colour = "#619CFF") +
  geom_smooth(se = FALSE, colour = "#619CFF") +
  labs(y = "No. flowers", color="", x="Day of the year") +
  geom_point(aes(y = (std.fly*1000), color="Pollinator visitation rate")) +
  geom_smooth(aes(y = (std.fly*1000)), se = FALSE, colour = "darkorange") +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Visitation rate"), limits = c(-0.5, 100)) +
  scale_color_manual(labels = c("Pollinator visitation rate","Flowering"), values = c("darkorange", "#619CFF")) +
  facet_wrap(~ stage) +
  theme_light(base_size = 18) +
  theme(legend.position = "bottom")
#theme(legend.position="none")

# 2017
pollination2 %>% 
  mutate(siteID = paste(stage, site)) %>% 
  filter(year.poll == 2017) %>% 
  ggplot(aes(x = doy, y = fl.sqm, colour=fl.sqm)) +
  geom_point(colour = "#619CFF") +
  geom_smooth(se = FALSE, colour = "#619CFF") +
  labs(y = "No. flowers", color="", x="Day of the year") +
  geom_point(aes(y = (std.fly*1000), color="Pollinator visitation rate")) +
  geom_smooth(aes(y = (std.fly*1000)), se = FALSE, colour = "darkorange") +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Visitation rate"), limits = c(-0.5, 100)) +
  scale_color_manual(labels = c("Pollinator visitation rate","Flowering"), values = c("darkorange", "#619CFF")) +
  facet_wrap(~ stage) +
  theme_light(base_size = 18) +
  theme(legend.position = "bottom")
  #theme(legend.position="none")

  
#### FLOWERING AND VISITATION RATE IN SAME PLOT ########################################  

#Flowering and visitation rates for all sites
pollination2 %>%
  mutate(siteID = paste(stage, site)) %>% 
  filter(year.fl == 2017) %>% 
  ggplot(aes(x = doy, y = tot.flowers, colour = siteID)) +
  geom_point()+
  geom_line() + labs(y = "No. flowers", color="", x="Day of the year") +
  geom_point(aes(y = (std.fly*10000), color="Pollinator visitation rate")) +
  geom_line(aes(y= (std.fly*10000), color="Pollinator visitation rate")) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Visitation rate")) +
  theme_light()


# Flowering and mean visitation rate per day for all sites
mean.rate <- pollination2 %>%
  mutate(siteID = paste(stage, site)) %>% 
  group_by(doy) %>% 
  mutate(mean.rate = mean(std.fly))

mean.rate %>%
  mutate(siteID = paste(stage, site)) %>%
  filter(year.fl == 2017) %>% 
  ggplot(aes(x = doy, y = tot.flowers, colour = siteID)) +
  geom_point()+
  geom_line() + labs(y = "No. flowers", color="", x="Day of the year") +
  geom_point(aes(y = (mean.rate*1000000), color="Pollinator visitation rate")) +
  geom_line(aes(y= (mean.rate*1000000), color="Pollinator visitation rate")) +
  scale_y_continuous(sec.axis = sec_axis(~./10000000, name = "Visitation rate")) +
  theme_light()

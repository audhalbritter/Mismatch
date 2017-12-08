source("RanunculusData.R")

#############################################################################################
#### MAKE PDFS FOR PREDICTED VALUES AND DATA ####

# add variable and combine pred flower with insect
pred.pl <- pred.poll %>% 
  mutate(variable = "pollinators", pred = pred*10)

pred <- pred.fl %>% 
  mutate(variable = "flowers") %>% 
  rbind(pred.pl)

# add variable and combine flower and insect data
fl <- dat.fl %>% 
  ungroup() %>% 
  select(year, doy, stage, site, flower.sum) %>% 
  mutate(variable = "flowers") %>% 
  rename(value = flower.sum)

all <- dat.pol %>% 
  select(year, doy, stage, site, fly) %>% 
  mutate(variable = "pollinators", fly = ifelse(year == 2017, 10*fly, fly)) %>% 
  rename(value = fly) %>% 
  rbind(fl) %>% 
  left_join(pred, by = c("site", "stage", "doy", "variable", "year"))


# Function to plot points and curves for predicted values and data
combined <- function(dat){dat %>% 
    ggplot(aes(x = doy, y = value, color = variable)) +
    geom_point() +
    geom_line(aes(y = pred, color = variable)) +
    labs(x = "Day of the year", y = "No. flowers") +
    scale_color_manual(values = c("yellow", "green")) +
    scale_y_continuous(sec.axis= sec_axis(~./10, name="Pollinator visitation rate")) +
    ggtitle(unique(paste(dat$year, dat$stage, dat$site, sep = " "))) +
    theme_minimal()
  }

# Make plots and print PDF
ComboCurves <- all %>% 
  group_by(year, site, stage) %>%
  do(combo.curves = combined(.))
pdf(file = "ComboCurves.pdf")
ComboCurves$combo.curves
dev.off()

#############################################################################################



########## PLOTTING REAL AND PREDICTED DATA ##############

# Flower data
FlowerData <- dat.fl %>% 
  left_join(pred.fl, by = c("site", "stage", "doy"))

FlowerRealAndPred <- function(dat){
  ggplot(dat, aes(x = doy, y = flower.sum)) +
    geom_point() +
    geom_line(aes(y = pred)) +
    ggtitle(unique(paste(dat$stage, dat$site, sep = " ")))
}
FlowerRealAndPred

FlowerCurves <- FlowerData %>% 
  #filter(year(day) == "2017") %>% 
  group_by(site, stage) %>%
  do(flower.curves = FlowerRealAndPred(.))

pdf(file = "FlowerCurves.pdf")
FlowerCurves$flower.curves
dev.off()


# Pollinator data
PollinatorData <- dat.pol %>% 
  #filter(site == "01", stage == "F") %>%
  left_join(pred.poll, by = c("site", "stage", "doy"))

PollRealAndPred <- function(dat){
  ggplot(dat, aes(x = doy, y = fly)) +
    geom_point() +
    geom_line(aes(y = pred)) +
    ggtitle(unique(paste(dat$stage, dat$site, sep = " ")))
}
PollRealAndPred

PollinatorCurves <- PollinatorData %>% 
  #filter(year(day) == "2017") %>% 
  group_by(site, stage) %>%
  do(pollinator.curves = PollRealAndPred(.))

pdf(file = "PollinatorCurves.pdf")
PollinatorCurves$pollinator.curves
dev.off()


### OLD MAPS ONLY DATA
PhenoPollinationMap <- function(df){
  ggplot(df, aes(x = day, y = value, color = observation, shape = observation)) + 
    geom_point() +
    facet_wrap(~ site) +
    theme_minimal() +
    ggtitle(unique(paste(df$stage, df$site, sep = " ")))
}
PhenoPollinationMap

# Combine phenology and pollination (using flowers and pollinators pr. square meter)
AllData <- phenology %>%
  bind_rows(pollination) %>% 
  select(day, site, stage, fl.sqm, poll.sqm) %>%
  gather(key = observation, value = value, fl.sqm, poll.sqm)

## plot maps
Maps2017 <- AllData %>% 
  filter(year(day) == "2017") %>% 
  group_by(site, stage) %>%
  do(pheno.maps = PhenoPollinationMap(.))

pdf(file = "Maps2017.pdf")
Maps2017$pheno.maps
dev.off()



## making plots with 2 y-axes
AllData2 <- pollination %>% 
  select(day, site, stage, poll.sqm) %>% 
  full_join(phenology, by = c("day", "site", "stage"))


## plot maps

PhenoPollinationMap2 <- function(df){
  flowers <- df %>% filter(!is.na(fl.sqm))
  flies <- df %>% filter(!is.na(poll.sqm))
  ggplot(flowers, aes(x = day, y = fl.sqm, colour = "Flowers")) +
    geom_point() +
    geom_line() +
    geom_point(data=flies, aes(y=poll.sqm*20, colour = "Flies")) +
    geom_line(data=flies, aes(y=poll.sqm*20, colour = "Flies")) +
    scale_y_continuous(sec.axis = sec_axis(~./20, name = expression(Pollinators~m^-2))) +
    labs(y=expression(Flowers~m^-2), colour = "", x = "")+
    theme_minimal()
}

## plot maps
Maps2017 <- AllData2 %>% 
  filter(year(day) == "2017") %>% 
  group_by(site, stage) %>%
  do(pheno.maps = PhenoPollinationMap2(.))
Maps2017$pheno.maps[1]
pdf(file = "Maps2017.pdf")
Maps2017$pheno.maps
dev.off()



##### PLOTTING POLLINATION RATE BY TIME (PR. PLOT) ########################

pollination2 %>%
  filter(year.poll == 2017, stage == "F", site == "02") %>%
  mutate(std.fly = std.fly*100000) %>% 
  ggplot(aes(x=day.poll, y=std.fly, color="pollinators")) +
  geom_point() +
  labs(x="date", y="sum of flowers") +
  geom_line(aes(y = tot.flowers, color = "flowers"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~./100000, name = "pollinator visitation rate")) +
  scale_color_manual(labels = c("flowers", "pollinators"), values = c("#FFCC00", "#33CC00")) +
  theme_minimal()

PollRate <- pollination2 %>%
  filter(year.poll == 2017) %>%
  mutate(std.fly = std.fly*100000) 

PollinationRateMap <- function(dat){
    ggplot(dat, aes(x=day.poll, y=std.fly, color="pollinators")) +
    geom_point() +
    labs(x="date", y="sum of flowers") +
    geom_line(aes(y = tot.flowers, color = "flowers"), size=1) +
    scale_y_continuous(sec.axis = sec_axis(~./100000, name = "pollinator visitation rate")) +
    scale_color_manual(labels = c("flowers", "pollinators"), values = c("#FFCC00", "#33CC00")) +
    theme_minimal() +
    ggtitle(unique(paste(dat$stage, dat$site, sep = " ")))
}
PollinationRateMap

PollinationRateCurves <- PollRate %>%
  group_by(site, stage) %>%
  do(pollrate.curves = PollinationRateMap(.))

pdf(file = "PollinationRateCurves.pdf")
PollinationRateCurves$pollrate.curves
dev.off()


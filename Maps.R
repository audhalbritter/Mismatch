# Combine phenology and pollination
AllData <- pollination %>% 
  select(day, site, stage, fly) %>% 
  full_join(phenology, by = c("day", "site", "stage")) %>% 
  gather(key = observation, value = value, -day, -site, -stage)


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


AllData %>%
  filter(site == "01", stage == "F") %>% 
  ggplot(aes(x = day, y = value, color = observation, shape = observation)) +
  geom_point() +
  facet_wrap(~ site) +
  theme_minimal() 
  ggtitle(unique(paste(stage, site, sep = " "))) #Error in paste(stage, site, sep = " ") : object 'stage' not found

## making plots with 2 y-axes
AllData2 <- pollination %>% 
  select(day, site, stage, poll.sqm) %>% 
  full_join(phenology, by = c("day", "site", "stage"))
  
  
p <- AllData2 %>% 
  filter(site == "01", stage == "E", day > "2017-01-01") %>% 
  ggplot(aes(x = day, y = fl.sqm, colour = "Flowers")) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=poll.sqm*20, colour = "Flies")) +
  geom_line(aes(y=poll.sqm*20, colour = "Flies")) +
  scale_y_continuous(sec.axis = sec_axis(~./20, name = expression(Pollinators~m^-2))) +
  labs(y=expression(Flowers~m^-2), colour = "", x = "")+
  theme_minimal()
p

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

##### MAPS, USING PREDICTED VALUES

pred.both <- pred.fl %>% 
  rename(pred.fl=pred) %>% 
  left_join(pred.poll, by=c("site"="site", "stage"="stage", "doy"="doy")) %>% 
  rename(pred.poll=pred)


PhenoPollMapPredicted <- function(df){
  flowers <- df %>% filter(!is.na(pred.fl))
  flies <- df %>% filter(!is.na(pred.poll))
  ggplot(flowers, aes(x = doy, y = pred.fl, colour = "Flowers")) +
    geom_point() +
    geom_line() +
    geom_point(data=flies, aes(y=pred.poll, colour = "Flies")) +
    geom_line(data=flies, aes(y=pred.poll, colour = "Flies")) +
    scale_y_continuous(sec.axis = sec_axis(~./20, name = expression(Pollinators~m^-2))) +
    labs(y=expression(Flowers~m^-2), colour = "", x = "")+
    theme_minimal()
}

MapsPred2017 <- pred.both %>% 
  group_by(site, stage) %>%
  do(pred.maps = PhenoPollMapPredicted(.))
MapsPred2017$pred.maps[1]
pdf(file = "MapsPred2017.pdf")
MapsPred2017$pred.maps
dev.off()



dat %>% 
  left_join(pred.fl, by = c("site", "stage", "doy")) %>% 
  filter(site == "01", stage == "F") %>% 
  ggplot(aes(x = doy, y = flower.sum)) +
  geom_point() +
  geom_line(aes(y = pred))

##### PHENOLOGY ######

#### LIBRARIES
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### READ IN DATA ####


# PHENOLOGY
pheno <- read.csv("RANfenologi.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
pheno <- as.data.frame(t(pheno), stringsAsFactors = FALSE) # transpose data
names(pheno) <- pheno[1,] # first column = name
pheno <- pheno[-1,] # remove first column
pheno$date <- dmy_hm(paste(pheno$Dato, pheno$Tid))
pheno$Dato <- NULL
pheno$Tid <- NULL
pheno2 <- gather(pheno, key = "plot", value = "flowering", -date, -Vaer, -Hvem) %>% 
  mutate(flowering = as.numeric(flowering)) %>% 
  mutate(stage = factor(substring(plot, 1,1))) %>% 
  mutate(site = factor(substring(plot, 2,3))) %>% 
  mutate(plot = factor(substring(plot, 4,4))) %>% 
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>% 
  mutate(day = format(as.Date(date,format="%Y-%m-%d"))) %>%
  filter(!is.na(flowering)) # remove NA


# POLLINATOR OBSERVATIONS
polli <- read.csv("RanunculusPollinator.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)
pollinator <- polli %>%
  filter(!Tid == "") %>% # slette alle koloner med Na
  mutate(date = dmy_hm(paste(Dato, Tid))) %>%# lime sammen dato å tid
  mutate(fly = as.numeric(Fluer)) %>%
  mutate(stage = substring(Site, 1,1), site = substring(Site, 2,3)) %>%# lage to nye variabler, stage å site
  select(-Tid, -Fluer, -Site, -Dato) %>% # sletter her koloner ikke rekker, - betyr ta vekk
  mutate(stage = factor(stage, levels = c("E", "M", "L"))) %>%  # bestemme rekkefölgen for en faktor
  mutate(minutes = (floor(minute(date)/10)*10)) %>%
  mutate(date = ymd_hm(paste0(format(date, "%Y-%m-%d %H:"), minutes))) %>% # making 10 minutes steps
  left_join(Temperature, by = c("date" = "date", "stage" = "stage", "site" = "site")) %>% 
  mutate(sol.og.sky = plyr::mapvalues(sol.og.sky, c("overskyet","overskyet_littsol","sol_littsky","sol"), c("overcast","cloudy","cloud_sun","sun"))) %>% 
  mutate(sol.og.sky = factor(sol.og.sky, levels = c("overcast","cloudy","cloud_sun","sun"))) %>% 
  mutate(site = factor(site)) %>% 
  mutate(day = format(as.Date(date,format="%Y-%m-%d"))) %>%
  select(-minutes.x, -minutes.y)



# Calculate and plot mean nr of flowers per site
fl <- pheno2 %>%
  group_by(stage, day, site) %>%
  summarise(n = n(), nrflower = sum(flowering)) %>% 
  select(-n)

dat <- pollinator %>%
  group_by(stage, day, site) %>%
  summarise(n = n(), nrvisit = mean(fly)) %>%
  select(-n) %>% 
  left_join(fl, by = c("day" = "day", "stage" = "stage", "site" = "site"))


# Plot flowering and visits together

# Early
dat %>%
  filter(stage == "E") %>% 
  ggplot() +
  geom_point(aes(x = day, y = nrflower), color = "red") +
  geom_point(aes(x = day, y = nrvisit)) +
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
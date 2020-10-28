##### PHENOLOGY ######

#### LIBRARIES
install.packages("tidyr")
install.packages("lubridate")
install.packages("ggplot2")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### READ IN DATA ####
getwd()
setwd("C:/Users/linn/Dropbox/mismatch/Data")

# FLOWERS
flower <- read.csv("RANfenologi.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
flower <- as.data.frame(t(flower), stringsAsFactors = FALSE) # transpose data
names(flower) <- flower[1,] # first column = name
flower <- flower[-1,] # remove first column
flower$date <- dmy_hm(paste(flower$Dato, flower$Tid))
flower$Dato <- NULL
flower$Tid <- NULL
flower2 <- gather(flower, key = "plot", value = "flowering", -date, -Vaer, -Hvem)
flower2$flowering <- as.numeric(flower2$flowering)
flower2$stage <- substring(flower2$plot, 1,1)
flower2$site <- substring(flower2$plot, 2,3)
flower2$plot <- substring(flower2$plot, 4,4)


# POLLINATOR OBSERVATIONS
polli <- read.csv("RANpollinator.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
names(polli) <- polli[1,] # first column = name
polli <- polli[-1,] # remove first column
polli <- rbind(polli[,1:6], polli[,7:12], polli[,13:18], polli[,19:24], polli[,25:30])
polli$date <- dmy_hm(paste(polli$Dato, polli$Tid))
polli$insect <- as.numeric(polli$Fluer)
polli$stage <- substring(polli$Site, 1,1)
polli$site <- substring(polli$Site, 2,3)
polli$Dato <- NULL
polli$Tid <- NULL
polli$Fluer <- NULL
polli$Site <- NULL
polli$Andre <- NULL # remove column Andre for the moment


#### PLOT DATA ####
setwd("C:/Users/linn/Dropbox/mismatch/Analysis/Mismatch")
# Calculate and plot mean nr of flowers per site
fl <- flower2 %>%
  filter(!is.na(flowering)) %>%
  filter(stage == "E") %>%
  group_by(site, date) %>%
  summarise(n = n(), nrflower = mean(flowering)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = nrflower, color = site)) +
  facet_wrap(~site)

# Calculate and plot mean nr of visits per site
pol <- polli %>%
  filter(!is.na(insect)) %>%
  filter(!is.na(date)) %>%
  filter(stage == "E") %>%
  #filter(site == "07") %>%
  group_by(site, date) %>%
  summarise(n = n(), nrvisit = mean(insect)) %>%
  ggplot() + 
  geom_point(aes(x = date, y = nrvisit, color = site)) +
  facet_wrap(~site)

# Plot flowering and visits together
ggplot() +
  geom_line(data = fl, aes(x = date, y = nrflower, color = site)) +
  geom_point(data = pol, aes(x = date, y = nrvisit, color = site), shape = 17) +
  facet_wrap(~site) +
  ylab("Nr of flowers / visits") +
  ggtitle("Site") +
  theme(legend.position="none")

  


##### PHENOLOGY ######

#### LIBRARIES
install.packages("tidyr")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### READ IN DATA ####
getwd()
setwd("/Users/audhalbritter/Dropbox/mismatch/Data")

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





#### PLOT DATA ####
setwd("/Users/audhalbritter/Dropbox/mismatch/Analysis/Mismatch")
# Calculate and plot mean nr of flowers per site
fl <- flower2 %>%
  filter(!is.na(flowering)) %>%
  #filter(stage == "M") %>%
  group_by(stage, date) %>%
  summarise(n = n(), nrflower = mean(flowering)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = nrflower, color = site)) +
  facet_wrap(~site)



# Plot flowering and visits together
ggplot() +
  geom_line(data = fl, aes(x = date, y = nrflower)) +
  geom_point(data = pol, aes(x = date, y = nrvisit), shape = 17) +
  facet_wrap(~stage) +
  ylab("Nr of flowers / visits") +
  ggtitle("Site") +
  theme(legend.position="none")





### LOAD POLLINATOR DATA

#### LIBRARIES
install.packages("tidyr"); install.packages("dplyr"); install.packages("lubridate"); install.packages("ggplot2")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

# POLLINATOR OBSERVATIONS
setwd("/Users/audhalbritter/Dropbox/mismatch/Data")
polli <- read.csv("RANpollinator.csv", header = FALSE, sep = ";", stringsAsFactors=FALSE)
names(polli) <- polli[1,] # first column = name
polli <- polli[-1,] # remove first column
polli2 <- rbind(polli[,1:6], polli[,7:12], polli[,13:18], polli[,19:24], polli[,25:30])
polli2 <- polli2[!(polli2$Tid==""), ] # remove empty rows
polli2$date <- dmy_hm(paste(polli2$Dato, polli2$Tid)) #  merge date and time
polli2$insect <- as.numeric(polli2$Fluer) # make numeric
polli2$stage <- substring(polli2$Site, 1,1) # split site into stage and site
polli2$site <- substring(polli2$Site, 2,3)
polli2$Dato <- NULL # remove
polli2$Tid <- NULL
polli2$Fluer <- NULL
polli2$Site <- NULL

# Calculate and plot mean nr of visits per site
pol <- polli2 %>%
  filter(stage == "E") %>%
  #filter(site == "07") %>%
  group_by(stage, date, site) %>%
  summarise(n = n(), nrvisit = mean(insect)) %>%
  ggplot() + 
  geom_point(aes(x = date, y = nrvisit, color = site)) +
  facet_wrap(~site)
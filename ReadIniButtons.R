### Read in iButtons

library("lubridate")
library("tidyr")
library("dplyr")
library("ggplot2")

#  READ IN ITAS LOGGERS
ReadIniButtons <- function(textfile){
  # import body of data
  dat <- read.table(textfile, header=FALSE, sep=",", comment.char = "", stringsAsFactors = FALSE)
  dat$date <- dmy_hms(dat$V1) # convert to date
  dat$temperature <- as.numeric(paste(dat$V3, dat$V4, sep = ".")) # paste temp values with . in between
  dat <- dat[,-c(1:4)] # remove unit column 
  
  # extract site name from file name
  textfile2 <- basename(textfile)
  dat$stage <- substr(textfile2,1,1)
  dat$site <- substr(textfile2,2,3)
  dat
}

# MAKE LIST OF ALL TXT FILES AND MERGE THEM TO ONE DATA FRAME
myfiles <- dir(path = paste0("Data/2016/iButtons"), pattern = "txt", recursive = TRUE, full.names = TRUE)
mdat <- plyr::ldply(as.list(myfiles), ReadIniButtons)

# Rename stage
Temperature <- mdat %>% 
  mutate(stage = plyr::mapvalues(stage, c("e", "l", "m"), c("early", "late", "mid"))) %>% # rename stage
  mutate(stage = factor(stage, levels = c("early", "mid", "late"))) # change order of levels

# Remove first observations
Temperature <- Temperature[!(Temperature$stage == "early" & Temperature$date < as.POSIXct(ymd("2016-06-19"))), ] # remove < 19.6
Temperature <- Temperature[!(Temperature$stage == "mid" & Temperature$date < as.POSIXct(ymd("2016-07-02"))), ] # remove < 2.7
Temperature <- Temperature[!(Temperature$stage == "mid" & Temperature$site %in% c("05", "06") & Temperature$date < as.POSIXct(ymd_hms("2016-07-02 12:00:00"))), ] # for site 5 and 6 2.7 at 12:00
Temperature <- Temperature[!(Temperature$stage == "late" & Temperature$date < as.POSIXct(ymd("2016-07-15"))), ] # remove < 15.7
  
# SAVE THE FILE
setwd("/Users/audhalbritter/Dropbox/mismatch/Analysis/Mismatch")
save(Temperature, file = "TemperatureiButton.RData")


ggplot(Temperature, aes(x = date, y = temperature)) +
  geom_line() +
  facet_wrap(~ site)

# to check things
mdat %>%
  mutate(stage = plyr::mapvalues(stage, c("e", "l", "m"), c("early", "late", "mid"))) %>% # rename stage
  mutate(stage = factor(stage, levels = c("early", "mid", "late"))) %>% # change order of levels
  filter(stage == "late") %>% 
  filter(date < as.POSIXct(ymd("2016-07-17"))) %>%
  ggplot() +
  geom_line(aes(x = date, y = temperature)) +
  facet_wrap(~ site)


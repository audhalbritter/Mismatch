
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
  geom_point() +
  theme_minimal()

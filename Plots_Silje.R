
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


### BY DATE OF SNOWMELT ###



# Making plot showing peak difference by date of snowmelt

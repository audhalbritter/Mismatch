### CALCULATE FIRST AND PEAK FLOWERING AND INSECT OBSERVATION

peak.fl <- phenology %>% 
  mutate(doy = yday(day)) %>% 
  group_by(year, stage, site) %>%   # group by year, stage and site to calculate first and peak
  summarize(first = first(doy), peak = doy[which.max(fl.sqm)]) %>% 
  rename(peak.fl = peak)
#mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
#mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
#group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups

View(peak.fl)

peak.pol <- pollination %>% 
  select(poll.sqm, year, stage, site, date) %>% 
  group_by(year, stage, site)%>% 
  mutate(doy = yday(date)) %>% 
  summarize(first = first(doy), peak = doy[which.max(poll.sqm)]) %>% 
  rename(peak.pol = peak)

View(peak.pol)

### JOIN PHENOLOGY AND POLLINATION ####
# Find closest phenology observation to each pollination observation
pollination2 <- pollination %>% 
  full_join(phenology, by = c("site", "stage"), suffix = c(".poll",".fl")) %>% 
  select(-weather, -wind, -remark) %>% 
  mutate(diff = day.poll - day.fl, abs.diff = abs(diff)) %>% 
  mutate(abs.diff.mult = if_else(diff > 0, abs.diff * 1.9, abs.diff)) %>% 
  group_by(day.poll, stage, site) %>% 
  slice(which.min(abs.diff.mult)) %>% 
  mutate(flowering = ifelse(abs.diff > 3, NA, flowering)) %>% # could check how much different flowers are
  mutate(tot.flowers = flowering*2*area) %>% # added new column: total number of flowers pr. area (based on mean flowers)
  mutate(std.fly = fly/tot.flowers) # standardize insect observation by fl per area


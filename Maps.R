pheno2 <- phenology %>%
  select(day, site, stage, plot, flowering)

pollination %>% 
  select(day, site, stage, fly) %>% 
  full_join(pheno2, by = c("day", "site", "stage")) %>% 
  gather(key = observation, value = value, -day, -site, -stage, -plot) %>% 
  filter(year(day) == "2017", stage == "M") %>% 
  ggplot(aes(x = day, y = value, colour = observation, shape = observation)) +
  geom_point() +
  facet_wrap(~ site)

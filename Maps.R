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

# Combine phenology and standardized insect visitation
AllData <- phenology %>%
  select(day, site, stage, flowering) %>%
  left_join(pollination2, by = c("day" = "day.poll", "site", "stage", "flowering")) %>%
  gather(key = observation, value = value, flowering, std.fly)

## plot maps
Maps2017 <- AllData %>% 
  filter(year(day) == "2017") %>% 
  group_by(site, stage) %>%
  do(pheno.maps = PhenoPollinationMap(.))

pdf(file = "Maps2017.pdf")
Maps2017$pheno.maps
dev.off()


AllData %>%
  filter(site == "01", stage == "F")
  ggplot(df, aes(x = day, y = value, color = observation, shape = observation)) 
  geom_point() +
  facet_wrap(~ site) +
  theme_minimal()
  ggtitle(unique(paste(df$stage, df$site, sep = " ")))
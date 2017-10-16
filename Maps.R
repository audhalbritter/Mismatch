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
  
## plot maps
Maps2017 <- AllData %>% 
  filter(year(day) == "2017") %>% 
  group_by(site, stage) %>% 
  do(pheno.maps = PhenoPollinationMap(.))

pdf(file = "Maps2017.pdf")
Maps2017$pheno.maps
dev.off()

MeanVisitRate <- pollination2 %>% 
  select(day.poll, year.poll, stage, site, tot.flowers, std.fly) %>% 
  group_by(year.poll, stage, site) %>% 
  summarise(mean.visit.rate = mean(std.fly), mean.tot.flowers = mean(tot.flowers)) %>% 
  rename(Year = year.poll, Stage = stage, Site = site) %>% 
  left_join(Biomass, by = c("Year", "Stage", "Site"))


pollination2 %>% 
  select(fly, tot.flowers, std.fly) %>% 
  filter(std.fly == Inf)


Biomass <- Biomass %>% 
  mutate(BlockID = as.factor(paste(Stage, Site, Block))) %>% #gir block en mer presis id
  filter(!is.na(Seed_mass)) %>%  #fjerner alle NA 
  filter(!is.na(Biomass))

Biomass %>%
  split(.$Year) %>% # from base R
  map(~ lm(log(Seed_mass) ~ Biomass*Stage, random = ~ 1 | BlockID, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")
lmm1 <- lme(log(Seed_mass) ~ Biomass*Stage, random = ~ 1 | BlockID, data = Biomass %>% filter(Year == 2016))


BiomassResult <- Biomass %>% 
  group_by(Year) %>% 
  do(fit = lme(log(Seed_mass) ~ Biomass*Stage, random = ~ 1 | BlockID, data = .))
tidy(x = BiomassResult, object = fit, effects = "fixed") %>% 
  mutate(estimate = round(exp(estimate), 2), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 2)) %>% 
  mutate(significant = ifelse(p.value < 0.05, "*", ""))


lmm1 <- lme(Reaction ~ Days, random=~ Days|Subject, sleepstudy)
tidy(lmm1)
tidy(lmm1, effects = "fixed")
glance(lmm1)


### Fix nedbor

### Make a map
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

library("raster")
library("tidyverse")
library("ggthemes")
library("grid")
library("ggsn")

### COORDINATES FIELD SITES
coords <- read_csv(file = "Data/Finse 2017.csv", col_names = TRUE, skip = 28)

coords %>% 
  select(lat, lon, ele, time, name) %>% 
  filter(name) # names that start with one letter then 2 digits
  distinct(name) %>% pn


  # GONGGA MOUNTAIN MAP
  # Read data with raster
  
  #files <- list.files(path = "map/data/", pattern='\\.bil$', recursive = TRUE, full.names = TRUE)
  
  files <- list.files(path = "/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/TransplantChina/Map", pattern='\\.bil$', recursive = TRUE, full.names = TRUE)
  
  f1 <- raster(files[1])
  f2 <- raster(files[2])
  
  gongga.spdf <- as(f1, "SpatialPixelsDataFrame")
  gongga.df <- as.data.frame(gongga.spdf)
  gongga2.spdf <- as(f2, "SpatialPixelsDataFrame")
  gongga2.df <- as.data.frame(gongga2.spdf)
  name <- c("elev", "x", "y")
  elev.gongga <- rbind(setNames(gongga.df, name), setNames(gongga2.df, name))
  
  # Crop
  gongga <- elev.gongga %>% filter(x > 102, x < 102.05, y > 29.825, y < 29.92)
  
  

  
  # SpatialLinesDataFrame object
  hw_lines <- as_sp(ways, "lines")  
  #fortify(hw_lines) %>%
  save(gongga, file = "gongga.RData")
  load(file = "gongga.RData")
  
  
  # scalebar
  bb2 <- data.frame(long = c(102, 102.05), lat = c(29.825, 29.92))
  
  GonggaMap <- fortify(hw_lines) %>%
    ggplot() +
    geom_raster(data = gongga, aes(x=x, y=y, fill = elev)) +
    geom_path(aes(x = long, y = lat, group = id), color = "white") +
    coord_equal() +
    scale_fill_gradient(name = "Elevation", low = "grey0", high = "grey100") + 
    geom_point(aes(x=long, y=lat), colour  = "red", shape = 17, data = coords, size=3) +
    scale_x_continuous(expand = c(0,0), limits = c(102, 102.05), breaks = scales::pretty_breaks(n = 2)) +
    scale_y_continuous(expand = c(0,0), limits = c(29.825, 29.92), breaks = scales::pretty_breaks(n = 2)) +
    #annotate("text", x = 102.002, y = 29.945, label = "A)", size= 5, color = "white") +
    labs(x = "", y = "") +
    theme(text = element_text(size=12), legend.justification = c(1,0)) +
    scalebar(bb2, dist = 1, dd2km = TRUE, model  = "WGS84", location = "bottomright", st.size = 3, anchor = c(x = bb2$long[1] + 0.55 * (bb2$long[2] - bb2$long[1]), y = bb2$lat[1] + 0.08 * (bb2$lat[2] - bb2$lat[1])))
  
  ggsave("GonggaMap.pdf", path = "~/Dropbox/Bergen/China/Master thesis _Li/Phenology/PhenologyLi", width = 6, height = 6)
  
save(gongga, file = "gongga.RData")

##### DATA ANALYSIS CODE ######
# This is also the code to make the figures for the plant-pollinator interaction part from Roos et al. 20XX.

# import data and calculate day of peak flowering and pollinaton
source("R/1_Import_RanunculusData.R")
source("R/2_PeakPredicted.R")

### LIBRARIES
library("nlme")


##### PEAK FLOWER/ PEAK POLLINATORS ################################################

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


Peak_Flower_Poll_Figure <- AllPred %>%
  filter(stage != "L") %>% 
  ggplot(aes(x = peak.fl, y = peak.poll, colour = stage)) +
  labs(x = "Day of peak flowering", y = "Day of peak pollinator visitation", color = "Snowmelt stage") +
  geom_smooth(method = lm, se = FALSE, colour = "black", size = 0.8) +
  geom_abline(slope = 1, color = "grey40", size = 1, linetype = "dashed") +
  geom_point(aes(colour = factor(stage), shape = factor(stage)), size = 2.5) +
  scale_color_manual(labels = c ("early","mid", "late"), values=cbbPalette[c(3,7,4)], name = "snowmelt stage") +
  scale_shape_manual(labels = c ("early","mid", "late"), values = c(15,16,17), name = "snowmelt stage") +
  theme_light(base_size = 18) +
  facet_wrap(~year) +
  theme(legend.position = "bottom", legend.title=element_text(size=12), legend.text=element_text(size=12), strip.text.x = element_text(size = 18))

#ggsave(Peak_Flower_Poll_Figure, filename = "Peak_Flower_Poll_Figure.jpeg", dpi = 300)


##### POLLINATION VISITATION RATE ##################################################

fp <- pollination2 %>% 
  ungroup() %>%
  mutate(fly=fly*5) %>% 
  select(year.poll, stage, fly, fl.sqm, doy) %>% 
  gather(key = category, value = value, -year.poll, -stage, -doy) %>% 
  mutate(Flowering = value) %>% 
  filter(year.poll == 2017) %>% 
  ggplot(aes(x = doy, y = Flowering, colour=category, shape = category)) +
  #geom_point(size = 1.5) +
  scale_shape_manual(labels = c("Flowering","Pollinator activity"), values = c(17,16), name = "") +
  scale_color_manual(labels = c("Flowering","Pollinator activity"), values = cbbPalette[c(3,7)], name = "", guide = guide_legend(override.aes = list(linetype = c(rep("blank"))))) +
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  geom_smooth(se = FALSE) +
  labs(y = "Number of flowers", color="", x="") +
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Number of visits")) +
  facet_wrap(~ stage, labeller = labeller(stage = as_labeller(c("F"="Early", "E" = "Mid", "M" = "Late")))) +
  theme_light(base_size = 16) +
  theme(legend.position = "top", axis.text.x = element_blank()) +
  scale_colour_manual(name=" ", values=cbbPalette[c(3,7)], labels = c("Pollinators", "Flowers"))

# Figure 2b
pv <- pollination2 %>% 
  filter(year.poll == 2017) %>% 
  ggplot(aes(doy, std.fly)) +
  geom_point(colour = "#009E73") +
  geom_smooth(se = FALSE, colour = "#009E73") +
  facet_wrap(~stage, labeller = labeller(stage = as_labeller(c("F"="Early", "E" = "Mid", "M" = "Late")))) +
  labs(y = "Visitation rate", x = "Day of the year") +
  theme_light(base_size = 16)

# Figure 2 complete
plot_grid(fp, pv, ncol = 1, labels = c("a)", "b)"), align = "v")



#### REPRODUCTIVE OUTPUT ##########################################################

## Reproductive output by treatment
Biomass %>% 
  filter(Stage != "L") %>%
  ggplot(aes(y=Seed_mass, x=Treatment, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(Year~Stage, labeller = labeller(Stage = as_labeller(c("F" = "Early", "E" = "Mid", "M" = "Late")))) +
  theme_light(base_size = 16) +
  labs(y="Reproductive output", x="", fill="") +
  scale_fill_manual(values=cbbPalette[c(7,3)]) +
  guides(fill = FALSE)

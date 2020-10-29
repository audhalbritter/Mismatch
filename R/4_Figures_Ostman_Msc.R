##### DATA ANALYSIS CODE ######
# This is the code to make the figures for Silje Andrea Hjortland Östman's master thesis (University of Bergen), 2018, Plant-pollinator interactions in the alpine: Landscape heterogeneity acts as a potential buffer against climate-change induced mismatch in the pollinator-generalist Ranunculus acris

# import data and calculate day of peak flowering and pollinaton
source("R/1_Import_RanunculusData.R")
source("R/2_PeakPredicted.R")
source("R/SumP_sumT.R")

library("gridExtra")
library("broom")
library("gtable")
library("ggpubr")
library("cowplot")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## FIGURE 1 ##############################################################
# (peak vs. peak + theoretical interaction curves)

# Figure 1a
pp <- AllPred %>%
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

# Figure 1a with label ("a)")
pp1 <- ggdraw(pp) +
  draw_label("a)", x = 0.07, y = 0.97, size = 17, hjust = 1)

# Figure 1b
T1 <- ggplot(data.frame(x = c(-10, 18)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 4), col="black", size = 1) +
  stat_function(fun = dnorm, args = list(mean = 5, sd = 4), col="black", size = 1, linetype = "dotted") +
  annotate("text", y = 0.095, x = -9.5, label = "b)", size = 6) +
  theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Figure 1c
T3 <- ggplot(data.frame(x = c(-10, 18)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 4), col="black", size = 1, linetype = "dotted") +
  stat_function(fun = dnorm, args = list(mean = 5, sd = 4), col="black", size = 1, ) +
  annotate("text", y = 0.095, x = -12.5, label = "d)", size = 6) +
  theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Figure 1d
T2 <- ggplot(data.frame(x = c(-13, 15)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 4), col="black", size = 1) +
  stat_function(fun = dnorm, args = list(mean = 0.5, sd = 4), col="black", size = 1, linetype = "dotted") +
  annotate("text", y = 0.095, x = -12.5, label = "c)", size = 6) +
  theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Figure 1b-d legend
T3L <- ggplot(data.frame(x = c(-10, 18)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 4), col="black", size = 1, aes(linetype = "pollinators")) +
  stat_function(fun = dnorm, args = list(mean = 5, sd = 4), col="black", size = 1, aes(linetype = "flowering")) +
  theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size = 12)) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(x = "time")

# legend
leg <- get_legend(T3L)

# Figure 1b-d combined
types <- grid.arrange(T1, T2, T3, leg, ncol = 1, heights=c(1, 1, 1, 0.4)) 

# Figure 1 (a-d) complete
grid.arrange(pp1, types, ncol = 2, nrow = 1, widths = c(2,1))


# FIGURE 2 ###############################################################
#(flowering and pollinator activity curves + visitation rate)

# Figure 2a
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

#### FIGURE 3#############################################################
# (reproductive output)

# Figure for paper
Biomass %>% 
  filter(Stage != "L") %>%
  ggplot(aes(y=Seed_mass, x=Treatment, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(Year~Stage, labeller = labeller(Stage = as_labeller(c("F" = "Early", "E" = "Mid", "M" = "Late")))) +
  theme_light(base_size = 16) +
  labs(y="Reproductive output", x="", fill="") +
  scale_fill_manual(values=cbbPalette[c(7,3)]) +
  guides(fill = FALSE)



##### FIGURE 4 ###########################################################
# (precipitation 2017)
# 2017
sumP17 <- sumTP %>%
  filter(year == 2017) %>% 
  ggplot(aes(x = stage, y = sumP.fl, fill = stage)) +
  geom_boxplot() +
  scale_fill_manual(values=cbbPalette[c(7,3,5)]) +
  labs(y="Cumulative precipitation (mm)", x="", fill="") +
  #ggtitle("Sum precipitation pr stage, 2017") +
  scale_x_discrete(labels=c("Early","Mid","Late")) +
  guides(fill = FALSE) +
  theme_minimal(base_size = 18)


# FIGURE I (Appendix) #################################################### # (summer temperatures and duration of flowering for all stages)

# Figure Ia 
tf16 <- Weather %>% 
  filter(year == 2016) %>% 
  ggplot(aes(x = doy, y = temperature)) +
  geom_rect(aes(xmin=169, xmax=203, ymin=0, ymax=1, fill = "mid"), colour = NA, alpha = 0.01) +
  geom_rect(aes(xmin=186, xmax=210, ymin=1, ymax=2, fill = "late"), colour = NA, alpha = 0.01) +
  geom_line(colour = "black") +
  xlab(" ") + ylab("Temperature (°C)") +
  scale_fill_manual("Stage", values =c("#F0E442","#56B4E9"), labels = c("Mid", "Late"), guide=guide_legend(override.aes = list(alpha = 0.5)), breaks = c("mid", "late")) +
  scale_y_continuous(minor_breaks = seq(0, 15, 2.5), breaks = seq(0, 15, 5)) +
  ggtitle("a) 2016") +
  theme_minimal() + theme(legend.position = "none")

# Figure Ib
tf17 <- Weather %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = doy, y = temperature)) +
  geom_rect(aes(xmin=162, xmax=203, ymin=0, ymax=1, fill = "early"), alpha=0.01) +
  geom_rect(aes(xmin=175, xmax=211, ymin=1, ymax=2, fill = "mid"), alpha = 0.01) +
  geom_rect(aes(xmin=184, xmax=219, ymin=2, ymax=3, fill = "late"), alpha = 0.01) +
  geom_line(colour = "black") +
  xlab("Day of the year") + ylab("Temperature (°C)") +
  scale_fill_manual("Stage", values =c("#D55E00", "#F0E442", "#56B4E9"), labels = c("Early", "Mid", "Late"), guide=guide_legend(override.aes = list(alpha = 0.4)), breaks=c("early", "mid", "late")) +
  scale_y_continuous(minor_breaks = seq(0, 15, 2.5), breaks = seq(0, 15, 5)) +
  ggtitle("b) 2017") +
  theme_minimal() +
  theme(legend.position = "none")

tf17Leg <- Weather %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = doy, y = temperature)) +
  geom_rect(aes(xmin=162, xmax=203, ymin=0, ymax=1, fill = "early"), alpha=0.01) +
  geom_rect(aes(xmin=175, xmax=211, ymin=1, ymax=2, fill = "mid"), alpha = 0.01) +
  geom_rect(aes(xmin=184, xmax=219, ymin=2, ymax=3, fill = "late"), alpha = 0.01) +
  geom_line(colour = "black") +
  xlab("Day of the year") + ylab("Temperature (°C)") +
  scale_fill_manual("Snowmelt stage", values =c("#D55E00", "#F0E442", "#56B4E9"), labels = c("Early", "Mid", "Late"), guide=guide_legend(override.aes = list(alpha = 0.4)), breaks=c("early", "mid", "late")) +
  scale_y_continuous(minor_breaks = seq(0, 15, 2.5), breaks = seq(0, 15, 5)) +
  ggtitle("b) 2017") +
  theme_minimal() +
  theme(legend.position = "bottom")

tfLeg <- get_legend(tf17Leg)  

# Figure I complete
grid.arrange(tf16, tf17, tfLeg, ncol = 1, heights = c(1, 1, 0.2), top = "Temperature, June-August")


# FIGURE II (Appedix)#####################################################
# (summer precipitation and duration of flowering for all stages)

# Figure IIa
pf16 <- Weather %>% 
  filter(year == 2016) %>% 
  ggplot(aes(x = doy, y = precipitation)) +
  geom_rect(aes(xmin=169, xmax=203, ymin=0, ymax=3, fill = "mid"), alpha = 0.01) +
  geom_rect(aes(xmin=186, xmax=231, ymin=3, ymax=6, fill = "late"), alpha = 0.01) +
  geom_line(colour = "black") +
  xlab(" ") + ylab("Precipitation (mm)") +
  scale_fill_manual("Stage", values =c("#F0E442","#56B4E9"), guide=guide_legend(override.aes = list(alpha = 0.5)), breaks = c("mid", "late")) +
  scale_y_continuous(limits = c(0, 45), minor_breaks = seq(0, 40, 10)) +
  ggtitle("a) 2016") +
  theme_minimal() +
  theme(legend.position = "none")

# Figure IIb
pf17 <- Weather %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = doy, y = precipitation)) +
  geom_rect(aes(xmin=162, xmax=203, ymin=0, ymax=3, fill = "early")) +
  geom_rect(aes(xmin=175, xmax=211, ymin=3, ymax=6, fill = "mid")) +
  geom_rect(aes(xmin=184, xmax=219, ymin=6, ymax=9, fill = "late")) +
  geom_line(colour = "black") +
  xlab("Day of the year") + ylab("Precipitation(mm)") +
  scale_fill_manual("Stage", values =c("#D55E00", "#F0E442", "#56B4E9"), guide=guide_legend(breaks=c("early", "mid", "late"))) +
  scale_y_continuous(limits = c(0, 45), minor_breaks = seq(0, 40, 10)) +
  ggtitle("b) 2017") +
  theme_minimal() + theme(legend.position = "none")

pf17Leg <- Weather %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = doy, y = precipitation)) +
  geom_rect(aes(xmin=162, xmax=203, ymin=0, ymax=3, fill = "early")) +
  geom_rect(aes(xmin=175, xmax=211, ymin=3, ymax=6, fill = "mid")) +
  geom_rect(aes(xmin=184, xmax=219, ymin=6, ymax=9, fill = "late")) +
  geom_line(colour = "black") +
  xlab("Day of the year") + ylab("Precipitation(mm)") +
  scale_fill_manual("Stage", values =c("#D55E00", "#F0E442", "#56B4E9"), labels = c("Early", "Mid", "Late"), guide=guide_legend(breaks=c("early", "mid", "late"))) +
  scale_y_continuous(limits = c(0, 45), minor_breaks = seq(0, 40, 10)) +
  ggtitle("b) 2017") +
  theme_minimal() + theme(legend.position = "bottom")

pfLeg <- get_legend(pf17Leg)

# Figure II complete
grid.arrange(pf16, pf17, pfLeg, ncol = 1, heights = c(1, 1, 0.2), top = "Precipitation, June-August")

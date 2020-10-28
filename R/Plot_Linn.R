dd <- AllPred %>%
  filter(year == 2016, stage != "L")

### PEAK FL VS. PEAK POLL
ggplot(dd, aes(x = peak.poll, y = peak.fl, color = stage, shape = factor(year))) +
  geom_point() + 
  labs(x = "Peak pollinator visitation (d.o.y)", y = "Peak flowering (d.o.y)", color = "Stage") +
  #scale_color_manual(labels = c ("E","M"), values=c("#F8766D", "#00BA380")) +
  geom_abline(slope = 0.7933, intercept = 42.6189, color = "blue") +
  geom_abline(slope = 1, color = "grey80", linetype = "dashed") +
  theme_minimal(base_size = 18)


summary(lm(peak.fl ~ peak.poll, dd))


### POLLEN LIMITATION
Biomass %>% 
  filter(Year == 2016, Stage != "L") %>% 
  ggplot(aes(y = Seed_mass, x = Treatment, color = Treatment)) +
  geom_boxplot() +
  facet_grid(~ Stage) + 
  theme_minimal(base_size = 18) + #outliers were re-weighed: they are correct
  labs(y="Reproductive output (g)", x="Treatment", fill="Treatment")+
  scale_fill_manual(labels = c("control (C)", "hand-pollinated (HP)"), values = c("#F8766D", "#00BA38"))


### MISMATCH VS. REP OUTPUT
Biomass %>% 
  left_join(dd, by=c("Site"="siteID", "Stage"="stage")) %>% 
  select(Stage, Site, Treatment, Seed_mass, peak.diff) %>% 
  #mutate(Stage = factor(Stage, levels = c("F", "E","M"))) %>% 
  ggplot(aes(y=Seed_mass, x=abs(peak.diff), color=Stage)) +
  geom_jitter() +
  labs(y="Reproductive output (g)", x="Mismatch") +
  #scale_color_manual(labels = c("E", "M", "L"), values = c("#F8766D", "#00BA38", "#619CFF")) +
  theme_minimal(base_size = 18)

ggplot(AllPred, aes(x = peak.fl, y = peak.poll, color = stage)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_abline(slope=1, color = "grey")

summary(lm(peak.poll ~ peak.fl + stage, AllPred))
summary(lm(first.poll ~ first.fl, AllPred))
summary(lm(last.poll ~ last.fl, AllPred))

AllPred %>% 
  group_by(stage) %>% 
  summarise(mean(peak.fl), mean(peak.poll))

mean(AllPred$peak.poll)

dd <- AllPred %>% 
  ungroup() %>% 
  select(siteID, peak.diff)

ddd <- pollen17 %>% 
  mutate(Site2 = paste("0", Site, sep = "")) %>% 
  mutate(Site2 = ifelse(Site2 == "010", "10", Site2)) %>% 
  mutate(siteID = paste(Stage, Site2, sep = " ")) %>% 
  left_join(dd, by = c("siteID")) %>% 
  ggplot(aes(x = peak.diff, y = ReproductiveOutput, shape = Stage)) +
  geom_point()


summary(lm(ReproductiveOutput ~ abs(peak.diff) + Stage, ddd))

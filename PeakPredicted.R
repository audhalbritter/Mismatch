##### FLOWERS

dat <- phenology %>% 
  filter(year == "2017") %>% 
  #filter(site == "01", stage == "F") %>% 
  mutate(doy = yday(day))



CompareModels <- function(dat){
  fit1 <- glm(round(fl.sqm, 0) ~ doy + I(doy^2), data = dat, family = "poisson")
  fit2 <- glm(round(fl.sqm, 0) ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  tab <- AIC(fit1, fit2)
  AIC1 <- tab$AIC[1]
  AIC2 <- tab$AIC[2]
  res <- data_frame(AIC1 = AIC1,
             AIC2 = AIC2)
  return(res)
}

dat %>% 
  group_by(site, stage) %>%
  do(CompareModels(.)) %>% 
  mutate(Diff = AIC1 - AIC2) %>% pn

plot(dat$doy, dat$fl.sqm)
with(new.dat, lines(x = doy, y = exp(pred)), col = "red")


tab <- AIC(fit1, fit2)
tab$AIC[2]


ggplot(dat, aes(x = doy, y = fl.sqm)) +
  geom_point() +
  geom_smooth()


PredictFlower <- function(dat){
  fit2 <- glm(round(fl.sqm, 0) ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  new.dat <- data.frame(doy = seq(min(dat$doy), max(dat$doy), length.out = 30))
  new.dat$pred <- exp(predict(fit2, new.dat))
  new.dat$pred
  res <- data_frame(pred = new.dat$pred)
  return(res)
}

pred.fl <- dat %>%
  group_by(site, stage, doy) %>%
  do(PredictFlower(.))


PredFl <- pred.fl %>%
  group_by(site, stage) %>%
  summarize(first = first(doy), peak = doy[which.max(pred)], last = last(doy)) %>% 
  rename(first.fl = first, peak.fl = peak, last.fl = last)



##### INSECTS
dat <- pollination %>% 
  filter(year == "2017") %>% 
  #filter(site == "01", stage == "F") %>% 
  mutate(doy = yday(day)) %>% 
  mutate(poll.sqm.trans = (poll.sqm - min(poll.sqm))/(max(poll.sqm) - min(poll.sqm)))


Compare.models <- function(dat){
fit2 <- glm(round(poll.sqm*100, 0) ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  tab <- AIC(fit1, fit2)
  AIC1 <- tab$AIC[1]
  AIC2 <- tab$AIC[2]
  res <- data_frame(AIC1 = AIC1,
                    AIC2 = AIC2)
  return(res)
}

dat %>% 
  group_by(site, stage) %>%
  do(Compare.models(.)) %>% 
  mutate(Diff = AIC1 - AIC2) %>% pn



plot(dat$doy, dat$poll.sqm*100)
with(new.dat, lines(x = doy, y = exp(pred)), col = "red")
new.dat <- data.frame(doy = seq(min(dat$doy), max(dat$doy), length.out = 30))
new.dat$pred <- predict(fit2, new.dat)
exp(new.dat$pred)/100



PredictPollinator <- function(dat){
  fit2 <- glm(round(poll.sqm*100, 0) ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  new.dat <- data.frame(doy = seq(min(dat$doy), max(dat$doy), length.out = 30))
  new.dat$pred <- exp(predict(fit2, new.dat))
  new.dat$pred
  res <- data_frame(pred = new.dat$pred)
  return(res)
}

pred.poll %>% 
  group_by(site, stage, doy) %>%
  do(PredictPollinator(.))


PredPoll <- pred.poll %>%
  group_by(stage, site) %>%
  summarize(first = first(doy), peak = doy[which.max(pred)], last = last(doy)) %>% 
  rename(first.poll = first, peak.poll = peak, last.poll = last)



##### JOINING FLOWERS AND INSECTS

AllPred <- PredPoll %>% 
  left_join(PredFl, by=c("stage"="stage", "site"="site")) %>% 
  mutate(peak.diff = peak.fl-peak.poll, siteID = paste(stage, site)) %>% 

AllPred$siteID <- as.character(AllPred$siteID)
AllPred$siteID <- factor(AllPred$siteID, levels=unique(AllPred$siteID))  


# PLOTTING PEAK DIFF AGAINST SITE
ggplot(AllPred, aes(y = peak.diff, x = siteID)) +
  geom_point() +
  theme_minimal()

##### FLOWERS

dat <- phenology %>% 
  filter(year == "2017") %>% 
  #filter(site == "01", stage == "F") %>% 
  mutate(doy = yday(day))


CompareModels.fl <- function(dat){
  fit1 <- glm(flower.sum ~ doy + I(doy^2), data = dat, family = "poisson")
  fit2 <- glm(flower.sum ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  tab <- AIC(fit1, fit2)
  AIC1 <- tab$AIC[1]
  AIC2 <- tab$AIC[2]
  res <- data_frame(AIC1 = AIC1,
             AIC2 = AIC2)
  return(res)
}

dat %>% 
  group_by(site, stage) %>%
  do(CompareModels.fl(.)) %>% 
  mutate(Diff = AIC1 - AIC2) %>% pn


PredictPeakFlower <- function(dat){
  fit2 <- glm(flower.sum ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  new.dat <- data.frame(doy = dat$doy)
  new.dat$pred <- exp(predict(fit2, new.dat))
  new.dat$pred
  res <- data_frame(doy = new.dat$doy,
                    pred = new.dat$pred)
  return(res)
}

pred.fl <- dat %>%
  group_by(site, stage) %>%
  do(PredictPeakFlower(.))

# Calculate peak, first and end
PredFl <- pred.fl %>%
  group_by(site, stage) %>%
  summarize(first = first(doy), peak = doy[which.max(pred)], last = last(doy)) %>% 
  rename(first.fl = first, peak.fl = peak, last.fl = last)


#********************************************************************************************
### Test one site and stage
plot(dat$doy, dat$flower.sum)
with(new.dat, lines(x = doy, y = pred), col = "red")

fit2 <- glm(flower.sum ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
new.dat <- data.frame(doy = dat$doy)
new.dat$pred <- exp(predict(fit2, new.dat))
new.dat

plot(dat$doy, dat$flowering)
with(new.dat, lines(x = doy, y = exp(pred)), col = "red")

pred.fl %>% 
  filter(site == "07", stage == "F") %>% 
  ggplot(aes(x = doy, y = pred)) +
  geom_point()
#********************************************************************************************


##### INSECTS
dat <- pollination %>% 
  filter(year == "2017") %>% 
  #filter(site == "01", stage == "F") %>% 
  mutate(doy = yday(day)) %>% 
  mutate(poll.sqm.trans = (poll.sqm - min(poll.sqm))/(max(poll.sqm) - min(poll.sqm)))


Compare.models.pol <- function(dat){
    fit1 <- glm(fly ~ doy + I(doy^2), data = dat, family = "poisson")
    fit2 <- glm(fly ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
    tab <- AIC(fit1, fit2)
    AIC1 <- tab$AIC[1]
    AIC2 <- tab$AIC[2]
    res <- data_frame(AIC1 = AIC1,
                    AIC2 = AIC2)
  return(res)
}

dat %>% 
  group_by(site, stage) %>%
  do(Compare.models.pol(.)) %>% 
  mutate(Diff = AIC1 - AIC2) %>% pn



PredictPollinator <- function(dat){
  fit2 <- glm(fly ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  new.dat <- data.frame(doy = dat$doy)
  new.dat$pred <- exp(predict(fit2, new.dat))
  new.dat$pred
  res <- data_frame(doy = new.dat$doy,
                    pred = new.dat$pred)
  return(res)
}

pred.poll %>% 
  group_by(site, stage) %>%
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


 

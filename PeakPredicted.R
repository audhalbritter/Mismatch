##### FLOWERS

dat.fl <- phenology %>% 
  filter(year == "2017") %>% 
  #filter(site == "01", stage == "F") %>% 
  mutate(doy = yday(day))


CompareModels.fl <- function(dat.fl){
  fit1 <- glm(flower.sum ~ doy + I(doy^2), data = dat.fl, family = "poisson")
  fit2 <- glm(flower.sum ~ doy + I(doy^2) + I(doy^3), data = dat.fl, family = "poisson")
  tab <- AIC(fit1, fit2)
  AIC1 <- tab$AIC[1]
  AIC2 <- tab$AIC[2]
  res <- data_frame(AIC1 = AIC1,
             AIC2 = AIC2)
  return(res)
}

dat.fl %>% 
  group_by(site, stage) %>%
  do(CompareModels.fl(.)) %>% 
  mutate(Diff = AIC1 - AIC2) %>% pn


PredictPeakFlower <- function(dat.fl){
  fit2 <- glm(flower.sum ~ doy + I(doy^2) + I(doy^3), data = dat.fl, family = "poisson")
  new.dat.fl <- data.frame(doy = dat.fl$doy)
  new.dat.fl$pred <- exp(predict(fit2, new.dat.fl))
  new.dat.fl$pred
  res <- data_frame(doy = new.dat.fl$doy,
                    pred = new.dat.fl$pred)
  return(res)
}

pred.fl <- dat.fl %>%
  group_by(site, stage) %>%
  do(PredictPeakFlower(.))

# Calculate peak, first and end
PredFl <- pred.fl %>%
  group_by(site, stage) %>%
  summarize(first = first(doy), peak = doy[which.max(pred)], last = last(doy)) %>% 
  rename(first.fl = first, peak.fl = peak, last.fl = last)


#********************************************************************************************
### Test one site and stage
plot(dat.fl$doy, dat.fl$flower.sum)
with(new.dat.fl, lines(x = doy, y = pred), col = "red")

fit2 <- glm(flower.sum ~ doy + I(doy^2) + I(doy^3), data = dat.fl, family = "poisson")
new.dat.fl <- data.frame(doy = dat.fl$doy)
new.dat.fl$pred <- exp(predict(fit2, new.dat.fl))
new.dat.fl

plot(dat.fl$doy, dat.fl$flowering)
with(new.dat.fl, lines(x = doy, y = exp(pred)), col = "red")

pred.fl %>% 
  filter(site == "07", stage == "F") %>% 
  ggplot(aes(x = doy, y = pred)) +
  geom_point()
#********************************************************************************************


##### INSECTS
dat.pol <- pollination %>% 
  filter(year == "2017") %>% 
  #filter(site == "01", stage == "F") %>% 
  mutate(doy = yday(day)) 
  #mutate(poll.sqm.trans = (poll.sqm - min(poll.sqm))/(max(poll.sqm) - min(poll.sqm)))


Compare.models.pol <- function(dat.pol){
    fit1 <- glm(fly ~ doy + I(doy^2), data = dat.pol, family = "poisson")
    fit2 <- glm(fly~ doy + I(doy^2) + I(doy^3), data = dat.pol, family = "poisson")
    tab <- AIC(fit1, fit2)
    AIC1 <- tab$AIC[1]
    AIC2 <- tab$AIC[2]
    res <- data_frame(AIC1 = AIC1,
                    AIC2 = AIC2)
  return(res)
}

dat.pol %>% 
  group_by(site, stage) %>%
  do(Compare.models.pol(.)) %>% 
  mutate(Diff = AIC1 - AIC2) %>% pn



PredictPollinator <- function(dat.pol){
  fit2 <- glm(fly ~ doy + I(doy^2) + I(doy^3), data = dat.pol, family = "poisson")
  new.dat.pol <- data.frame(doy = dat.pol$doy)
  new.dat.pol$pred <- exp(predict(fit2, new.dat.pol))
  new.dat.pol$pred
  res <- data_frame(doy = new.dat.pol$doy,
                    pred = new.dat.pol$pred)
  return(res)
}

pred.poll <- dat.pol %>% 
  group_by(site, stage) %>%
  do(PredictPollinator(.))

# Calculate peak, first and end
PredPoll <- pred.poll %>%
  group_by(stage, site) %>%
  summarize(first = first(doy), peak = doy[which.max(pred)], last = last(doy)) %>% 
  rename(first.poll = first, peak.poll = peak, last.poll = last)



##### JOINING FLOWERS AND INSECTS

AllPred <- PredPoll %>% 
  left_join(PredFl, by=c("stage"="stage", "site"="site")) %>% 
  mutate(peak.diff = peak.fl-peak.poll, siteID = paste(stage, site))

AllPred$siteID <- as.character(AllPred$siteID)
AllPred$siteID <- factor(AllPred$siteID, levels=unique(AllPred$siteID))  

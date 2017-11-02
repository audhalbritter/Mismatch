### FLOWERS

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

dat %>% 
  group_by(site, stage) %>%
  do(PredictFlower(.))


### INSECTS
dat <- pollination %>% 
  filter(year == "2017") %>% 
  #filter(site == "01", stage == "F") %>% 
  mutate(doy = yday(day)) %>% 
  mutate(poll.sqm.trans = (poll.sqm - min(poll.sqm))/(max(poll.sqm) - min(poll.sqm)))


Compare.models <- function(dat){
  fit1 <- glm(round(poll.sqm*100, 0) ~ doy + I(doy^2), data = dat, family = "poisson")
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



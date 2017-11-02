dat <- phenology %>% 
  filter(site == "02", stage == "F") %>% 
  mutate(doy = yday(day))



>SmoothFunction <- function(dat){
  fit1 <- glm(round(fl.sqm, 0) ~ doy + I(doy^2), data = dat, family = "poisson")
  fit2 <- glm(round(fl.sqm, 0) ~ doy + I(doy^2) + I(doy^3), data = dat, family = "poisson")
  tab <- AIC(fit1, fit2)
  res <- tab$AIC[1] - tab$AIC[2]
  return(res)

  
  #new.dat <- data.frame(doy = seq(min(dat$doy), max(dat$doy), length.out = 30))
  #new.dat$pred <- predict(fit, new.dat)
}



plot(dat$doy, dat$fl.sqm)
with(new.dat, lines(x = doy, y = exp(pred)), col = "red")


AIC(fit1, fit2)

dat %>% 
  group_by(site, stage) %>%
  do(SmoothFunction(.))

ggplot(dat, aes(x = doy, y = fl.sqm)) +
  geom_point() +
  geom_smooth()

# ANALYSIS
# Simple Regressions
newdat <- pollinator%>%
  filter(stage != "L") %>%
  filter(vind != 3)

# Temperature: regression
mod01 <- lm(log(fly+1) ~ temperature + stage, data = newdat)
summary(mod01)
anova(mod01)
par(mfrow = c(1,1))
plot(mod01)
        
# Vind, Weather, hour: anova
mod01 <- lm(log(fly+1) ~ factor(hour) + stage, data = newdat)
summary(mod01)

str(pollinator)

#chek residuals independent on x
plot(mod01$resid~mod01$fitted)

plot(mod01)
hist(log(newdat$fly))

# look at normal dist. of residuals
qqnorm(mod01$resid)
#this test should be "not significant" 
shapiro.test(mod01$resid)

#konstant varians av residualer(är variansen konstant längs x?)
plot(mod01$resid~mod01$fitted) 

#Independence of observations - no autocorrelation of residuals
plot(mod01$resid)#testar för seriemönster
durbin.watson(mod01)#leta efter autocorrelation, fick inte att fungera 



head(pollinator)


pollinator %>%
  group_by(stage)%>%
  filter(!is.na(vind))%>%
  summarise(corel = cor(temperature, vind))


plot((pollinator$sol.og.sky), pollinator$fly)# det första är för x-axeln och det andra är för y. Lag for VVT. LAg tabell med Anova bara att skriva >anova(mod01) så fungerar det för var ny variabel. 


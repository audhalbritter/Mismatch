# ANALYSIS
# Simple Regressions
newdat <- pollinator%>%
  filter(stage != "L") %>%
  filter(vind != 3)

# Temperature: regression
mod01 <- lm(log(fly+1) ~ sol.og.sky + stage, data = newdat)
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

#konstant varians av residualer(?r variansen konstant l?ngs x?)
plot(mod01$resid~mod01$fitted) 

#Independence of observations - no autocorrelation of residuals
plot(mod01$resid)#testar f?r seriem?nster
durbin.watson(mod01)#leta efter autocorrelation, fick inte att fungera 



head(pollinator)


pollinator %>%
  group_by(stage)%>%
  filter(!is.na(vind))%>%
  summarise(corel = cor((vind), fly))
anova(mod01)#FRÅGA AUD 

plot((pollinator$vind), pollinator$fly)# det f?rsta ?r f?r x-axeln och det andra ?r f?r y. Lag for VVT. 
#Boxplot 
pollinator%>%filter(stage!="L")%>%

ggplot(aes(x=sol.og.sky , y=fly))+
geom_boxplot() +
facet_wrap(~stage)
 

#Boxplot 
pollinator%>%filter(stage!="L")%>%filter(vind!=3)%>%filter(!is.na(vind))%>%#try to use filter
  ggplot(aes(x=factor(vind) , y=fly))+
  geom_boxplot() +
  facet_wrap(~stage)

# Temp
pollinator%>%filter(stage!="L")%>%
  mutate(stage = plyr::mapvalues(stage, c("E", "M"), c("Early", "Mid"))) %>% 
  ggplot(aes(x=temperature , y=fly)) +
  geom_smooth(method=lm) +
  xlab("Temperature in °C") +
  ylab("Number of flies") +
  geom_point() +
  facet_wrap(~stage)
#add labels to axes
plot(x,
     y,
     main="temperature vs flies",
     xlab="celsius",
     ylab="flies") 
#make a regressionline for temperature
ggplot(mpg, aes(x=temperature , y=fly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

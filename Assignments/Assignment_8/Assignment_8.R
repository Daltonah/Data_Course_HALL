library(modelr)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(MASS)
#load data
df1 <- read_csv("../../Data/mushroom_growth.csv")

#create plots for response and predictors

ggplot(data = df1, aes(x= Light, y=GrowthRate)) + theme_bw() + geom_point()+facet_wrap(~Species)

ggplot(data = df1, aes(x=Nitrogen, y=GrowthRate)) + theme_bw() + geom_point()+ facet_wrap(~Species)

ggplot(data = df1, aes(x=Humidity, y=GrowthRate)) + theme_bw() + geom_point()+ facet_wrap(~Species)

ggplot(data = df1, aes(x=Temperature, y=GrowthRate)) + theme_bw() + geom_point()+ facet_wrap(~Species)

#create models with dependent variable as "GrowthRate"

mod1 <- glm(data=df1, formula = GrowthRate ~ Light * Nitrogen * Humidity * Temperature)

mod2 <- glm(data=df1, formula = GrowthRate ~ Light + Temperature + Humidity* Nitrogen)

stepAIC(mod2)
stepAIC(mod1)

mod3 <- aov(formula = GrowthRate ~ Light + Nitrogen + Humidity + Temperature + 
              Light:Nitrogen + Light:Humidity + Humidity:Temperature, data = df1)

mod4 <-glm(data=df1, formula = GrowthRate ~ Light * Nitrogen * Humidity * Temperature+ Species)

stepAIC(mod4)



mod5 <- glm(formula = GrowthRate ~ Light + Nitrogen + Humidity + Temperature + 
              Species + Light:Nitrogen + Light:Humidity + Humidity:Temperature, 
            data = df1)

#calculate mean sq. error of each model, smaller=better

mean(mod1$residuals^2)

mean(mod2$residuals^2)

mean(mod3$residuals^2)

mean(mod4$residuals^2)

mean(mod5$residuals^2)

# mod4 looks like the best model so far
mod4$formula

#add predictions and make another df

df2 <- df1 %>% 
  add_predictions(mod4) 

#Make hypothetical data frame

fake_df = data.frame(Light= rep(c(5,15,25), 120) , Nitrogen= rep(c(0,5,15,25,35), 72), Temperature= rep(c(5,10,15,20,25,30),60), Species= rep(c("P.ostreotus","P.cornucopiae"),180),
                     
                     Humidity= rep(c("High","Low"), 180))


fakedf_pred= fake_df %>% add_predictions(mod4)

#graph 


df2$PredictionType <- "Real"
fakedf_pred$PredictionType <- "Hypothetical"

combo <- full_join(df2, fakedf_pred)

ggplot(data = combo, aes(x= Nitrogen, y=pred, color=PredictionType))+ geom_point()+
  geom_point(aes(y=GrowthRate),color="BlacK") + theme_bw()






library(tidyverse)
library(MASS)
library(GGally)
library(modelr)
library(patchwork)
df <- read.csv("../../Data/GradSchool_Admissions.csv")

#Make summary
summary(df)

#clean data
df$admit <- as.logical(df$admit)

df$rank <- factor(df$rank)

#Test Models 

mod1 <- glm(data=df, formula = admit~ gre + rank * gpa)

mod2 <- glm(data= df, formula = admit~ rank + gpa+ gre, family = "binomial")

stepAIC(mod1)
stepAIC(mod2)

mean(mod2$residuals^2)
# We will go with the second model

#Make new data frame with predictions
df_pred <- add_predictions(df,mod2, type = "response")


# Showing various plots of the predicted data
ggplot(df_pred, aes(x=gpa, y=pred)) + geom_point()
#It appears that acceptance should go up with gpa, but lets include rank as well.
ggplot(df_pred, aes(x=gpa, y=pred, color=rank)) + geom_point()
#It looks like the higher rank undergraduate school you come from, the higher your expected acceptance.
#Also within each rank, the higher gpa increases expected acceptance.

#Now lets look at gre predictions
ggplot(df_pred, aes(x=gre, y=pred, color=rank)) + geom_point()

#We get very similar results, with higher gre increasing likelihood of graduate school acceptance. 
#Higher ranked schools still having the advantage.


#Now lets compare predictions with reality

p1 <- ggplot(df, aes(x= gre, y= admit, color=rank)) +geom_point()
p2 <- ggplot(df_pred, aes(x=gre, y=pred, color=rank)) + geom_point()
p1 + p2

p3 <- ggplot(df_pred, aes(x=gpa, y=pred, color=rank)) + geom_point()
p4 <- ggplot(df, aes(x= gpa, y= admit, color=rank)) +geom_point()
p3 + p4
#These show us that while it is rare, individuals with low gre and/or gpa do in reality get admitted to grad school.
#Does this mean our model and graphs are bad predicters? No, reality just has many more variables than we can account for. 

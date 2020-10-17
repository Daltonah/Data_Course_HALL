#load needed library and assign df

library(tidyverse)
library(zoo)
library(ggplot2)
library(ggfortify)
land_df <-read.csv("./landdata-states.csv")
unicef_df <-read.csv("./unicef-u5mr.csv")
options(scipen = 999)

#look at data
summary(land_df)
glimpse(land_df)

# task one- recreate plot using ggplot
Region <- land_df$region

jpeg(file="HALL_Fig_1.jpg") 
ggplot(land_df, aes(x=Year, y=Land.Value, color=Region)) + theme_minimal() + geom_smooth()+ labs(y="Land Value (USD)") 
dev.off()
#Task II
#NA region is the region for inputs that are not valid or NA, they seem to be in Washington D.C..

#create data frame that shows only info for NA regions. 
land_df_NA <- land_df[is.na(land_df$region),]

#look at new Data frame
glimpse(land_df_NA)

#Tidy the unicef data


# pivot longer to clean year clutter
unicef_df2 <-pivot_longer(data = unicef_df, cols = starts_with("U")  , names_to = "Year",values_to = "Mortality_Rate", names_prefix = "U5MR.")

#remove NA values
unicef_df2 = unicef_df2 %>%
  filter(`Mortality_Rate` !="NA")

#change year from character to numeric
unicef_df2$Year <- as.numeric(unicef_df2$Year)
str(unicef_df2)


#Make plot and save
jpeg(file="HALL_Fig_2.jpg") 
ggplot(data =unicef_df2, aes(x=Year, y=`Mortality_Rate`, color=Continent))+ theme_minimal() + geom_point()+ labs(y="MortalityRate")
dev.off()


#task 4 fig 3 work

#putting together the mean values to use in a plot
unicef_df2_Mean <- unicef_df2 %>%
  group_by(Continent, Year) %>%
  summarise(Mean_Mortality_Rate = mean(Mortality_Rate))

#plotting graph and saving 

jpeg(file="HALL_Fig_3.jpg") 
ggplot(unicef_df2_Mean, aes(x=Year, y= Mean_Mortality_Rate, color=Continent))+
  geom_line(aes(group=Continent, color=Continent),size=2)+
  theme_minimal()+ labs(y="Mean Mortality Rate (deaths per 1000 live births)")
dev.off()

#Task 5 work

jpeg(file="HALL_Fig_4.jpg") 
p1 <-ggplot(data = unicef_df2, aes(x=Year, y=Mortality_Rate/1000)) + geom_point(color="blue", size=0.5) + facet_wrap(~Region) 
p1 + theme(strip.background = element_rect(colour = "black", fill= "white"))
dev.off()



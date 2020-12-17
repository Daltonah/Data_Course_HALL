#Load Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
#Read in the data we will be using and assign it as a data frame.
df<- read_csv("DNA_Conc_by_Extraction_Date.csv")

#Recreate Ben and Katy's histograms with labels.

hist(df$DNA_Concentration_Katy,main="DNA Concentration Histogram",xlab = "Katy's DNA Concentrations")

hist(df$DNA_Concentration_Ben,main="DNA Concentration Histogram",xlab = "Ben's DNA Concentrations")

#II Recreate provided images

plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Katy,main="Katy's Extractions",ylab="DNA Concentration",xlab="Year")

plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Ben,main="Ben's Extractions",ylab="DNA Concentration",xlab="Year")

#Saving the plots as jpegs

jpeg("./HALL_Plot1.jpeg")
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Katy,main="Katy's Extractions",ylab="DNA Concentration",xlab="Year")
dev.off()

jpeg("./HALL_Plot2.jpeg")
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Ben,main="Ben's Extractions",ylab="DNA Concentration",xlab="Year")
dev.off()

#IV Compare Ben vs Katy lowest performance

min.ben <-min(df$DNA_Concentration_Ben - df$DNA_Concentration_Katy)

min.row <-which(df$DNA_Concentration_Ben - df$DNA_Concentration_Katy==min.ben)

df[min.row,]$Year_Collected

#V Subset data to just downstairs and make scatterplot as directed

df_down <- df[df$Lab == "Downstairs",]

as.Date(df_down$Date_Collected)

jpeg("./Ben_DNA_over_time.jpg")
plot(x=as.Date(df_down$Date_Collected), y=df_down$DNA_Concentration_Ben, xlab="Date_Collected", ylab="DNA_Concentrations_Ben")
dev.off()

#VI (Bonus) Making a data frame using Ben's data with an extra column that contains average values for related year.

Ben_avg<-aggregate(DNA_Concentration_Ben ~ Year_Collected, df, mean)

print(Ben_avg)

#Max for avg and year associated
Ben_avg[which(Ben_avg$DNA_Concentration_Ben == max(Ben_avg$DNA_Concentration_Ben)),]

write.csv(Ben_avg, "./Ben_Average_Conc.csv")


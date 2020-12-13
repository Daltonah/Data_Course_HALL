library(tidyverse)
library(ggplot2)
read.csv("./DNA_Conc_by_Extraction_Date.csv")
df <- read.csv("./DNA_Conc_by_Extraction_Date.csv")
summary(Dframe)
hist(Dframe$DNA_Concentration_Katy, xlab= "DNA Conc", main = "Katy Histogram")
hist(Dframe$DNA_Concentration_Ben, xlab = "DNA Conc", main = "Ben Histogram")
jpeg(filename = "HALL_Plot1.jpeg")
plot (x=(as.factor(df$Year_Collected)), y = df$DNA_Concentration_Katy, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extraction") 
dev.off()
jpeg(filename = "HALL_Plot2.jpeg")
plot (x=(as.factor(df$Year_Collected)), y = df$DNA_Concentration_Ben, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extraction") 
dev.off()

comparedata <- df$DNA_Concentration_Ben / df$DNA_Concentration_Katy
df$DNA_Concentration_Ben / df$DNA_Concentration_Katy
x <- min(comparedata)
Place_min <- which(x == comparedata)
Low_Year <- df$Year_Collected[Place_min]

Downstairs_var <- filter(df,Lab == "Downstairs")

jpeg(filename = "Ben_DNA_over_time.jpg")
plot (x=(as.POSIXct(Downstairs_var$Date_Collected)), y = Downstairs_var$DNA_Concentration_Ben, xlab = "Date_Collected", ylab = "DNA Concentration", main = "Ben's Downstairs") 
dev.off()



library(tidyverse)
#1
data("mtcars")
str(mtcars)

#subset mtcars, #

auto_transmissions <- mtcars$am %in% "0"
Auto_subset <- mtcars[auto_transmissions,]
save(Auto_subset, file = "automatic_mtcars.csv")

#4/5 plots the effect of horsepower on miles-per-gallon using ggplot2 (update plot to have meaningful labels and title)
#saves this plot as a png image called “mpg_vs_hp_auto.png” in your Assignment_6 directory

as.numeric(mtcars$hp)
as.numeric(mtcars$mpg)  
jpeg(filename = "mpg_vs_hp_auto.png")
ggplot(Auto_subset, aes(x= hp, y= mpg)) +theme_bw() + geom_point() + geom_smooth() +
  labs(title = "Effect of HP on MPG", x= "Horsepower", y= "MPG") 
dev.off()

#plots the effect of weight on miles-per-gallon (with improved labels, again)
#saves this second plot as a tiff image called “mpg_vs_wt_auto.tiff” in your Assignment_5 directory
jpeg(filename = "mpg_vs_wt_auto.tiff")
ggplot(Auto_subset, aes(x=wt, y=mpg)) + theme_minimal() +geom_point() +geom_smooth()+
  labs(title = "Effect of vehicle weight on fuel efficiency", x= "Weight", y= "MPG")
dev.off()

#8/9subset the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
#saves that new subset as a csv file called mtcars_max200_displ.csv

Low_Disp <-!(mtcars$disp>200)
Low_Disp_Subset <- mtcars[Low_Disp,]
save(Low_Disp_Subset, file = "mtcars_max200_displ.csv")

#10/11 include code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)
#prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt

#sort? or order? then do tail? 

mtcars_max<- max(mtcars$hp)
Auto_max<- max(Auto_subset$hp)
Low_Disp_max<- max(Low_Disp_Subset$hp)

print(c(mtcars_max, Auto_max, Low_Disp_max))

writeLines(c("The code for max HP of mtcars is data(mtcars) \n then mtcars_max<- max(mtcars$hp)",
"\n The code for calculating max HP for only automatic cars is auto_transmissions <- mtcars$am %in% 0
Auto_subset <- mtcars[auto_transmissions,] \n then Auto_max<- max(Auto_subset$hp)", 
"\n The code for max HP of low Disp cars is Low_Disp <-!(mtcars$disp>200)
Low_Disp_Subset <- mtcars[Low_Disp,] \n then Low_Disp_max<- max(Low_Disp_Subset$hp)"), "hp_maximums.txt")

#12/13combine the following 3 plots into one image using the patchwork package (all 3 plots use the full un-subsetted mtcars data)
#Scatterplot + trendline of the effect of weight on mpg (points and linear trendlines colored by the number of cylinders)
#Violin chart of the distributions of mpg for cars, separated and colored by the number of cylinders
#Scatterplot + trendline of the effect of horsepower on mpg (points and linear trendlines colored by the number of cylinders)
#save that combined figure as a single png image file called combined_mtcars_plot.png in your Assignment_6 directory

library(patchwork)
as.factor(mtcars$cyl)
p1<- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) + theme_minimal() +geom_point() +geom_smooth(method = lm)+
  labs(title = "Effect of vehicle weight on fuel efficiency", x= "Weight", y= "MPG")

p2<- ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, color=as.factor(cyl))) +geom_violin()+
  labs(x='Cylinders', y="MPG", color= "Cylinder number", title = "Cyl Effect on MPG")

p3<- ggplot(mtcars, aes(x= hp, y= mpg, color=cyl)) +theme_bw() + geom_point() + geom_smooth(method = lm) +
  labs(title = "Effect of HP on MPG", x= "Horsepower", y= "MPG") 

jpeg(filename = "combined_mtcars_plot.png")
p1+p2+p3
dev.off()

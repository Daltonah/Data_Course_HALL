#Libraries loaded
library(tidyverse)
library(MASS)
library(patchwork)
library(modelr)
library(broom)
#Load our Data
Fs1 <- read_csv("./FacultySalaries_1995.csv")

# Shorten our column names
colnames(Fs1)[colnames(Fs1) == "AvgAssistProfSalary"] = "Assist"

colnames(Fs1)[colnames(Fs1) == "AvgAssocProfSalary"] = "Assoc"

colnames(Fs1)[colnames(Fs1) == "AvgFullProfSalary"] = "Full"

#Change some our data to long format for clarity

fs2 <- pivot_longer(Fs1, cols = 5:7, names_to ="Rank", values_to = "Salary")

fs3 <- pivot_longer(fs2, cols = 6:8, names_to = "Comp", values_to= "Comp_value")

fs4 <- pivot_longer(fs3,cols = 7:10, names_to = "Employees", values_to= "Employee_value")

#Remove erroneous data

fs5 <-fs4[-c(41545:41580), ]

#Plot Data

p1 <-ggplot(fs5, aes(x=Rank, y=Salary, fill=Rank)) + geom_boxplot()+ facet_wrap(~Tier)+ theme_minimal()+ theme(axis.text.x = element_text(angle = 60, hjust=1))

#Save plot as jpeg file

ggsave(filename = "HALL_Fig_1.jpeg",p1)                                                                                                          


# Task 2

#Create models to be compared by anova
mod1 <-glm(data = fs5, formula= Salary~State)

mod2 <- glm(data = fs5,formula= Salary~Tier)

mod3 <- glm(data = fs5, formula= Salary~Rank)

#creating anova table
anova_table <-anova(mod1,mod2,mod3)

#turning anova table into a txt file
write.table(anova_table, file = "Salary_ANOVA_Summary.txt", sep = "\t")

#load next data set

jo1<- read_csv("./Juniper_Oils.csv")

#Task 3
jo2<-pivot_longer(jo1,cols= 11:33, names_to="Chemicals", values_to= "Concentration")

#Task 4 
#facet wrap by chemical, x= years since burn, y= conc, add a best fit line

p2<- ggplot(data=jo2,aes(x=YearsSinceBurn, y=Concentration))+theme_minimal()+facet_wrap(~Chemicals, scales = "free")+geom_smooth()

ggsave(filename = "HALL_Fig_2.jpeg",p2) 


#Task 5 

#Make a linear model to show significant chemical concentrations against years since burn

jmod<- glm(data=jo2, formula = Concentration~YearsSinceBurn+ Chemicals)


#Use tidy to create data frame with just significant chemicals
options(scipen = 999)

tidytable<-tidy(jmod)

sigs_chems <- filter(tidytable, p.value < 0.05)





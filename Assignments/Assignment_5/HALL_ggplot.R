#1
library(tidyverse)
data(iris)
jpeg(file="iris_fig1.png")
ggplot(iris, aes(x=Sepal.Length , y= Petal.Length, color= Species )) + geom_point() + geom_smooth(method = lm) + theme_minimal() +
labs(title = "Sepal length vs petal length", subtitle = "for three iris species") 
dev.off()

#2
jpeg(file="iris_fig2.png")
ggplot(iris, aes(x=Petal.Width, fill= Species)) + geom_density(alpha=(0.5)) + theme_minimal() +
labs(title = "Distribution of Petal Width", subtitle = "for three iris species")
dev.off()
#3
jpeg(file="iris_fig3.png")
ggplot(iris, aes(x=Species , y= c(Petal.Width/Sepal.Width) , fill= Species)) + geom_boxplot() + theme_minimal() +
  labs(title= "Sepal- to Petal-Width Ratio", subtitle = "for three iris species", y = "Ratio of Sepal Width to Petal Width")
dev.off()
#4





#ernies version
jpeg(file="iris_fig4.png")
iris$`Species1` <- rownames(iris) # create new column for species names
iris$Species_length_dev <- round((iris$Sepal.Length - mean(iris$Sepal.Length))/sd(iris$Sepal.Length), 3) # compute normalized Lengths
iris <- iris[order(iris$Species_length_dev), ] #sort
iris$`Species1` <- factor(iris$`Species1`, levels = iris$`Species1`)

ggplot(iris, aes(x=`Species1`, y=Species_length_dev, label=Species_length_dev)) +
  #ggplot(iris, aes(x=Sepal.Length, y=Sepal.Length -mean(Sepal.Length))) +
  geom_bar(stat='identity', aes(fill=Species), width = .5) +
  labs(subtitle="Sepal length deviance from the mean of all observations", y="Deviance from the Mean")+
  coord_flip() +
  #theme(axis.title.y=element_blank()
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
dev.off()




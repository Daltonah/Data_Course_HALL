# ggplot2 intoduction 

# Load ggplot2 (it is included in the tidyverse package) ####
library(tidyverse)

# Load the data we will work with (built-in to ggplot)
data("midwest", package = "ggplot2")

# Intro to ggplot syntax

# The syntax for constructing ggplots could be puzzling if you are a beginner or work primarily with base graphics. 
# The main difference is that, unlike base graphics, ggplot works with dataframes and not individual vectors. 
# All the data needed to make the plot is typically be contained within the dataframe supplied to the ggplot() itself 
# or can be supplied to respective geoms. More on that later.
# The second noticeable feature is that you can keep enhancing the plot by adding more layers (and themes) 
# to an existing plot created using the ggplot() function.


# THE BASIC STEPS FOR CREATING A GRAPH:
  # 1. Initialize the plot by giving ggplot a data.frame to work with 
  # 2. Tell ggplot which "aesthetics" to use, and where (x and y axes, colors, sizes, shapes, transparency, etc)
  # 3. Add "geom" layers to display the aesthetics ... geoms tell ggplot how to deal with the aesthetic layers you chose
  # 4. Add labels, make it pretty
  # 5. Bask in your genious

# Let’s initialize a basic ggplot based on the midwest dataset that we loaded.
ggplot(midwest) # what do you see?
names(midwest)
# give it some aesthetics to work with...
ggplot(midwest, aes(x=area, y=poptotal))  # area and poptotal are columns in 'midwest'

# A blank ggplot is drawn. Even though the x and y are specified, there are no points or lines in it. 
# This is because, ggplot doesn’t assume that you meant a scatterplot or a line chart to be drawn. 
# I have only told ggplot what dataset to use and what columns should be used for X and Y axis. 
# I haven’t explicitly asked it to draw any points.
# 
# Also note that aes() function is used to specify the X and Y axes. 
# That’s because, any information that is part of the source dataframe has to be specified inside the aes() function.

# Give it a geom to map to your defined aesthetics... Basic Scatterplot, in this case:
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() # The "+" tells ggplot to add another layer to our base plot

# Add another geom ... a trendline:
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method = "lm")
# The line of best fit is in blue. Can you find out what other method options are available for geom_smooth? 

# Store your plot as an object to add to...
p <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method = "lm")
p
# Zoom in
p + lims(x=c(0,0.1),y=c(0,1000000)) # what did this do?
p + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) # how is this different?
#ccor zooms in without chnging data

# Store this new zoomed-in plot
p2 <- p + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))

# Add Title and Labels:
p2 + labs(title="Area Vs Population", 
          subtitle="From midwest dataset", 
          y="Population", 
          x="Area", 
          caption="Midwest Demographics")

# Nifty!  So here's the full function call to make this plot:
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Let's make it pretty ####

# Change color of points and line to static values:
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(color="steelblue",size=3) + 
  geom_smooth(method="lm",color="firebrick") + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
# what else did we change, and how?


# Here's where ggplot gets really cool...
# Suppose if we want the color to change based on another column in the source dataset, 
# we can specify "color" inside the "aesthetic" aes() function.
p3 <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(color=state),size=3) + 
  geom_smooth(method="lm",color="firebrick") + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
p3

# Don't like those colors?
p3 + scale_color_brewer(palette = "Set1")

# Want more color choices? You can check them out in the RColorBrewer package, or even make your own
library(RColorBrewer)
brewer.pal.info

3
remotes::install_github("wilkelab/cowplot")

install.packages("colorspace", repos = "http://R-Forge.R-project.org")
remotes::install_github("clauswilke/colorblindr")
# Make your own and take a peek at it:
library(colorblindr)
pal = c("#c4a113","#c1593c","#643d91","#820616","#477887","#688e52",
        "#12aa91","#705f36","#8997b2","#753c2b","#3c3e44","#b3bf2d",
        "#82b2a4","#894e7d","#a17fc1","#262a8e","#abb5b5","#000000")
palette_plot(pal)
# You can even check to see if your color choices would work for someone who has colorblindness...
cvd_grid(palette_plot(pal))

# Our plot with my custom color palette
p3 + scale_color_manual(values=pal)

# Other neat tricks:
p3 + scale_x_reverse()
p3 + theme_minimal()
p3 + theme_dark()


# You can also transform your data right in ggplot:
p4 = ggplot(midwest, aes(x=area/max(midwest$area), y=log10(poptotal))) + 
  geom_point(aes(color=state),size=3) + 
  geom_smooth(method="lm",color="firebrick") + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", color = "State",
       y="log10 Population", x="Area (proportion of max)", caption="Midwest Demographics") +
  theme_minimal() +
  scale_color_manual(values=pal)

p4


# Want to divide up your plot into multiple ones based on a categorical variable?
p4 + facet_wrap(~ state)
p4 + facet_wrap(~ state, scales = "free") + theme(legend.position = "none")
p4 + facet_wrap(~ state) + theme(legend.position = "none", strip.text.x = element_text(size = 12, face="bold"))
p4 + facet_wrap(~ state) + theme(legend.position = "none", 
                                 strip.text.x = element_text(size = 12, face="bold"),
                                 strip.background = element_rect(fill = "lightblue"))


# Some other "geom" types ... for categorical x axis
p5 = ggplot(midwest, aes(x=state,y=percollege, fill=state)) + labs(x="State",y="Percent with college degree")  
p5

p5 + geom_boxplot()
p5 + geom_violin()
p5 + geom_bar(stat="identity") # something wrong with this picture!


#practice example

data("mtcars")
ggplot(mtcars, aes(x=mpg , y=as.factor(carb), color=as.factor(carb),)) + labs (x= "mpg", y="Carb", title= "Does MPG affect Carbs?-No", color = "Carbs!") + geom_point()

# Geoms for looking at a single variable's distribution:
library(carData)
data("MplsStops")

ggplot(MplsStops, aes(x=lat)) + geom_histogram() + labs(title = "Latitude of police stops in Minneapolis - 2017",)
ggplot(MplsStops, aes(x=lat, fill = race)) + geom_density(alpha = .5) + labs(title = "Latitude of police stops in Minneapolis - 2017")


ggplot(MplsStops, aes(x=lat, fill = race)) + geom_histogram() + labs(title = "Latitude of police stops in Minneapolis - 2017") +
  facet_wrap(~race)



# Look at lat AND lon
ggplot(MplsStops, aes(x=lat,y=long,color=race)) + geom_point() + theme_minimal()

ggplot(MplsStops, aes(x=lat,y=long,color=race)) + geom_point() + theme_minimal() + facet_wrap(~race) # "overplotting!?"


ggplot(MplsStops,)

# Check out the issue with some random data
random_data = data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 11, 4.5) )

# quick look at data
plot(random_data$x)

# Basic scatterplot
ggplot(random_data, aes(x=x, y=y) ) +
  geom_point()

# 2D Density plot, instead
ggplot(random_data, aes(x=x, y=y) ) +
  geom_bin2d() +
  theme_bw()


# Back to our over-plotted figure from before...
ggplot(MplsStops, aes(x=lat,y=long,color=race)) + geom_point(alpha=.05) + theme_minimal() + facet_wrap(~race)

ggplot(MplsStops, aes(x=lat,y=long,color=race)) + geom_density_2d() + theme_minimal() + facet_wrap(~race)

ggplot(MplsStops, aes(x=lat,y=long)) + geom_bin2d()

ggplot(MplsStops, aes(x=lat,y=long)) + geom_bin2d() + facet_wrap(~race)


# try faceting by month
MplsStops$Month <- months(MplsStops$date)
(MplsStops$date)

ggplot(MplsStops, aes(x=lat,y=long)) + geom_bin2d() + facet_wrap(~Month)

library(gganimate)
library(gifski)
ggplot(MplsStops, aes(x=lat,y=long)) + geom_bin2d() +
  labs(title = "Month: {frame_state}") +
  transition_time(date) +
  ease_aes('linear')


gganimate::
# More advanced understanding of R functions will be required to replicate the following section, but it
# is included as an example follow-up analysis

# Plot using two related data sets
data("MplsDemo") # demographic info by neighborhood can be joined to our police stop dataset

# don't worry about this yet...you'll learn how to do this soon, but I'm just adding mean neighborhood income
# to each row of the police stop data set
income = as.numeric(as.character(plyr::mapvalues(MplsStops$neighborhood, from=MplsDemo$neighborhood, to = MplsDemo$hhIncome)))
MplsStops$income <- income


ggplot(MplsStops, aes(x=lat,y=long,color=income)) + geom_point(alpha=.2)
  
ggplot(MplsStops, aes(x=income)) + geom_histogram(bins = 30)


counts = as.data.frame(table(MplsStops$income))
counts$Var1 <- as.numeric(as.character(counts$Var1))
mod1 = lm(Freq ~ Var1, data = counts)

ggplot(counts, aes(x=Var1,y=Freq)) + geom_point() + geom_smooth(method="lm") +
  labs(x="Mean neighborhood income",y="Numer of police stops",title = "Police stop counts in each neighborhood",
       subtitle = paste0("Adjusted R-sq. value = ",signif(summary(mod1)$adj.r.squared),3)) + 
  theme_minimal()



#plot themes
+ theme_classic(
+ theme_bw()
#ect.

#customizing theme for plot
plot + theme(axis.title.x = element_text(face = "bold"))

panel.grid = element_line(color= "red")
element_blank()#deletes grid
#changes just x axis title to bold font
strip.text = element_text(face = "italic")
#turns elementd into italic font in facets
library(ggimage)

data("mtcars")
mtcars$image <- "#image file path"
#creates column with just that image over and over, concatinate multiple images for more 
#ex. c(rep( "file path", 16, rep (...)))
ggplot(mtcars,aes(x = disp, y = mpg)) + geom_point()
+ geom_image("#filepath")

geom_image()
ggbackground(#plot, "filepath to png")

library(patchwork)
allows you to add plots together
ex. p1 + p2
or p/ p2 
library(tidyverse)
data("iris")

pivot_longer()
#legnthens data, increasing rows and shortenting columns
#ex. 
pivot_longer(df, 1:3, names_to = "Days", values_to = "Grams")
str_remove(df_long$Days, "Day")
#gets rid of word day in the string. 

str_replace(df_long$Days, "Days", "Your Mom")
df_long$Days <-str_remove(df_long$Days, "Day")
df_long$Days <- as.numberic(df_long$Days)

library(readxl)
read_xlsx("./Data/wide_data_example.xlsx")
df <- read_xlsx("./Data/wide_data_example.xlsx")
df$`Treatment 1` <- as.numeric(df$`Treatment 1`)
df_long <- pivot_longer(df, 2:3, names_to = "Treatment", values_to = "Mass_g", names_prefix = "Treatment")

ggplot(df_long, aes(x=Treatment, y=Mass_g)) + geom_boxplot()

read.csv("./Data/BioLog_Plate_Data.csv")
df2 <- read.csv("./Data/BioLog_Plate_Data.csv")

pivot_longer(df2, 6:8, names_to = "Time", values_to = "Absorance", names_prefix = Hr_)
df2_long <- pivot_longer(df2, 6:8, names_to = "Time", values_to = "Absorbance", names_prefix = "Hr_")
names(df2_long)
df2_long$Time <- as.numeric(df2_long$Time)

ggplot(df2_long, aes(x=Time,y=Absorbance,color=Substrate)) +
  geom_smooth(se=FALSE) +
  facet_wrap(~Sample.ID) +
  theme_minimal()

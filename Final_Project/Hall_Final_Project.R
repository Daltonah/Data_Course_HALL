#Load Libraries
library(tidyverse)
library(ggplot2)
library(janitor)
library(MASS)
library(patchwork)
library(broom)
library(readxl)
library(naniar)
library(tidyquant)
#Read in our data in csv format. We will be looking at case rate over a 14 day period in utah by county and comparing it against mask compliance data
Raw_mask_age <- read.csv("./Use_Risk_BRFSS Survey Mask Wearing Compliance by Age and Sex_2020-12-15.csv")
Raw_mask_county <- read.csv("./Use_Risk_BRFSS Survey Mask Wearing Compliance by County_2020-12-15.csv")
Raw_case_county <- read.csv("./Use_Overview_COVID-19 14-Day Case Rate per 100,000 Population by County_2020-12-15.csv")
Raw_total_case <- read.csv("./Use_Demographics_Total People Living in Utah with COVID-19 by Age Chart_2020-12-15.csv")

#Use janitor package to clean the names of the data frames
mask_age = janitor::clean_names(Raw_mask_age)
mask_county = janitor::clean_names(Raw_mask_county)
case_county = janitor::clean_names(Raw_case_county)
case_age_sex = janitor::clean_names(Raw_total_case)
#I am still unsatisfied with some column names, so we will be manually renaming them.

mask_age <- mask_age %>%
  rename(sex = birthsex, age = age_group, individuals_masked_unmasked = n)
case_county <- case_county %>%
  rename(county = name, cases_in_period = x14_day_total_cases, rate_per_100k = x14_day_rate_100k, stability = data_note)

case_age_sex <- case_age_sex %>%
  rename(sex= birth_sex, age= age_group, case_number= count)

#Here we are fixing the age column be removing the word years off the end of each row.

years<- case_age_sex$age %>%
  as.character() %>%
  str_split(" ") %>%
  purrr::map_chr(1)
case_age_sex$age <- years

# If we look at mask_county data we see that some data is being suppressed, assuming this means we do not have access to this data, we will replace these values with NA
print(mask_county)

mask_county <- mask_county %>%
  mutate(percent_compliant = na_if(percent_compliant, "Suppressed"))

#We can see that mask percent is a character but we need it to be a numeric now
class(mask_county$percent_compliant)

mask_county$percent_compliant <- as.numeric(mask_county$percent_compliant)

class(mask_county$percent_compliant)

#Our other data frames look fine for now

#Now that our data is cleaned, lets visualize the data

mc <-ggplot(mask_county, aes(x=county , y=percent_compliant, color=county))+ theme_minimal()+ geom_col() + theme(axis.text.x = element_text(angle = 60, hjust=1))+ xlab("County")+
  ylab("Mask Compliance by %")+ ggtitle("Mask Compliance Percent per County")

#We can see that Salt Lake county has the highest percent of population that wears masks appropriately
#Duchesne and Unitah are competing for the lowest masking.

#Lets visualize how age and sex is correlated to mask observance 

mas <-ggplot(mask_age, aes(x=age , y=percent_compliance, color=sex))+ theme_dark() + facet_wrap(~compliance)+ geom_point(size=5)+ 
  xlab("Age Range")+ylab("Percent of Individuals")+ggtitle("Utah Mask Compliance by Age and Sex")

#From this we can see that Females are overall more likely to wear masks than men at all age ranges. 
#The largest disparity between men and woman wearing masks is the 18-34 age range, followed closely by 50-64 age range.
#It appears the oldest age 65+ and therefore most susceptible to disease, are the most likely overall to wear masks.
#While men between 18-34 and woman between 35-49 are least likely to mask up

#Now lets look at covid case counts per county between dec2 to dec 14th 2020

cc<- ggplot(case_county, aes(x=county , y=cases_in_period, color=jurisdiction))+ theme_bw()+geom_col()+ theme(axis.text.x = element_text(angle = 60, hjust=1))+ylab("Number of Cases")+
  xlab("County")+ggtitle("Utah Covid-19 Cases Between Dec-2-2020 and Dec-14-2020")

#Utah county takes the cake for the most cases by a large margin, followed by Utah county.
#But this is not the while picture, lets look at these number extrapolated to a rate per 100,000.

cc100 <-ggplot(case_county, aes(x=county , y=rate_per_100k, fill=jurisdiction))+ theme_bw()+geom_col()+ theme(axis.text.x = element_text(angle = 60, hjust=1))+ylab("Rate of Cases")+
  xlab("County")+ggtitle("Utah Covid-19 Case Rate per 100,000 Between Dec-2-2020 and Dec-14-2020")

#This chart is more fair as we see that Salt Lake only appeared to be doing worse at preventing Covid due to its population density.
#This time we see Sanpete county taking the lead with Millard county in second place.
#Interestingly both these counties are under the same Central Utah Jurisdiction.

#Lets take a look at these counties separated out by jurisdiction and see which counties are doing best.

ccj100<- ggplot(case_county, aes(x=county, y=rate_per_100k))+ theme_bw()+geom_col()+ theme(axis.text.x = element_text(angle = 50, hjust=1))+ylab("Rate of Cases")+
  xlab("County")+ggtitle("Utah Covid-19 Case Rate per 100,000 Between Dec-2-2020 and Dec-14-2020")+ facet_wrap(~jurisdiction)+labs(subtitle = "By Jurisdiction")

#This chart honestly does not give a lot of information besides showing how many counties are in each jurisdiction as well as the lowest rate of cases for jurisdictions with multiple counties

#Lastly, before we compare data to speculate, lets look at overall cases in Utah as of Dec-12-2020

ccas<- ggplot(case_age_sex, aes(x=age, y=case_number, color=sex))+theme_grey()+ geom_point(size=4,alpha=1/2)+labs(title = "Total People in Utah Living with Covid-19", subtitle = "As of Dec 15th 2020")+
  xlab("Age Range")+ylab("Number of Cases")



#It appears that there is no bias of the virus between sexes among all age ranges.

#Now that we have visualized our data individually, lets look at some charts side by side to see if we can notice patterns.

mas+ ccas

#Interestingly, even though males between 18 and 35 are less likely to wear a mask than women of that age range,we see in the total cases that males have notably less cases between age 15-24 currently.
#Besides that it looks like the even though males across all ages are less likely to wear a mask, that both sexes have about equal case numbers.

#Next lets take a look at mask and case data per county side by side.

cc + mc

#According to this comparison it would appear that even though Salt Lake county is one of the most mask compliant counties, they have significantly more cases than every other county.
#One could assume that masks do literally nothing if given just these two charts, but next lets look at a more accurate representation of cases next to mask compliance.

cc100+ mc

#When the cases are turned into rates per 100,000 individuals the story changes. Salt Lake county suddenly becomes average while the Central Utah jurisdiction struggles with high case rate.
#Comparing this to masks compliance in the two highest rate counties of Sanpete and Millard, we see that these two counties have some of the lowest mask compliance in utah.
#A completely opposite view of the previous comparison.

#In addition to this, we see the counties with lowest rate of cases did not have values provided for mask compliant and instead were labelled as "suppressed", this information would have been very useful in seeing if masks truly do help prevent the spread of Covid-19.
#Also interestingly, Uintah county has both one of the lowest case rates as well as lowest mask compliance.












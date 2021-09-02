###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(tidyverse)
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(mydata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)


#make a scatterplot of data
#we also add a linear regression line to it
p1 <- mydata %>% ggplot(aes(x=Height, y=Weight)) + geom_point() + geom_smooth(method='lm')

#look at figure
plot(p1)

#save figure
figure_file = here("results","resultfigure.png")
ggsave(filename = figure_file, plot=p1) 

######################################
#Data fitting/statistical analysis
######################################

# fit linear model
lmfit <- lm(Weight ~ Height, mydata)  

# place results from fit into a data frame with the tidy function
lmtable <- broom::tidy(lmfit)

#look at fit results
print(lmtable)

# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(lmtable, file = table_file)

######
#Contributer 3- boxplot with x= new variable and y=height
######

#Gender boxplot
BP3<-mydata%>%
  ggplot(aes(x=Gender, y=Height))+
  geom_boxplot(aes(fill=Gender))

plot(BP3)  

figure_file = here("results","BoxPlot.png")
ggsave(filename = figure_file, plot=BP3) 


#Age scatterplot
SP3<-mydata%>%
  ggplot(aes(x=Weight, y=Age))+
  geom_point(aes(color=Gender))+
  geom_smooth(method='lm')

plot(SP3)  

figure_file = here("results","ScatterPlot.png")
ggsave(filename = figure_file, plot=SP3) 

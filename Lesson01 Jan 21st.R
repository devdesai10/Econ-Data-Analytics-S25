3+6

#Set a working directory
setwd("~/Users/devandesai/Desktop/Econ Data Analytics")


#Import data from *.csv 

tesla <- read.csv("~/Desktop/Econ Data Analytics/TSLA HistoricalData_1737501231341.csv")
View(tesla)
# Primarily use lower case in this situation


#Create a Data variable
tesla$date <- as.Date(tesla$Date, "%m/%d/%Y")

#sort data
tesla <- tesla[order(tesla$date), ]

#Create stock price 
tesla$price = as.numeric(gsub("[$,]","",tesla$Close.Last))

#Create stock returns
#what is a stock return, it is the growth rate from one day to the next

library(dplyr)

tesla$lag1_price = lag(tesla$price, n=1, order_by = tesla$date)

#this is the price of the stock today minus the stock of yesterday divided by the price of yesterday multiplied by 100 
#this is the growth rate
tesla$return = ((tesla$price - tesla$lag1_price)/tesla$lag1_price)*100
View(tesla)

#visualizing

library(ggplot2)
library(plotly)

range(tesla$date)

ggplot(tesla, aes(x=date, y=price)) + 
  geom_line() + 
  theme(legend.position = "bottom") + 
  labs(title = "tesla daily returns",
      subtitle = "(rounded)",
      x = "date",
      y = "percent",
      caption = "source: https://www.nasdaq.com/") 

#+
#  theme(plot.title = element_text(hjust=0.5), 
#        plot.substitle=element_text(hjust=0.5),
#        axis.text.x=element_text(angle = 45, hjust=1)) +
#  scale_x_date(limits = c(min(tesla$date), ))


#stem and leaf plot 
stem(tesla$return, scale =3)

#can be a question in the future
  #What is the correlation between time and return

#Histogram
summary(tesla$return)

hist(tesla$return
     labels = TRUE,
     breaks = round(max(tesla$return, na.rm=TRUE) - min(tesla$return, na.rm=TRUE),0))

#Different types of breaks
#  Scott Breaks (Every 2.5)
#  Sturges (Every 5)
#  Freedman-Diaconis Breaks (Every 1)
  


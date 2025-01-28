# Lesson01-Univariate descriptive statistics / visualization
# Spring 2025
# Oscar Torres-Reyna

# Set working directory
# In the menu go to Session -> Set Working Directory
# select the folder where your data is

setwd("[ENTER PATH TO DATA HERE")
setwd("~/Documents/GitHub/Econ-Data-Analytics")

# Get the data

# "Source: https://www.nasdaq.com/"
# Find the stock
# Click on "Historical quotes"
# Select one year
# Download, file will be in *.csv format

####### TESLA

tesla <- read.csv("Lesson01-Tesla.csv", header = TRUE, stringsAsFactors = FALSE)

# Changing date in 'chr' format to 'Date' format
# https://www.r-bloggers.com/2020/04/a-comprehensive-introduction-to-handling-date-time-in-r/
# https://www.datacamp.com/doc/r/dates

tesla$date = as.Date(tesla$Date, "%m/%d/%Y")

tesla <- tesla[order(tesla$date), ]

# Changing the stock closing price from 'chr' to 'numeric'
#https://stackoverflow.com/questions/31944103/convert-currency-with-commas-into-numeric

tesla$price = as.numeric(gsub("[$,]","", tesla$Close.Last))

# Creating the lag of stock price

# If you do not have the package -dplyr- you need to install it, 
# once you install it you do not need to install it again.

install.packages("dplyr")

# Activating the package

library(dplyr)

# Create the lag 1 of price

tesla$lag1_price = lag(tesla$price, n=1, order_by = tesla$date)

# Create the stock return
# ((price_today - price_yesterday) / price_yesterday) * 100

tesla$return = ((tesla$price - tesla$lag1_price)/tesla$lag1_price)*100

# Visualizing 

# If you do not have them, need to install them once.

install.packages("ggplot2","plotly")

# Activate packages

library(ggplot2)
library(plotly)

# https://ggplot2.tidyverse.org/reference/
# https://exts.ggplot2.tidyverse.org/gallery/

# From the windows where the plot is display, click on "Zoom" to see full size
# Static plot
# Click on "Zoom" in the "Plots" window to expand the graph

# Check the date range in the data

range(tesla$date)

### Line graph - Time series since we are observing data over time

ggplot(tesla, aes(x=date, y=return)) +
  geom_line() +
  theme(legend.position= "bottom") +
  labs(title = "Tesla returns",
       subtitle = "(daily)", 
       x = "Date",
       y = "Percent",
       caption = "Source: https://www.nasdaq.com/")  +
  theme(plot.title= element_text(hjust= 0.5),
        plot.subtitle= element_text(hjust= 0.5),
        axis.text.x= element_text(angle = 45, hjust= 1)) +
  scale_x_date(limits = c(min(tesla$date), max(tesla$date)),
               breaks = seq(min(tesla$date), max(tesla$date), by = 7) )

# Stem-and-leaf, shows distribution of data

stem(tesla$return, scale = 3)

# Histogram - Frequency - shows distribution of data.

# Available colors in R

colors()

# Five summary numbers

summary(tesla$return)

# Histogram - manual breaks
# https://nkugwamarkwilliam.medium.com/how-to-determine-bin-width-for-a-histogram-r-and-pyth-653598ab0d1c

hist(tesla$return,  
     labels = TRUE, 
     breaks=round(max(tesla$return, na.rm=TRUE) - min(tesla$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(tesla$return), na.rm=TRUE),round(max(tesla$return, na.rm = TRUE),0)), 
     col="lightblue", 
     ylim=c(0,40), 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for Tesla" )

# Histogram - Freedman-Diaconis breaks

hist(tesla$return,  
     labels = TRUE, 
     breaks="Freedman-Diaconis", 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for Tesla" )

# Histogram - Scott breaks

hist(tesla$return,  
     labels = TRUE, 
     breaks="Scott", 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for Tesla" )

# Histogram - Sturges breaks

hist(tesla$return,  
     labels = TRUE, 
     breaks="Sturges", 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for Tesla" )


# Histogram - Relative Frequency
# https://r-charts.com/distribution/density-histogram/

# Saving histogram as an object

h = hist(tesla$return,
     probability = TRUE,
     breaks=round(max(tesla$return, na.rm=TRUE) - min(tesla$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(tesla$return), na.rm=TRUE),round(max(tesla$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for Tesla")

names(h)

hist(tesla$return,
     probability = TRUE,
     breaks=round(max(tesla$return, na.rm=TRUE) - min(tesla$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(tesla$return), na.rm=TRUE),round(max(tesla$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for Tesla")
# Add labels to the bars
text(x = h$mids, y = h$density, labels = round(h$density,3), adj = 1, cex = 1.5, srt = 90)

# Probabilities from the saved histogram

h$density

# Sum of probabilities must add to 1

sum(h$density)

# Expected mean (also knows as weighted mean)
# Accounts for the probabilities of all possible outcomes in a sample space

# Creating the sample space for returns by rounding them

tesla$return_integer = round(tesla$return, digits = 0)

# Display frequency table

table(tesla$return_integer)

# Create a new dataset (object) with all possible returns (rounded)

return_tesla = as.data.frame(table(tesla$return_integer))

return_tesla

# Creating the probabilities

return_tesla$prob = return_tesla$Freq/sum(return_tesla$Freq)

return_tesla

# Changing the format of 'Var1' from 'factor' to 'numeric'

return_tesla$return_tesla = as.numeric(as.character(return_tesla$Var1))

return_tesla

# Multiplying probability of event (return) happening times the event (return)

return_tesla$return_tesla_x_prob = return_tesla$return_tesla * return_tesla$prob

return_tesla

# Expected return_tesla: E(X)

exp_return_tesla = sum(return_tesla$return_tesla_x_prob)

exp_return_tesla

# Variance using expectations: X^2 - E(X^2)

return_tesla$return_tesla_sqr = return_tesla$return_tesla^2

return_tesla$return_tesla_sqr_x_prob = return_tesla$return_tesla_sqr* return_tesla$prob

return_tesla

# Expected return_tesla or X^2 - E(X^2)

exp_return_tesla_sqr = sum(return_tesla$return_tesla_sqr_x_prob)

exp_return_tesla_sqr

variance_return_tesla = exp_return_tesla_sqr - exp_return_tesla^2

variance_return_tesla

# Standard deviation

std_return_tesla = sqrt(variance_return_tesla)

std_return_tesla

# Similar to using the original returns in continous form

sd(tesla$return, na.rm = TRUE)

# Coefficient of variation

cv_tesla = std_return_tesla / exp_return_tesla

cv_tesla

write.csv(return_tesla, file="Lesson01-TeslaReturn.csv")

# Descriptive statistics

summary(tesla$return)


# Amazon

amazon <- read.csv("Lesson01-amazon.csv", header = TRUE, stringsAsFactors = FALSE)
amazon$date = as.Date(amazon$Date, "%m/%d/%Y")
amazon <- amazon[order(amazon$date), ]
amazon$price = as.numeric(gsub("[$,]","", amazon$Close.Last))
amazon$lag1_price = lag(amazon$price, n=1, order_by = amazon$date)
amazon$return = ((amazon$price - amazon$lag1_price)/amazon$lag1_price)*100
# Visualizing 
range(amazon$date)
### Line graph

ggplot(amazon, aes(x=date, y=return)) +
  geom_line() +
  theme(legend.position= "bottom") +
  labs(title = "amazon daily returns",
       subtitle = "(rounded)", 
       x = "Date",
       y = "Percent",
       caption = "Source: https://www.nasdaq.com/")  +
  theme(plot.title= element_text(hjust= 0.5),
        plot.subtitle= element_text(hjust= 0.5),
        axis.text.x= element_text(angle = 45, hjust= 1)) +
  scale_x_date(limits = c(min(amazon$date), max(amazon$date)),
               breaks = seq(min(amazon$date), max(amazon$date), by = 7) )

# Stem-and-leaf
stem(amazon$return, scale = 3)
# Histogram - Frequency
colors()
summary(amazon$return)
hist(amazon$return,  
     labels = TRUE, 
     breaks=round(max(amazon$return, na.rm=TRUE) - min(amazon$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(amazon$return), na.rm=TRUE),round(max(amazon$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for amazon" )

# Histogram - Relative Frequency

h = hist(amazon$return,
         probability = TRUE,
         breaks=round(max(amazon$return, na.rm=TRUE) - min(amazon$return, na.rm=TRUE), 0), 
         xlim=c(min(floor(amazon$return), na.rm=TRUE),round(max(amazon$return, na.rm = TRUE),0)), 
         col="lightblue", 
         xlab="Return (%)", 
         ylab="Probability", 
         main="Distribution returns for amazon")

names(h)

hist(amazon$return,
     probability = TRUE,
     breaks=round(max(amazon$return, na.rm=TRUE) - min(amazon$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(amazon$return), na.rm=TRUE),round(max(amazon$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for amazon")
text(x = h$mids, y = h$density, labels = round(h$density,3), adj = 1, cex = 1.5, srt = 90)

h$density

sum(h$density)

# Expected mean

amazon$return_integer = round(amazon$return, digits = 0)
table(amazon$return_integer)
return_amazon = as.data.frame(table(amazon$return_integer))
return_amazon
return_amazon$prob = return_amazon$Freq/sum(return_amazon$Freq)
return_amazon
return_amazon$return_amazon = as.numeric(as.character(return_amazon$Var1))
return_amazon
return_amazon$return_amazon_x_prob = return_amazon$return_amazon * return_amazon$prob
return_amazon
# Expected return_amazon - E(X)
exp_return_amazon = sum(return_amazon$return_amazon_x_prob)
exp_return_amazon
# Variance
return_amazon$return_amazon_sqr = return_amazon$return_amazon^2
return_amazon$return_amazon_sqr_x_prob = return_amazon$return_amazon_sqr* return_amazon$prob
return_amazon
# Expected return_amazon or X^2 - E(X^2)
exp_return_amazon_sqr = sum(return_amazon$return_amazon_sqr_x_prob)
exp_return_amazon_sqr
variance_return_amazon = exp_return_amazon_sqr - exp_return_amazon^2
variance_return_amazon
# Standard deviation
std_return_amazon = sqrt(variance_return_amazon)
std_return_amazon
sd(amazon$return, na.rm = TRUE)
# Coefficient of variation
cv_amazon = std_return_amazon / exp_return_amazon
cv_amazon

write.csv(return_amazon, file="Lesson01-AmazonReturn.csv")

# Descriptive statistics

summary(amazon$return)


# Microsoft

# Microsoft

microsoft <- read.csv("Lesson01-microsoft.csv", header = TRUE, stringsAsFactors = FALSE)
microsoft$date = as.Date(microsoft$Date, "%m/%d/%Y")
microsoft <- microsoft[order(microsoft$date), ]
microsoft$price = as.numeric(gsub("[$,]","", microsoft$Close.Last))
microsoft$lag1_price = lag(microsoft$price, n=1, order_by = microsoft$date)
microsoft$return = ((microsoft$price - microsoft$lag1_price)/microsoft$lag1_price)*100
# Visualizing 
range(microsoft$date)
### Line graph

ggplot(microsoft, aes(x=date, y=return)) +
  geom_line() +
  theme(legend.position= "bottom") +
  labs(title = "microsoft daily returns",
       subtitle = "(rounded)", 
       x = "Date",
       y = "Percent",
       caption = "Source: https://www.nasdaq.com/")  +
  theme(plot.title= element_text(hjust= 0.5),
        plot.subtitle= element_text(hjust= 0.5),
        axis.text.x= element_text(angle = 45, hjust= 1)) +
  scale_x_date(limits = c(min(microsoft$date), max(microsoft$date)),
               breaks = seq(min(microsoft$date), max(microsoft$date), by = 7) )

# Stem-and-leaf
stem(microsoft$return, scale = 3)
# Histogram - Frequency
colors()
summary(microsoft$return)
hist(microsoft$return,  
     labels = TRUE, 
     breaks=round(max(microsoft$return, na.rm=TRUE) - min(microsoft$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(microsoft$return), na.rm=TRUE),round(max(microsoft$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for microsoft" )

# Histogram - Relative Frequency

h = hist(microsoft$return,
         probability = TRUE,
         breaks=round(max(microsoft$return, na.rm=TRUE) - min(microsoft$return, na.rm=TRUE), 0), 
         xlim=c(min(floor(microsoft$return), na.rm=TRUE),round(max(microsoft$return, na.rm = TRUE),0)), 
         col="lightblue", 
         xlab="Return (%)", 
         ylab="Probability", 
         main="Distribution returns for microsoft")

names(h)

hist(microsoft$return,
     probability = TRUE,
     breaks=round(max(microsoft$return, na.rm=TRUE) - min(microsoft$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(microsoft$return), na.rm=TRUE),round(max(microsoft$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for microsoft")
text(x = h$mids, y = h$density, labels = round(h$density,3), adj = 1, cex = 1.5, srt = 90)

h$density

sum(h$density)

# Expected mean

microsoft$return_integer = round(microsoft$return, digits = 0)
table(microsoft$return_integer)
return_microsoft = as.data.frame(table(microsoft$return_integer))
return_microsoft
return_microsoft$prob = return_microsoft$Freq/sum(return_microsoft$Freq)
return_microsoft
return_microsoft$return_microsoft = as.numeric(as.character(return_microsoft$Var1))
return_microsoft
return_microsoft$return_microsoft_x_prob = return_microsoft$return_microsoft * return_microsoft$prob
return_microsoft
# Expected return_microsoft - E(X)
exp_return_microsoft = sum(return_microsoft$return_microsoft_x_prob)
exp_return_microsoft
# Variance
return_microsoft$return_microsoft_sqr = return_microsoft$return_microsoft^2
return_microsoft$return_microsoft_sqr_x_prob = return_microsoft$return_microsoft_sqr* return_microsoft$prob
return_microsoft
# Expected return_microsoft or X^2 - E(X^2)
exp_return_microsoft_sqr = sum(return_microsoft$return_microsoft_sqr_x_prob)
exp_return_microsoft_sqr
variance_return_microsoft = exp_return_microsoft_sqr - exp_return_microsoft^2
variance_return_microsoft
# Standard deviation
std_return_microsoft = sqrt(variance_return_microsoft)
std_return_microsoft
sd(microsoft$return, na.rm = TRUE)
# Coefficient of variation
cv_microsoft = std_return_microsoft / exp_return_microsoft
cv_microsoft

write.csv(return_microsoft, file="Lesson01-MicrosoftReturn.csv")

# Combining all results

table1 = data.frame(stock = c("Amazon", "Microsoft", "Tesla"),
                    exp_r = c(exp_return_amazon,exp_return_microsoft,exp_return_tesla),
                    var_r = c(variance_return_amazon, variance_return_microsoft, variance_return_tesla),
                    std_r = c(std_return_amazon, std_return_microsoft, std_return_tesla),
                    cv_r  = c(cv_amazon, cv_microsoft, cv_tesla))
table1

# CV = volatility relative to the return (finance)
# CV = economic inequality (economics)
#"In finance, the coefficient of variation allows investors to determine how much volatility, or risk, 
# is assumed in comparison to the amount of return expected from investments.
# The lower the ratio of the standard deviation to mean return, the better the risk-return tradeoff."
# https://www.investopedia.com/terms/c/coefficientofvariation.asp

# Lesson01-Univariate descriptive statistics / visualization
# Spring 2025
# Oscar Torres-Reyna

# DEALING WITH EXCEL FILES

# Set working directory
# In the menu go to Session -> Set Working Directory
# select the folder where your data is

setwd("[ENTER PATH TO DATA HERE")

# Get the data for AMAZON, TESLA, AND MICROSOFT

# "Source: https://www.nasdaq.com/"
# Find the stock
# Click on "Historical quotes"
# Select one year
# Download, file will be in *.csv format

# OPEN DATA IN EXCEL AND SAVE THEM AS EXCEL (extension *.xlsx)

####### TESLA

tesla <- read_excel("Lesson01-Tesla.xlsx")

View(tesla)

# Changing date from POSIXct to date format
# https://www.r-bloggers.com/2020/04/a-comprehensive-introduction-to-handling-date-time-in-r/
# https://www.datacamp.com/doc/r/dates

tesla$date = as.Date(tesla$Date, "%Y/%m/%D")

tesla <- tesla[order(tesla$date), ]

# Creating the price variable
# Notice the single quotes

tesla$price = tesla$`Close/Last`

# Creating the lag of stock price

# If you do not have the package -dplyr- you need to install it, 
# In the menu go to Tool -> Install Packages -> enter the name of the package.
# Once the package is installed, you do not need to install it again.

# Once install, you need to activate the package

library(dplyr)

# Create the lag 1 of price

tesla$lag1_price = lag(tesla$price, n=1, order_by = tesla$date)

# Create the stock return
# ((price_today - price_yesterday) / price_yesterday) * 100

tesla$return = ((tesla$price - tesla$lag1_price)/tesla$lag1_price)*100

# Visualizing 

# If you do not have them, need to install them once.

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

# Histogram - Relative Frequency
# https://r-charts.com/distribution/density-histogram/

# Saving histogram as an object

h_tesla = hist(tesla$return,
     probability = TRUE,
     breaks=round(max(tesla$return, na.rm=TRUE) - min(tesla$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(tesla$return), na.rm=TRUE),round(max(tesla$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for Tesla")

names(h_tesla)

hist(tesla$return,
     probability = TRUE,
     breaks=round(max(tesla$return, na.rm=TRUE) - min(tesla$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(tesla$return), na.rm=TRUE),round(max(tesla$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for Tesla")
# Add labels to the bars
text(x = h_tesla$mids, y = h_tesla$density, labels = round(h_tesla$density,3), adj = 1, cex = 1.5, srt = 90)

###### Amazon

amazon <- read_excel("Lesson01-Amazon.xlsx")
amazon$date = as.Date(amazon$Date, "%Y/%m/%d")
amazon <- amazon[order(amazon$date), ]
amazon$price = amazon$`Close/Last`
amazon$lag1_price = lag(amazon$price, n=1, order_by = amazon$date)
amazon$return = ((amazon$price - amazon$lag1_price)/amazon$lag1_price)*100
# Visualizing 
range(amazon$date)
### Line graph
ggplot(amazon, aes(x=date, y=return)) +
  geom_line() +
  theme(legend.position= "bottom") +
  labs(title = "Amazon daily returns",
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
hist(amazon$return,  
     labels = TRUE, 
     breaks=round(max(amazon$return, na.rm=TRUE) - min(amazon$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(amazon$return), na.rm=TRUE),round(max(amazon$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for amazon" )

# Histogram - Relative Frequency

h_amazon = hist(amazon$return,
         probability = TRUE,
         breaks=round(max(amazon$return, na.rm=TRUE) - min(amazon$return, na.rm=TRUE), 0), 
         xlim=c(min(floor(amazon$return), na.rm=TRUE),round(max(amazon$return, na.rm = TRUE),0)), 
         col="lightblue", 
         xlab="Return (%)", 
         ylab="Probability", 
         main="Distribution returns for amazon")

names(h_amazon)

hist(amazon$return,
     probability = TRUE,
     breaks=round(max(amazon$return, na.rm=TRUE) - min(amazon$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(amazon$return), na.rm=TRUE),round(max(amazon$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for amazon")
text(x = h_amazon$mids, y = h_amazon$density, labels = round(h_amazon$density,3), adj = 1, cex = 1.5, srt = 90)

##### Microsoft

microsoft <- read_excel("Lesson01-Microsoft.xlsx")
microsoft$date = as.Date(microsoft$Date, "%Y/%m/%d")
microsoft <- microsoft[order(microsoft$date), ]
microsoft$price = microsoft$`Close/Last`
microsoft$lag1_price = lag(microsoft$price, n=1, order_by = microsoft$date)
microsoft$return = ((microsoft$price - microsoft$lag1_price)/microsoft$lag1_price)*100
# Visualizing 
range(microsoft$date)
### Line graph
ggplot(microsoft, aes(x=date, y=return)) +
  geom_line() +
  theme(legend.position= "bottom") +
  labs(title = "Microsoft returns",
       subtitle = "(daily)", 
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
hist(microsoft$return,  
     labels = TRUE, 
     breaks=round(max(microsoft$return, na.rm=TRUE) - min(microsoft$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(microsoft$return), na.rm=TRUE),round(max(microsoft$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Frequency", 
     main="Distribution returns for microsoft" )

# Histogram - Relative Frequency

h_microsoft = hist(microsoft$return,
         probability = TRUE,
         breaks=round(max(microsoft$return, na.rm=TRUE) - min(microsoft$return, na.rm=TRUE), 0), 
         xlim=c(min(floor(microsoft$return), na.rm=TRUE),round(max(microsoft$return, na.rm = TRUE),0)), 
         col="lightblue", 
         xlab="Return (%)", 
         ylab="Probability", 
         main="Distribution returns for microsoft")

names(h_microsoft)

hist(microsoft$return,
     probability = TRUE,
     breaks=round(max(microsoft$return, na.rm=TRUE) - min(microsoft$return, na.rm=TRUE), 0), 
     xlim=c(min(floor(microsoft$return), na.rm=TRUE),round(max(microsoft$return, na.rm = TRUE),0)), 
     col="lightblue", 
     xlab="Return (%)", 
     ylab="Probability", 
     main="Distribution returns for microsoft")
text(x = h_microsoft$mids, y = h_microsoft$density, labels = round(h_microsoft$density,3), adj = 1, cex = 1.5, srt = 90)

###### Combining all results

summary(amazon$return)
summary(microsoft$return)
summary(tesla$return)

table1 = data.frame(stock = c("Amazon", "Microsoft", "Tesla"),
                    mean_r = c(mean(amazon$return, na.rm = TRUE),
                              mean(microsoft$return, na.rm = TRUE),
                              mean(tesla$return, na.rm = TRUE)),
                    var_r = c(var(amazon$return, na.rm = TRUE),
                              var(microsoft$return, na.rm = TRUE),
                              var(tesla$return, na.rm = TRUE)),
                    std_r = c(sd(amazon$return, na.rm = TRUE),
                              sd(microsoft$return, na.rm = TRUE),
                              sd(tesla$return, na.rm = TRUE)))
table1

table1$cv = table1$std_r / table1$mean_r

table1

# CV = volatility relative to the return (finance)
# CV = economic inequality (economics)
#"In finance, the coefficient of variation allows investors to determine how much volatility, or risk, 
# is assumed in comparison to the amount of return expected from investments.
# The lower the ratio of the standard deviation to mean return, the better the risk-return tradeoff."
# https://www.investopedia.com/terms/c/coefficientofvariation.asp


##### QUARTILES, PERCENTILES

# Positional measures
# n*p where
#           n = sample size
#           p = proportion

quantile(amazon$return, c(.25,.5,.75,.9), type=2, na.rm = TRUE) 
quantile(microsoft$return, c(.25,.5,.75,.9), type=2, na.rm = TRUE) 
quantile(tesla$return, c(.25,.5,.75,.9), type=2, na.rm = TRUE) 

##### BOXPLOTS


boxplot(amazon$return, horizontal = FALSE, 
        frame.plot = FALSE,
        main="Modified boxplot")

### APPENDING DATASETS

amazon$stock = "Amazon"
microsoft$stock = "Microsoft"
tesla$stock = "Tesla"

library(gtools)

# Re-formating tables as data frames

amazon = data.frame(amazon)
microsoft = data.frame(microsoft)
tesla = data.frame(tesla)


mydata = smartbind(amazon, microsoft, tesla)


boxplot(return ~ stock, data = mydata, horizontal = FALSE, 
        frame.plot = FALSE,
        main="Modified boxplot")


##### STATISTICAL CONTROL


ggplot(amazon, aes(x = date, y = return)) +
  geom_line(color = "lightblue") + 
  labs(title = "Daily return - Amazon", 
       y = "Return (%)",
       x = "Date",
       caption = "Source: https://www.nasdaq.com/") +
  geom_hline(yintercept = mean(amazon$return, na.rm = TRUE),
             linetype = 1, 
             size = 1,
             color = "black") +
  geom_hline(yintercept = mean(amazon$return, na.rm = TRUE)+sd(amazon$return, na.rm = TRUE)*2,
             linetype = 3, 
             size = 1,
             color = "black") +
  geom_hline(yintercept = mean(amazon$return, na.rm = TRUE)-sd(amazon$return, na.rm = TRUE)*2,
             linetype = 3, 
             size = 1,
             color = "black") +
  annotate(geom="text", 
           x=min(amazon$date, na.rm = TRUE), 
           y=mean(amazon$return, na.rm = TRUE)+0.33, 
           label=paste("Mean =",round(mean(amazon$return, na.rm = TRUE),3)),
           color="red",
           hjust = 0) + 
  annotate(geom="text", 
           x=min(amazon$date, na.rm = TRUE),  
           y=mean(amazon$return, na.rm = TRUE)+sd(amazon$return, na.rm = TRUE)*2+0.33, 
           label=paste("Upper control limit =",
                       round(mean(amazon$return, na.rm = TRUE)+sd(amazon$return, na.rm = TRUE)*2,3)),
           color="red",
           hjust = 0) +
  annotate(geom="text", 
           x=min(amazon$date, na.rm = TRUE),  
           y=mean(amazon$return, na.rm = TRUE)-sd(amazon$return, na.rm = TRUE)*2+0.33, 
           label=paste("Lower control limit =",
                       round(mean(amazon$return, na.rm = TRUE)-sd(amazon$return, na.rm = TRUE)*2,3)),
           color="red",
           hjust = 0) +
  scale_y_continuous(limits = c(min(amazon$return, na.rm = TRUE), max(amazon$return, na.rm = TRUE)),
                     breaks = seq(min(round(amazon$return,0), na.rm = TRUE), max(round(amazon$return, 0), na.rm = TRUE), by = 1)) + 
  scale_x_continuous(limits = c(min(amazon$date), max(amazon$date)),
                     breaks = seq(min(amazon$date), max(amazon$date), by = 7)) +
  theme(plot.title= element_text(hjust= 0.5),
        axis.text.x= element_text(angle = 45, hjust= 1))


ggplot(microsoft, aes(x = date, y = return)) +
  geom_line(color = "lightblue") + 
  labs(title = "Daily return - Microsoft", 
       y = "Return (%)",
       x = "Date",
       caption = "Source: https://www.nasdaq.com/") +
  geom_hline(yintercept = mean(microsoft$return, na.rm = TRUE),
             linetype = 1, 
             size = 1,
             color = "black") +
  geom_hline(yintercept = mean(microsoft$return, na.rm = TRUE)+sd(microsoft$return, na.rm = TRUE)*2,
             linetype = 3, 
             size = 1,
             color = "black") +
  geom_hline(yintercept = mean(microsoft$return, na.rm = TRUE)-sd(microsoft$return, na.rm = TRUE)*2,
             linetype = 3, 
             size = 1,
             color = "black") +
  annotate(geom="text", 
           x=min(microsoft$date, na.rm = TRUE), 
           y=mean(microsoft$return, na.rm = TRUE)+0.33, 
           label=paste("Mean =",round(mean(microsoft$return, na.rm = TRUE),3)),
           color="red",
           hjust = 0) + 
  annotate(geom="text", 
           x=min(microsoft$date, na.rm = TRUE),  
           y=mean(microsoft$return, na.rm = TRUE)+sd(microsoft$return, na.rm = TRUE)*2+0.33, 
           label=paste("Upper control limit =",
                       round(mean(microsoft$return, na.rm = TRUE)+sd(microsoft$return, na.rm = TRUE)*2,3)),
           color="red",
           hjust = 0) +
  annotate(geom="text", 
           x=min(microsoft$date, na.rm = TRUE),  
           y=mean(microsoft$return, na.rm = TRUE)-sd(microsoft$return, na.rm = TRUE)*2+0.33, 
           label=paste("Lower control limit =",
                       round(mean(microsoft$return, na.rm = TRUE)-sd(microsoft$return, na.rm = TRUE)*2,3)),
           color="red",
           hjust = 0) +
  scale_y_continuous(limits = c(min(microsoft$return, na.rm = TRUE), max(microsoft$return, na.rm = TRUE)),
                     breaks = seq(min(round(microsoft$return,0), na.rm = TRUE), max(round(microsoft$return, 0), na.rm = TRUE), by = 1)) + 
  scale_x_continuous(limits = c(min(microsoft$date), max(microsoft$date)),
                     breaks = seq(min(microsoft$date), max(microsoft$date), by = 7)) +
  theme(plot.title= element_text(hjust= 0.5),
        axis.text.x= element_text(angle = 45, hjust= 1))


ggplot(tesla, aes(x = date, y = return)) +
  geom_line(color = "lightblue") + 
  labs(title = "Daily return - tesla", 
       y = "Return (%)",
       x = "Date",
       caption = "Source: https://www.nasdaq.com/") +
  geom_hline(yintercept = mean(tesla$return, na.rm = TRUE),
             linetype = 1, 
             size = 1,
             color = "black") +
  geom_hline(yintercept = mean(tesla$return, na.rm = TRUE)+sd(tesla$return, na.rm = TRUE)*2,
             linetype = 3, 
             size = 1,
             color = "black") +
  geom_hline(yintercept = mean(tesla$return, na.rm = TRUE)-sd(tesla$return, na.rm = TRUE)*2,
             linetype = 3, 
             size = 1,
             color = "black") +
  annotate(geom="text", 
           x=min(tesla$date, na.rm = TRUE), 
           y=mean(tesla$return, na.rm = TRUE)+0.66, 
           label=paste("Mean =",round(mean(tesla$return, na.rm = TRUE),3)),
           color="red",
           hjust = 0) + 
  annotate(geom="text", 
           x=min(tesla$date, na.rm = TRUE),  
           y=mean(tesla$return, na.rm = TRUE)+sd(tesla$return, na.rm = TRUE)*2+0.66, 
           label=paste("Upper control limit =",
                       round(mean(tesla$return, na.rm = TRUE)+sd(tesla$return, na.rm = TRUE)*2,3)),
           color="red",
           hjust = 0) +
  annotate(geom="text", 
           x=min(tesla$date, na.rm = TRUE),  
           y=mean(tesla$return, na.rm = TRUE)-sd(tesla$return, na.rm = TRUE)*2+0.66, 
           label=paste("Lower control limit =",
                       round(mean(tesla$return, na.rm = TRUE)-sd(tesla$return, na.rm = TRUE)*2,3)),
           color="red",
           hjust = 0) +
  scale_y_continuous(limits = c(min(tesla$return, na.rm = TRUE), max(tesla$return, na.rm = TRUE)),
                     breaks = seq(min(round(tesla$return,0), na.rm = TRUE), max(round(tesla$return, 0), na.rm = TRUE), by = 1)) + 
  scale_x_continuous(limits = c(min(tesla$date), max(tesla$date)),
                     breaks = seq(min(tesla$date), max(tesla$date), by = 7)) +
  theme(plot.title= element_text(hjust= 0.5),
        axis.text.x= element_text(angle = 45, hjust= 1))






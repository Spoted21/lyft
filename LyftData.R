# Load libraries ----
library(lubridate) 
library(sqldf)

# Read in Data ---
# lyft <- read.csv(file="https://raw.githubusercontent.com/Spoted21/lyft/master/lyft2.csv")
lyft <- read.csv( 
  file="/home/spoted21/Documents/R/lyft/lyft2.csv",
  stringsAsFactors = FALSE
  )

# Examine Data ----
head(lyft)
dim(lyft)
str(lyft)


# Calculate StartTime and EndTime ----
lyft$StartTime <- as.POSIXct(
  paste(
    strptime(lyft$Date,format = "%m/%d/%Y"),
    format(strptime(lyft$Time, "%I:%M %p"), format="%H:%M:%S")
  ),format="%Y-%m-%d %H:%M:%OS")

lyft$EndTime <-lyft$StartTime+(lyft$Time_Min*60+lyft$Time_Sec)

################################################################################
# Calculate a driving session ----
# If more than 4 hours since last ride a new session 
# is assumed to have started
################################################################################


lyft$rideSession <- 0

for(i in 1:nrow(lyft) ){
  #First Row - no comparison
  if(i==1) {lyft$rideSession[1] <- 1 } else if(i== nrow(lyft) ){ #Last Row
    lyft$rideSession[i] <- lyft$rideSession[i-1] }  else {
      timedifference <- as.numeric(difftime( lyft[i+1,]$StartTime, lyft[i,]$EndTime,units = "mins"))
      if(timedifference <= (60*4) ) { lyft$rideSession[i] <-lyft$rideSession[i-1] } else {
        lyft$rideSession[i] <- max(lyft$rideSession)+1 }
    }
}

lyft$TotalMoney <- lyft$Amount + lyft$Tip
lyft$starthour <- hour(lyft$StartTime)
# Night time driving 5pm to 3AM
night <- lyft[lyft$starthour %in% c(17:23,(0:3)) , ]

# Used for formatting the plot
HourLabels <- data.frame(hour = 0L:23L, label =
                           c(paste0(c(12,1:11),"AM") ,
                             paste0(c(12,1:11),"PM"))
)
# Calculate the time while waiting for rides as well 
# as giving rides at the week level

# Assuming no collisions, take the time driven as unique and 
# then find the min and max dates for each amount of time 
lyft$HoursOnClock <-lyft$HoursLoggedIn+(lyft$MinutesLoggedIn/60)+(lyft$SecondsLoggedIn/60/60)

# sqldf(" Select distinct 
#       HoursOnClock,
#       rideSession
#       From 
#       lyft
#       Order By 
#       rideSession
#       ")

hoursSpentDriving <- round(sum(unique(lyft$HoursOnClock ),na.rm = T),2)
moneyEarnedDriving <- round(sum(lyft$Amount+lyft$Tip),2)
moneyPerHour <- round(moneyEarnedDriving/hoursSpentDriving,2)


#Distribution of Money Made by Ride hour
byHour <- sqldf("Select starthour
                From lyft
                group BY starthour
                Having Count(rideSession)> 5 
                ")


sqldf("Select Count(*),starthour
      From lyft
      group BY starthour
      order by starthour
      ")
#Distribution of Money Made by start hour ----
plotData <- night
myLabels <- HourLabels[HourLabels$hour %in% unique(plotData$starthour),]$label 
myColors <- c("lightblue","wheat","lightgreen","gray")
totalrides <- nrow(night)
with(
  plotData,
  boxplot(
    TotalMoney ~ starthour ,
    col= myColors,
    las=1,
    main=paste0("Distribution of Money Made by Start Hour\n",
                "(n = ",totalrides,")") ,
    names=myLabels,
    yaxt="n"
  )
)#End With Statement
# Add formatted axis 
axis(side=2,
     at = axTicks(2),
     labels =paste0("$ ",axTicks(2)),
     las=1)

# mtext(text = paste0("Based upon ",totalrides," rides"),
#       side =1,
#       outer =FALSE ,
#       line = 3,
#       cex=1.25
#       )

# for the tidyverse fans
library(tidyverse)
plotData$plotColor <- myColors
p <- ggplot(plotData , aes( factor(starthour),TotalMoney ),
            fill= plotColor) +
  geom_boxplot() +
  scale_x_discrete(labels=myLabels) 
  # scale_fill_manual(name="foo",values=c("red","blue") ) 
p
# library(plotly)
# ggplotly(p)

# Looking at Data by Day 
lyft$day <- weekdays(lyft$StartTime)

dayData <- sqldf("Select 
                  count(*) Rides ,
                  sum(TotalMoney) as TotalMoney ,
                  starthour,
                 day ,
              case 
                   when day='Monday'   then 1 
                   when day='Tuesday'  then 2 
                   when day='Wednesday'  then 3
                   when day='Thursday'  then 4
                   when day='Friday'  then 5
                   when day='Saturday'  then 6
                   when day='Sunday'  then 7
              end as DayOrder
                 From 
                    lyft 
                 group by 
                  day ,
                  starthour
                 order by DayOrder") 

totalrides2 <- sum(dayData$Rides)
with(
  dayData,
  boxplot(
    TotalMoney ~ day ,
    col= myColors,
    las=1,
    main=paste0("Distribution of Money Made by day \n",
                "(n = ",totalrides2,")") ,
    # names=myLabels,
    yaxt="n"
  )
)
# Add formatted axis 
axis(side=2,
     at = axTicks(2),
     labels =paste0("$ ",axTicks(2)),
     las=1)


# build simple model to estimate earnings given day of week and start hour

# Question
# If I drive at 5pm on a Friday, what would I expect to make?

# Start with a linear model
simpleModel <- lm(TotalMoney ~ factor(day) + factor(starthour) ,data=lyft)

simpleModel
summary(simpleModel)

lyft[lyft$day=="Friday" & lyft$starthour==17,]
getStats <- function(day,starthour) {
  
  
}

# Read in Data
# foo <- read.csv(file = "http://www.beardedanalytics.com/todd/lyft.csv")
foo <- read.csv(file="https://raw.githubusercontent.com/Spoted21/lyft/master/lyft2.csv")

head(foo)
# Get Time into reasonable format
foo$StartTime <- as.POSIXct(
  paste(strptime(foo$Date,format = "%m/%d/%Y"),
        format(strptime(foo$Time, "%I:%M %p"), format="%H:%M:%S")
  ),format="%Y-%m-%d %H:%M:%OS")

library(lubridate)
foo$EndTime <-foo$StartTime+(foo$Time_Min*60+foo$Time_Sec)
foo$duration <- difftime(foo$EndTime,foo$StartTime,units = "min")
foo$duration <- difftime(foo$EndTime,foo$StartTime,units = "hour")


# Need to calculate driving session
# If more than 4 hours since last ride a new session 
# is assumed to have started
foo$timeBetweenRides <-""
# foo$rideSession <-as.numeric(as.na())
str(foo)
head(foo)
# Get this working
for(i in 1:nrow(foo) ){
  print(i)
  #First Row - no comparison
  if(i==1) {foo$rideSession[1] <- 1 } else if(i== nrow(foo) ){ #Last Row
    foo$rideSession[i] <- foo$rideSession[i-1] }  else {
      timedifference <- as.numeric(difftime(foo[i+1,]$StartTime, foo[i,]$EndTime,units = "mins"))
      if(timedifference <= (60*4) ) { foo$rideSession[i] <-foo$rideSession[i-1] } else {
        foo$rideSession[i] <- max(foo$rideSession)+1 }
    }
}

foo$TotalMoney <- foo$Amount + foo$Tip
#Last Row - same Ride session as previous record
#Everything Else

#Distribution of Money Made by Ride Session
with(foo, boxplot(TotalMoney ~ rideSession ,
          col=c("lightblue","wheat","red"),
          las=1,main="Distribution of Money Made\n by Ride Session") )

# Check a flat one
foo[foo$rideSession==10,]


#Figure out the money made per hour of day
# use starting hour

foo$starthour <- hour(foo$StartTime)
#Distribution of Money Made by start hour
with(foo, boxplot(TotalMoney ~ starthour ,
                  col=c("lightblue","wheat","red"),
                  las=1,main="Distribution of Money Made\n by Start Hour") )



# Calculate the time while waiting for rides as well 
# as giving rides at the week level

# Assuming no collisions, take the time drive as unique and 
#then find the min and max dates for each amount of time 
foo$HoursOnClock <-foo$HoursLoggedIn+(foo$MinutesLoggedIn/60)+(foo$SecondsLoggedIn/60/60)

hoursSpentDriving <- round(sum(unique(foo$HoursOnClock ),na.rm = T),2)
moneyEarnedDriving <- round(sum(foo$Amount+foo$Tip),2)
moneyPerHour <- round(moneyEarnedDriving/hoursSpentDriving,2)
#Print Summary
paste0("Total time Spent = ",hoursSpentDriving," hours")
paste0("Total Money Earned before Expenses = $",moneyEarnedDriving)
paste0("Earnings Per Hour Before Expenses = $",moneyPerHour)
paste0("Earnings Per Ride Before Expenses = $",round(moneyEarnedDriving/max(foo$RideNumber),2))

HourLabels <- cbind(hour = 0:23, label =
                      c(paste0(c(12,1:11),"AM") ,
                        paste0(c(12,1:11),"PM"))
)

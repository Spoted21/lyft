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
lyft$HoursOnClock <-lyft$HoursLoggedIn+(lyft$MinutesLoggedIn/60)+
  (lyft$SecondsLoggedIn/60/60)


hoursSpentDriving <- round(sum(unique(lyft$HoursOnClock ),na.rm = T),2)
moneyEarnedDriving <- round(sum(lyft$Amount+lyft$Tip),2)
moneyPerHour <- round(moneyEarnedDriving/hoursSpentDriving,2)


#Distribution of Money Made by Ride hour
byHour <- sqldf("Select starthour
                From lyft
                group BY starthour
                Having Count(rideSession)> 5 
                ")


#Distribution of Money Made by start hour ----
plotData <- night
myLabels <- HourLabels[HourLabels$hour %in% unique(plotData$starthour),]$label 
myColors <- c("lightblue","wheat","lightgreen","gray")
totalrides <- nrow(night)

# Make Boxplot To Show Money Made by Hour ----
png(filename = "MoneyByHour.png")
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
dev.off()



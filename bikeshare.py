#1 Popular times of travel (i.e., occurs most often in the start time)
#What is the most common month?
#The most common month for bike rentals from the first 6 months of 2017 was June from all three cities with a total of 37151 rentals.

library(dplyr)
library(stringr)

#Establish combined, trimmed table
trim.ny <- ny[-c(8,9)]
trim.chi <- chi[-c(8,9)]
trim.nychi <- rbind(trim.ny,trim.chi)
df = rbind(trim.nychi,wash)

#Extract month out of the table
df.time <- df %>%
  mutate(Start.Time = as.POSIXct(strptime(df$Start.Time, "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(start.month = format(Start.Time, "%B"))
df.time$start.month=factor(df.time$start.month,levels=c("January","February","March","April","May","June"))

#Rental count per month
month.count = table(df.time$start.month)

#Build graph
bike.bar = barplot(month.count, ylim = c(0,40000), xlab = "Months in 2017", ylab = "Count of Rides", main = "Bike Rentals by Month")
text (x = bike.bar, y = month.count, label = month.count, pos = 1, cex = 1, col = "Blue")

#What is the most common day of week?
#The most common day of the week for bike rentals from the first 6 months of 2017 was Wednesday from all three cities with a total of 25080 rentals.

library(dplyr)
library(stringr)

#Establish combined, trimmed table
trim.ny <- ny[-c(8,9)]
trim.chi <- chi[-c(8,9)]
trim.nychi <- rbind(trim.ny,trim.chi)
df = rbind(trim.nychi,wash)

#Extract weekday out of the table
df.time <- df %>%
  mutate(Start.Time = as.POSIXct(strptime(df$Start.Time, "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(start.day = weekdays(Start.Time))
df.time$start.day=factor(df.time$start.day,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#Rental count per weekday
weekday.count = table(df.time$start.day)

#Build graph
bike.bar = barplot(weekday.count, ylim = c(0,30000), xlab = "Day of the Week", ylab = "Count of Rides", main = "Bike Rentals by Day in First Half of 2017")
text (x = bike.bar, y = weekday.count, label = weekday.count, pos = 1, cex = 1, col = "Blue")

#What is the most common hour of day?
#The most common hour of the day for bike rentals from the first 6 months of 2017 was 8:00AM from all three cities with a total of 14835 rentals.

library(dplyr)
library(stringr)

#Establish combined, trimmed table
trim.ny <- ny[-c(8,9)]
trim.chi <- chi[-c(8,9)]
trim.nychi <- rbind(trim.ny,trim.chi)
df = rbind(trim.nychi,wash)

#Extract weekday out of the table
df.time <- df %>%
  mutate(Start.Time = as.POSIXct(strptime(df$Start.Time, "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(start.hour = format(Start.Time, "%H"))
df.time$start.hour=factor(df.time$start.hour)

#Rental count per weekday
hour.count = table(df.time$start.hour)

#Build graph
bike.bar = barplot(hour.count, ylim = c(0,16000), xlab = "Hour of the Day", ylab = "Count of Rentals", main = "Bike Rentals by Hour in First Half of 2017")
text (x = bike.bar, y = hour.count, label = hour.count, pos = 3, cex = 0.5, col = "Blue")



#2 Popular stations and trip
#What is the most common start station?
#The most common start station from the first 6 months of 2017 was Columbus Circle / Union Station from all three cities with a total of 1700 rentals starting from this station.

library(dplyr)
library(stringr)
library(ggplot2)

#Establish combined, trimmed table
trim.ny <- ny[-c(8,9)]
trim.chi <- chi[-c(8,9)]
trim.nychi <- rbind(trim.ny,trim.chi)
df = rbind(trim.nychi,wash)

#Organize by Start Station
df.station = df %>%
  group_by(Start.Station) %>%
  summarize(Start.Station.Count=n()) %>%
  top_n(5)

#Plot
df.plot = ggplot(df.station, aes(x=reorder(Start.Station,-Start.Station.Count),y=Start.Station.Count))+
  geom_bar(stat="identity")+
  coord_cartesian(xlim=c(1,5),ylim=c(0,2000))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  labs(x="Start Station",y="Rental Count",title="Top 5 Starting Stations")+
  geom_text(aes(x=reorder(Start.Station,-Start.Station.Count),y=Start.Station.Count,label=Start.Station.Count),vjust=2)

df.plot

#What is the most common end station?
#The most common end station from the first 6 months of 2017 was Columbus Circle / Union Station from all three cities with a total of 1767 rentals ending at this station.

library(dplyr)
library(stringr)
library(ggplot2)

#Establish combined, trimmed table
trim.ny <- ny[-c(8,9)]
trim.chi <- chi[-c(8,9)]
trim.nychi <- rbind(trim.ny,trim.chi)
df = rbind(trim.nychi,wash)

#Organize by End Station
df.station = df %>%
  group_by(End.Station) %>%
  summarize(End.Station.Count=n()) %>%
  top_n(5)

#Plot
df.plot = ggplot(df.station, aes(x=reorder(End.Station,-End.Station.Count),y=End.Station.Count))+
  geom_bar(stat="identity")+
  coord_cartesian(xlim=c(1,5),ylim=c(0,2000))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  labs(x="End Station",y="Rental Count",title="Top 5 Ending Stations")+
  geom_text(aes(x=reorder(End.Station,-End.Station.Count),y=End.Station.Count,label=End.Station.Count),vjust=2)
df.plot


#What is the most common trip from start to end (i.e., most frequent combination of start station and end station)?
#The most common starting and ending station combination from the first 6 months of 2017 was Jefferson Dr & 14th St SW to Jefferson Dr & 14th St SW from all three cities with a total of 198 rentals with this combination.

library(dplyr)
library(stringr)
library(ggplot2)

#Establish combined, trimmed table
trim.ny <- ny[-c(8,9)]
trim.chi <- chi[-c(8,9)]
trim.nychi <- rbind(trim.ny,trim.chi)
df = rbind(trim.nychi,wash)

#Combine start and end stations
df$combo  <- paste(df$Start.Station,"-",df$End.Station)

#Organize by End Station
df.station = df %>%
  mutate(combo=gsub("-","\n",combo))%>%
  group_by(combo) %>%
  summarize(combo.count=n()) %>%
  top_n(5)

#Plot
df.plot = ggplot(df.station, aes(x=reorder(combo,combo.count),y=combo.count))+
  geom_bar(stat="identity")+
  coord_cartesian(xlim=c(1,5),ylim=c(0,200))+
  theme(axis.text.x = element_text(hjust=1))+
  labs(x="Start and End Station",y="Rental Count",title="Top 5 Start to End Stations")+
  geom_text(aes(x=reorder(combo,combo.count),y=combo.count,label=combo.count),hjust=2)+
  coord_flip()
df.plot



#3 Trip duration
#What is the total travel time for users in different cities?
#The total rental time from the first 6 months of 2017 was 2247 hours in Chicago, 13747 hours in New York, and 30511 hours in Washington

library(dplyr)
library(stringr)
library(ggplot2)

#Get sums
chi.sum  <- round(sum(as.integer(chi$Trip.Duration),na.rm=TRUE)/3600,0)
ny.sum  <- round(sum(as.integer(ny$Trip.Duration),na.rm=TRUE)/3600,0)
wash.sum  <- round(sum(as.integer(wash$Trip.Duration),na.rm=TRUE)/3600,0)
city.sum <- c(chi.sum,ny.sum,wash.sum)
cities  <- c("Chicago","New York","Washington")

#Create data frame
df.sum = data.frame(cities,city.sum)

#Plot data frame
df.plot = ggplot(df.sum, aes(x=cities,y=city.sum))+
  geom_bar(stat="identity")+
  coord_cartesian()+
  labs(x="City Name",y="Duration (hrs)",title="Total Rental Duration in Hours")+
  geom_text(aes(x=reorder(cities,-city.sum),y=city.sum,label=city.sum),vjust=2)
df.plot

#What is the average travel time for users in different cities?
#The average rental time from the first 6 months of 2017 was 15.62 minutes in Chicago, 15.06 minutes in New York, and 20.56 minutes in Washington.

library(dplyr)
library(stringr)
library(ggplot2)

#Get averages
chi.avg  <- round(mean(as.integer(chi$Trip.Duration),na.rm=TRUE)/60,2)
ny.avg  <- round(mean(as.integer(ny$Trip.Duration),na.rm=TRUE)/60,2)
wash.avg  <- round(mean(as.integer(wash$Trip.Duration),na.rm=TRUE)/60,2)
city.avg <- c(chi.avg,ny.avg,wash.avg)
cities  <- c("Chicago","New York","Washington")

#Create data frame
df.avg = data.frame(cities,city.avg)

#Plot data frame
df.plot = ggplot(df.avg, aes(x=cities,y=city.avg))+
  geom_bar(stat="identity")+
  coord_cartesian()+
  labs(x="City Name",y="Duration (min)",title="Average Rental Duration in Minutes")+
  geom_text(aes(x=reorder(cities,-city.avg),y=city.avg,label=city.avg),vjust=2)
df.plot

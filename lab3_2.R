rm(list = ls())

set.seed(12345)
library(geosphere)
stations = read.csv("stations.csv", fileEncoding = "latin1")
temps = read.csv("temps50k.csv")
st = merge(stations,temps,by="station_number")

# These three values are up to the students
h_distance <- 100000 #small h -> overfitting, big h -> underfitting
h_date <- 5000
h_time <- 2

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826 #longitude and latitude
date <- "2013-11-04" # The date to predict (up to the students)

#times <- c("04:00:00", "06:00:00", ..., "24:00:00") THIS DIDNT WORK??
times <- seq(from = 4, to = 24, by = 2)
temp <- vector(length=length(times))

# Students’ code here -----------------------------
st_filtered = st[st$date <= date, ] #filter data to exlude future points

#Gaussian kernel = e⁻(∣∣u∣∣²), u = (x*-xi)/h

#Kernel 1 
distance = distHaversine(matrix(c(st_filtered$longitude, st_filtered$latitude), ncol = 2), c(b, a)) #calculates euclidean norm of x*-xi
st_filtered$kernel_distance = exp(-(distance^2) / (h_distance^2))

#Kernel 2
diff = as.numeric(difftime(date, st_filtered$date, units = "days"))
#hist(diff, main="Distribution of Date Differences", xlab="Days Difference", breaks=50)
#hist used to see distribution, to choose h

st_filtered$kernel_date = exp(-(diff^2) / (h_date^2))

#Kernel 3
hours = as.numeric(format(as.POSIXct(st_filtered$time, format="%H:%M:%S"), "%H"))

for (i in 1:length(times)) {
  diff = abs(hours - times[i])
  diff[diff > 12] = 24 - diff[diff > 12]  # Adjust for wraparound (e.g., 23:00 and 01:00 should be close)
  column_name = paste("kernel_time_", times[i], sep="")
  st_filtered[column_name] =  exp(-(diff^2) / h_time^2)
}

#Sum Kernels
for (i in 1:length(times)){
  column_name_time = paste("kernel_time_", times[i], sep="")
  kernel_sum = st_filtered$kernel_distance + st_filtered$kernel_date + st_filtered[column_name_time]
  temp[i] = sum(st_filtered$air_temperature * kernel_sum) / sum(kernel_sum)
}

#product kernels 


plot(temp, type="o", 
     main=paste("Predicted Temperature for", date),
     xlab="Time (Hours)", ylab="Temperature (°C)", xaxt="n")

# Add custom x-axis labels (4, 6, 8, ..., 24)
axis(1, at=1:length(times), labels=times)





rm(list = ls())

set.seed(1234567890)
library(geosphere)
stations = read.csv("stations.csv", fileEncoding = "latin1")
temps = read.csv("temps50k.csv")
st = merge(stations,temps,by="station_number")

# These three values are up to the students
h_distance <- 100000 #small h -> overfitting, big h -> underfitting
h_date <- 5000
h_time <- 2

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)

#times <- c("04:00:00", "06:00:00", ..., "24:00:00") #THIS DIDNT WORK??
times <- seq(from = 4, to = 24, by = 2)
temp_sum <- vector(length=length(times))
temp_prod <- vector(length=length(times))

# Students’ code here -----------------------------
st_filtered = st[st$date <= date, ] #filter data to exlude future points

#Gaussian kernel = e⁻(∣∣u∣∣²), u = (x*-xi)/h

#Kernel 1 
distance = distHaversine(matrix(c(st_filtered$longitude, st_filtered$latitude), ncol = 2), c(b, a)) #calculates euclidean norm of x*-xi
st_filtered$kernel_distance = exp(-(distance^2) / (h_distance^2))

plot(distance, st_filtered$kernel_distance, type="p", 
     main="Kernel Values vs. Distance (Spatial)",
     xlab="Distance (meters)", ylab="Kernel Value",
     col="blue", pch=16)

#Kernel 2
diff = as.numeric(difftime(date, st_filtered$date, units = "days"))

st_filtered$kernel_date = exp(-(diff^2) / (h_date^2))

plot(diff, st_filtered$kernel_date, type="p",
     main="Kernel Values vs. Date Difference (Temporal)",
     xlab="Days Difference", ylab="Kernel Value",
     col="blue", pch=16)


#Kernel 3
hours = as.numeric(format(as.POSIXct(st_filtered$time, format="%H:%M:%S"), "%H"))

for (i in 1:length(times)) {
  diff = abs(hours - times[i])
  diff[diff > 12] = 24 - diff[diff > 12]    # Adjust for wraparound (e.g., 23:00 and 01:00 should be close)
  column_name = paste("kernel_time_", times[i], sep="")
  st_filtered[column_name] =  exp(-(diff^2) / h_time^2)
}

diff = abs(hours - times [5])
diff[diff > 12] = 24 - diff[diff > 12]   
plot(diff, st_filtered$kernel_time_12, type="p",
     main="Kernel Values vs. Time Difference (Temporal)",
     xlab="Time Difference, x* = 12", ylab="Kernel Value",
     col="blue", pch=16)


#Sum Kernels and prod kernels
for (i in 1:length(times)){
  column_name_time = paste("kernel_time_", times[i], sep="")
  
  kernel_sum = st_filtered$kernel_distance + st_filtered$kernel_date + st_filtered[column_name_time]
  temp_sum[i] = sum(st_filtered$air_temperature * kernel_sum) / sum(kernel_sum)
  
  kernel_prod = st_filtered$kernel_distance * st_filtered$kernel_date * st_filtered[column_name_time]
  temp_prod[i] = sum(st_filtered$air_temperature * kernel_prod) / sum(kernel_prod)
}

y_min <- min(c(temp_sum, temp_prod)) - 1
y_max <- max(c(temp_sum, temp_prod)) + 1  

plot(temp_sum, type="o", col="blue", 
     main=paste("Predicted Temperature for", date),
     xlab="Time (Hours)", ylab="Temperature (°C)", 
     xaxt="n", ylim=c(y_min, y_max))  

# Add custom x-axis labels (4, 6, 8, ..., 24)
axis(1, at=1:length(times), labels=times)

# Add the temperatures from the product of kernels (in a different color)
lines(temp_prod, type="o", col="red")

legend("topright", legend=c("Sum of Kernels", "Product of Kernels"), 
       col=c("blue", "red"), pch=1)




library(goftest)
library(Directional)
library(circular)
library(protr)
library(tidyverse)
library(leaflet)
library(Matrix)
library(mixtools)
library(spam)
library(ggplot2)
library(SpatialVx)
library(skmeans)
library(NPCirc)
library(rgl)
library(flexmix)
library(slam)
library(mclust)
library(lattice)
library(mvdalab)
library(Amelia)
library(fields)
library(maps)
library(maptools)
library(tm)
#  Meteor lalnding(1) dataset 
library(lubridate)
library(readr)
head(data)
cneos_fireball_data_final <- read_csv("cneos_fireball_data_final.csv")
fireball = data.frame(cneos_fireball_data_final)
head(fireball)
final_data = fireball[1:764,]
tail(final_data)
head(final_data)
final_data = data.frame(final_data$Peak.Brightness.Date.Time..UT. , final_data$Latitude , final_data$long , final_data$Altitude..km.,
                        final_data$Velocity..km.s. , final_data$vx , final_data$vy , final_data$vz , final_data$Total.Radiated.Energy..J.,
                        final_data$Calculated.Total.Impact.Energy..kt.)
head(final_data)
watson.test(final_data$final_data.Latitude , alpha = 0.05, dist = "vonmises")
watson.test(final_data$final_data.long , alpha = 0.05 , dist = "vonmises")


library(maps)
library(mice)

imputed_data <- mice(final_data, method = "lasso.norm", m = 5, maxit = 50)
completed_data <- complete(imputed_data)
#is.numeric(completed_data[, -1]$long)
head(completed_data)


head(completed_data)
#completed_data = completed_data[,-1]


date <- as.Date(completed_data$date, format = "%d-%m-%Y")

# Extract the year from the date
year <- format(date, "%Y")

# Create a new date object with only the year
startOfYear <- as.Date(paste0(year, "-01-01"))


# Calculate the number of days between the date and the start of the year
dayOfYear <- as.numeric(date - startOfYear) + 1

# Print the number of days in the year
print(dayOfYear)

new_data = data.frame(completed_data , dayOfYear)
head(new_data)
new_data$delta = 23.45*sin(0.986*(new_data$dayOfYear + 284))
new_data$h = asin(sin(new_data$final_data.Latitude)*sin(new_data$delta) + cos(new_data$final_data.Latitude)*cos(new_data$delta))
new_data$The_turbidity_factor = 0.796 - 0.01*sin(0.986*(new_data$dayOfYear + 284))       
head(new_data)
new_data$C_t = 1 + 0.034*cos(new_data$dayOfYear - 2)
new_data$I_h = 1367*new_data$C_t*new_data$The_turbidity_factor*exp(-0.13/sin(new_data$h))*sin(new_data$h)
head(new_data)
new_data$D_h = 120*new_data$The_turbidity_factor*exp(-1/(0.4511+ sin(new_data$h)))
new_data$G_h = new_data$I_h + new_data$D_h
head(new_data)
new_data_final = new_data[, c(2:13, 20)]
head(new_data_final)
sum(is.na(new_data_final))


atmospheric_density <- function(altitude, sea_level_density, scale_height) {
  density <- sea_level_density * exp(-altitude / scale_height)
  return(density)
}

# Example usage
altitude <- new_data_final$final_data.Altitude..km. * 1000  # Altitude in meters
sea_level_density <- 1.225  # Density at sea level in kg/m³
scale_height <- 8000  # Scale height in meters

# Calculate atmospheric density
new_data_final$density <- atmospheric_density(altitude, sea_level_density, scale_height)
head(new_data_final)


completed_data$date = as.POSIXct(completed_data$date)
head(completed_data)

library(readr)
dataset = read_csv("Meteorite_Landings.csv")
dataset = data.frame(dataset)
head(dataset)
watson.test(dataset$reclat , alpha = 0.01 , dist = "vonmises")
watson.test(dataset$reclong , alpha = 0.01 , dist = "vonmises")
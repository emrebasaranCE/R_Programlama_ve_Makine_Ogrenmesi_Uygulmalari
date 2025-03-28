# Sample dataset: 365 daily temperature data (for one year)
sicaklik <- c(rnorm(365, mean = 15, sd = 10))  # Mean of 15°C, standard deviation of 10°C

# Let's define the months (divide the 365 daily data into 12 months)
aylar <- rep(1:12, each = 30, length.out = length(sicaklik))

# Calculate the average temperature for each month
aylik_ortalama <- tapply(sicaklik, aylar, mean)

# Print out aylik_ortalama variable
aylik_ortalama

# Find the month with the highest average temperature
G1 <- which.max(aylik_ortalama)

# Show the result
G1

# Find the days with temperatures below 0°C
G2 <- which(sicaklik < 0)

# Show the result
G2

# Convert to Fahrenheit
G3_sicaklikFahrenheit  <- (sicaklik * 9/5) + 32

# Show the result
head(G3_sicaklikFahrenheit)

# Find the number of days with temperatures above 25°C
G4Bonus <- sum(sicaklik > 25)

# Show the result
G4Bonus

# Get summary statistics of the data

# Visualize the temperature data as a line chart
plot(sicaklik, type = "l", col = "blue", xlab = "Day", ylab = "Temperature (°C)", main = "Temperature Throughout the Year")

# Visualize the temperature distribution as a histogram
hist(sicaklik, col = "green", main = "Temperature Distribution", xlab = "Temperature (°C)", breaks = 20)

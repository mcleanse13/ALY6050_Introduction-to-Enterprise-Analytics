#ALY 6050 Module 4 Project
#Part 1
# Function to calculate Economic Order Quantity (EOQ)
calculate_EOQ <- function(D, S, H) {
  EOQ <- sqrt((2 * D * S) / H)
  return(EOQ)
}

# Function to calculate total inventory cost
calculate_total_cost <- function(D, S, H, EOQ) {
  # Calculate annual ordering cost
  AOC <- (D / EOQ) * S
  
  # Calculate annual holding cost
  AHC <- (EOQ / 2) * H
  
  # Total inventory cost
  total_cost <- AOC + AHC
  
  return(total_cost)
}

# Given data
D <- 15000  # Annual demand
S <- 220    # Ordering cost per order
H <- 80 * 0.18  # Holding cost per unit per year (18% of unit cost)

# Calculate EOQ
EOQ <- calculate_EOQ(D, S, H)

# Calculate total inventory cost
total_cost <- calculate_total_cost(D, S, H, EOQ)

# Print the results
cat("Economic Order Quantity (EOQ):", EOQ, "\n")
cat("Total inventory cost:", total_cost, "\n")

#Part 2
install.packages("triangle")
install.packages("RTriangle")
library(triangle)

# Set parameters and run simulation consisting of 1000 occurrences and calculate the minimum total cost for 
#each occurrence
n_simulations <- 1000
demand_samples <- rtriangle(n_simulations, a = 13000, b = 17000, c = 15000)
unit_cost <- 80
holding_cost_rate <- 14.40
ordering_cost <- 220

#Function to calculate total cost
calculate_total_cost <- function(demand) {
  order_quantity <- sqrt(2 * demand * ordering_cost / holding_cost_rate)
  total_cost <- (unit_cost * demand + ordering_cost * n_simulations) / n_simulations
  return(total_cost)
}

# Perform simulation
total_costs <- sapply(demand_samples, calculate_total_cost)

#i. Estimate expected minimum total cost
expected_min_total_cost <- mean(total_costs)
confidence_interval_min_total_cost <- t.test(total_costs)$conf.int

# Print results
cat("Expected Minimum Total Cost:", expected_min_total_cost, "\n")
cat("Confidence Interval (95%) for Minimum Total Cost:", confidence_interval_min_total_cost, "\n\n")

# Probability distribution that best fits its distribution (Uniform Distribution)
# Set seed for reproducibility
set.seed(123)

# Generate 1000 random numbers from a uniform distribution
uniform_data <- runif(1000, min = 1412.719, max = 1420.749)

# Plot histogram of the uniform distribution
hist(uniform_data, breaks = 20, main = "Uniform Distribution", xlab = "Value ($)", ylab = "Frequency")

#ii. Estimate expected order quantity
expected_order_quantity <- sqrt(2 * mean(demand_samples) * ordering_cost / holding_cost_rate)
confidence_interval_order_quantity <- t.test(demand_samples)$conf.int

#Print results
cat("Expected Order Quantity:", expected_order_quantity, "\n")
cat("Confidence Interval (95%) for Order Quantity:", confidence_interval_order_quantity, "\n\n")

# Probability distribution that best fits its distribution (Normal Distribution)
# Generate random numbers from a normal distribution with a different mean and standard deviation
custom_random_numbers <- rnorm(1000, mean = 15000, sd = 2000)

# Plot histogram of the uniform distribution
hist(custom_random_numbers, breaks = 20, main = "Normal Distribution", xlab = "Quantity (Units)", ylab = "Frequency")

#iii. Estimate expected annual number of orders
expected_annual_orders <- sqrt(2 * mean(demand_samples) / holding_cost_rate)
confidence_interval_annual_orders <- t.test(demand_samples)$conf.int

# Print results
cat("Expected Annual Number of Orders:", expected_annual_orders, "\n")
cat("Confidence Interval (95%) for Annual Number of Orders:", confidence_interval_annual_orders, "\n")

# Probability distribution that best fits its distribution (Exponential Distribution)
# Set seed for reproducibility
set.seed(456)

# Calculate the mean of the triangular distribution
mean_triangular <- expected_annual_orders

# Calculate the rate parameter for the exponential distribution
lambda <- 1 / mean_triangular  # Lambda is the reciprocal of the mean

# Generate random values from the exponential distribution
simulated_values_exponential <- rexp(1000, rate = lambda)

# Print the summary statistics
summary(simulated_values_exponential)


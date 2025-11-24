#ALY 6050 - Module 1 Assignment

#Part 1

#Question 2: Construct a probability distribution for your net win (X) in the series. Calculate your 
#expected net win (the mean of X) and the standard deviation of X.

# Given probability of Red Sox winning the series
prob_red_sox_win_series <- 0.648

# Net win values
X_Win <- 500
X_Loss <- -520

# Probability distribution for net win (X) in the series
prob_X_Win <- prob_red_sox_win_series
prob_X_Loss <- 1 - prob_red_sox_win_series

# Expected net win (E(X))
E_X <- X_Win * prob_X_Win + X_Loss * prob_X_Loss

# Variance (Var(X))
Var_X <- (X_Win - E_X)^2 * prob_X_Win + (X_Loss - E_X)^2 * prob_X_Loss

# Standard deviation (SD(X))
SD_X <- sqrt(Var_X)

# Display the results
cat("Probability of Red Sox winning the series:", prob_red_sox_win_series, "\n")
cat("Probability distribution for net win (X):\n")
cat("P(X = 500):", prob_X_Win, "\n")
cat("P(X = -520):", prob_X_Loss, "\n")
cat("Expected net win (E(X)):", E_X, "\n")
cat("Variance (Var(X)):", Var_X, "\n")
cat("Standard deviation (SD(X)):", SD_X, "\n")

#Question 3: Use Excel or R to create 10,000 random values for X. Let these random values be 
#denoted by Y. Use these Y values to estimate your expected net win by using a 95% 
#confidence interval. Does this confidence interval contain E(X)?

# Set seed for reproducibility
set.seed(123)

# Generate 10,000 random values for Y
Y <- rnorm(10000, mean = 140.96, sd = 487.1458)

# Calculate mean and standard deviation of Y
mean_Y <- mean(Y)
sd_Y <- sd(Y)

# Calculate 95% confidence interval for E(X)
confidence_interval <- c(mean_Y - 1.96 * (sd_Y / sqrt(10000)), mean_Y + 1.96 * (sd_Y / sqrt(10000)))

# Print results
cat("Estimated mean of Y:", mean_Y, "\n")
cat("95% Confidence Interval for E(X): [", confidence_interval[1], ",", confidence_interval[2], "]\n")

#Question 4: Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit 
#test to verify how closely the distribution of Y has estimated the distribution of X.

# Set seed for reproducibility
set.seed(123)

# Generate 10,000 random values for Y
Y <- rnorm(10000, mean = 140.96, sd = 487.1458)

# Define bins for the frequency distribution
breaks <- seq(min(Y), max(Y), length.out = 11)

# Create frequency distribution using cut and table functions
frequency_distribution <- cut(Y, breaks = breaks, right = TRUE)
frequency_table <- table(frequency_distribution)

# Perform Chi-squared goodness-of-fit test
expected_counts <- dnorm(breaks, mean = mean(Y), sd = sd(Y) / sqrt(10000)) * 10000

# Create a matrix with observed and expected counts
observed_expected_matrix <- cbind(as.numeric(frequency_table), expected_counts)

#State the hypothesis of the Chi-squared test
#H0: The observed distribution of Y is the same as the expected distribution of X.
#H1: The observed distribution of Y is different from the expected distribution of X.

#Find the Critical Value
# Degrees of freedom
df <- 10

# Significance level
alpha <- 0.05

# Calculate critical value
critical_value <- qchisq(1 - alpha, df)

# Display the result
print(critical_value)
#df = 10, a = 0.05, Critical Value = 18.30704

# Perform Chi-squared test
chi_squared_test <- chisq.test(observed_expected_matrix)

# Print results
cat("Frequency Distribution:\n")
print(frequency_table)

cat("\nChi-squared Test Results:\n")
print(chi_squared_test)

#Part 2

#Question 2: Construct a probability distribution for your net win (X) in the series. Calculate your 
#expected net win (the mean of X) and the standard deviation of X.

# Given probability of Yankees winning the series
prob_yankees_win_series <- 0.604

# Net win values
X_Win <- 500
X_Loss <- -520

# Probability distribution for net win (X) in the series
prob_X_Win <- prob_yankees_win_series
prob_X_Loss <- 1 - prob_yankees_win_series

# Expected net win (E(X))
E_X <- X_Win * prob_X_Win + X_Loss * prob_X_Loss

# Variance (Var(X))
Var_X <- (X_Win - E_X)^2 * prob_X_Win + (X_Loss - E_X)^2 * prob_X_Loss

# Standard deviation (SD(X))
SD_X <- sqrt(Var_X)

# Display the results
cat("Probability of Yankees winning the series:", prob_yankees_win_series, "\n")
cat("Probability distribution for net win (X):\n")
cat("P(X = 500):", prob_X_Win, "\n")
cat("P(X = -520):", prob_X_Loss, "\n")
cat("Expected net win (E(X)):", E_X, "\n")
cat("Variance (Var(X)):", Var_X, "\n")
cat("Standard deviation (SD(X)):", SD_X, "\n")

#Question 3: Use Excel or R to create 10,000 random values for X. Let these random values be 
#denoted by Y. Use these Y values to estimate your expected net win by using a 95% 
#confidence interval. Does this confidence interval contain E(X)?

# Set seed for reproducibility
set.seed(123)

# Generate 10,000 random values for Y
Y <- rnorm(10000, mean = 96.08, sd = 498.8457)

# Calculate mean and standard deviation of Y
mean_Y <- mean(Y)
sd_Y <- sd(Y)

# Calculate 95% confidence interval for E(X)
confidence_interval <- c(mean_Y - 1.96 * (sd_Y / sqrt(10000)), mean_Y + 1.96 * (sd_Y / sqrt(10000)))

# Print results
cat("Estimated mean of Y:", mean_Y, "\n")
cat("95% Confidence Interval for E(X): [", confidence_interval[1], ",", confidence_interval[2], "]\n")

#Question 4: Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit 
#test to verify how closely the distribution of Y has estimated the distribution of X.

# Set seed for reproducibility
set.seed(123)

# Generate 10,000 random values for Y
Y <- rnorm(10000, mean = 96.08, sd = 498.8457)

# Define bins for the frequency distribution
breaks <- seq(min(Y), max(Y), length.out = 11)

# Create frequency distribution using cut and table functions
frequency_distribution <- cut(Y, breaks = breaks, right = TRUE)
frequency_table <- table(frequency_distribution)

# Perform Chi-squared goodness-of-fit test
expected_counts <- dnorm(breaks, mean = mean(Y), sd = sd(Y) / sqrt(10000)) * 10000

# Create a matrix with observed and expected counts
observed_expected_matrix <- cbind(as.numeric(frequency_table), expected_counts)

#State the hypothesis of the Chi-squared test
#H0: The observed distribution of Y is the same as the expected distribution of X.
#H1: The observed distribution of Y is different from the expected distribution of X.

#Find the Critical Value
# Degrees of freedom
df <- 10

# Significance level
alpha <- 0.05

# Calculate critical value
critical_value <- qchisq(1 - alpha, df)

# Display the result
print(critical_value)
#df = 10, a = 0.05, Critical Value = 18.30704

# Perform Chi-squared test
chi_squared_test <- chisq.test(observed_expected_matrix)

# Print results
cat("Frequency Distribution:\n")
print(frequency_table)
cat("\nChi-squared Test Results:\n")
print(chi_squared_test)

#Part 3

#Question 2: Construct a probability distribution for your net win (X) in the series. Calculate your 
#expected net win (the mean of X) and the standard deviation of X.

# Given probability of Red Sox winning the series in five games
prob_red_sox_win_series_five <- 0.682

# Net win values
X_Win <- 500
X_Loss <- -520

# Probability distribution for net win (X) in the series
prob_X_Win <- prob_red_sox_win_series_five
prob_X_Loss <- 1 - prob_red_sox_win_series_five

# Expected net win (E(X))
E_X <- X_Win * prob_X_Win + X_Loss * prob_X_Loss

# Variance (Var(X))
Var_X <- (X_Win - E_X)^2 * prob_X_Win + (X_Loss - E_X)^2 * prob_X_Loss

# Standard deviation (SD(X))
SD_X <- sqrt(Var_X)

# Display the results
cat("Probability of Red Sox winning the series in five:", prob_red_sox_win_series_five, "\n")
cat("Probability distribution for net win (X):\n")
cat("P(X = 500):", prob_X_Win, "\n")
cat("P(X = -520):", prob_X_Loss, "\n")
cat("Expected net win (E(X)):", E_X, "\n")
cat("Variance (Var(X)):", Var_X, "\n")
cat("Standard deviation (SD(X)):", SD_X, "\n")

#Question 3: Use Excel or R to create 10,000 random values for X. Let these random values be 
#denoted by Y. Use these Y values to estimate your expected net win by using a 95% 
#confidence interval. Does this confidence interval contain E(X)?

# Set seed for reproducibility
set.seed(123)

# Generate 10,000 random values for Y
Y <- rnorm(10000, mean = 175.64, sd = 475.0135)

# Calculate mean and standard deviation of Y
mean_Y <- mean(Y)
sd_Y <- sd(Y)

# Calculate 95% confidence interval for E(X)
confidence_interval <- c(mean_Y - 1.96 * (sd_Y / sqrt(10000)), mean_Y + 1.96 * (sd_Y / sqrt(10000)))

# Print results
cat("Estimated mean of Y:", mean_Y, "\n")
cat("95% Confidence Interval for E(X): [", confidence_interval[1], ",", confidence_interval[2], "]\n")

#Question 4: Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit 
#test to verify how closely the distribution of Y has estimated the distribution of X.

# Set seed for reproducibility
set.seed(123)

# Generate 10,000 random values for Y
Y <- rnorm(10000, mean = 175.64, sd = 475.0135)

# Define bins for the frequency distribution
breaks <- seq(min(Y), max(Y), length.out = 11)

# Create frequency distribution using cut and table functions
frequency_distribution <- cut(Y, breaks = breaks, right = TRUE)
frequency_table <- table(frequency_distribution)

# Perform Chi-squared goodness-of-fit test
expected_counts <- dnorm(breaks, mean = mean(Y), sd = sd(Y) / sqrt(10000)) * 10000

# Create a matrix with observed and expected counts
observed_expected_matrix <- cbind(as.numeric(frequency_table), expected_counts)

#State the hypothesis of the Chi-squared test
#H0: The observed distribution of Y is the same as the expected distribution of X.
#H1: The observed distribution of Y is different from the expected distribution of X.

#Find the Critical Value
# Degrees of freedom
df <- 10

# Significance level
alpha <- 0.05

# Calculate critical value
critical_value <- qchisq(1 - alpha, df)

# Display the result
print(critical_value)
#df = 10, a = 0.05, Critical Value = 18.30704

# Perform Chi-squared test
chi_squared_test <- chisq.test(observed_expected_matrix)

# Print results
cat("Frequency Distribution:\n")
print(frequency_table)
cat("\nChi-squared Test Results:\n")
print(chi_squared_test)
